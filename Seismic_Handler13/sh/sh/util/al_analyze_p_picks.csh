#! /bin/csh
#
# file analyze_p_picks.csh
#      ===================
#
# version 3, 29-Mar-2001
#
# Checks relonset file for futher processing.  The <relonset> file must be
# a file computed with correlation mode 1.  This is used for checking the
# pick quality.  If an alternative relonset-file <relonset>:r_ALT.STX exists,
# this is used for location, after the checking procedures have been
# passed.
#
# Creates files:
# <relonset>:r_CORR.STX  : corrected P-picks (resid. corr. and without mispicks)
# p-locate.txt           : Output of fit_travel2 on (corrected) P-picks
# P_SLOWNESS.STX         : measured P slowness
# S_ARRIVAL.STX          : theoretical arrival time of S at ref. station
# S_SLOWNESS.STX         : theoretical S slowness
# S_BAZIMUTH.STX         : measured P azimuth (should be the same as S)
# SCS_SLOWNESS.STX       : theoretical ScS slowness
#
# K. Stammler, 24-Mar-2001

if  ("$2" == "")  then
	echo "Usage: $0 <relonset> <refstation>"
	exit
endif

# get parameters
set relonset=$1
set defaultstation=`$DPROG/stringop upper $2`

#set echo

# event selection criteria
# minimum mean correlation coefficient (statistics on corrmode 1 file)
set minmeancc=0.5
# minimum mean signal/noise ratio (statistics on corrmode 1 file)
set minmeansnr=2.5
# minimum snr deviation (statistics on corrmode 1 file)
set minsnrdev=1.0
# maximum standard deviation for residuals (statistics on corrmode 1 file)
set maxresiddev=0.8
# maximum residual for pick (picks with larger resdiuals are removed)
set maxresid=1.5
# minimum number of remaining picks (if less, no location is made)
set minpick=7

# set constants
set defaultdepth=33.0
set f_tmppick=tmppick_$$.000
set f_corr_resid=$relonset:r
set f_corr_resid=${f_corr_resid}_CORR.STX
set f_presults=p-locate.txt

# check files
if  (! -e $relonset)  then
	echo "P: _E_ file $relonset not found.  Abort."
	exit
endif

# check for alternative relonset file
set relonset_alt=$relonset:r
set relonset_alt=${relonset_alt}_ALT.STX
# if it is not found use the primary file
if  (! -e $relonset_alt)  set relonset_alt=$relonset

# check statistics on corrmode 1 file
echo "P: _I_ checking pick quality"
set res=`$SH_UTIL/al_pick_statistics.csh fset $relonset`
if  ($#res < 4)  then
	echo "P: _E_ illegal output of al_pick_statistics.csh on $relonset"
	exit
endif
set tmp=`$DPROG/floatop $res[1] gt $minmeancc`
if  ($tmp == 0)  then
	echo "P: _E_ mean corrcoeff $res[1] too small in $relonset"
	exit
endif
set tmp=`$DPROG/floatop $res[3] gt $minmeansnr`
if  ($tmp == 0)  then
	echo "P: _E_ mean signoise $res[3] too small in $relonset"
	exit
endif
set tmp=`$DPROG/floatop $res[4] gt $minsnrdev`
if  ($tmp == 0)  then
	echo "P: _E_ signoise stddev $res[4] too small in $relonset"
	exit
endif
set tmp=`$DPROG/floatop $res[6] lt $maxresiddev`
if  ($tmp == 0)  then
	echo "P: _E_ residual stddev $res[3] too large in $relonset"
	exit
endif

# remove picks with large residuals
set stlist=`$SH_UTIL/relonset2slo -resid $relonset_alt | grep '^[A-Z]' | $DPROG/colrange 2 m$maxresid $maxresid | awk '{print $1}'`
if (-e $f_tmppick)  \rm $f_tmppick
touch $f_tmppick
foreach s ($stlist)
	grep ^$s $relonset_alt >>$f_tmppick
end
grep '^\! reftime' $relonset_alt >>$f_tmppick

set tmp=`wc -l $f_tmppick`
set tmp=$tmp[1]
@ tmp = $tmp - 1
if  ($tmp < $minpick)  then
	echo "P: _E_ file $relonset_alt ignored: too less picks ($tmp) remaining"
	\rm $f_tmppick
	exit
endif

# perform corrections to relonset-file
set res=`$SH_UTIL/relonset2slo $f_tmppick`
if  ($#res < 4)  then
	echo "P: _E_ illegal beam result for pickfile.  Abort."
	exit
endif
set beam_slo=$res[1]
set beam_baz=$res[2]
if  (-e $f_corr_resid)  \rm $f_corr_resid
$SH_UTIL/al_apply_resid_corr.csh $f_tmppick $f_corr_resid P $beam_slo $beam_baz
\rm $f_tmppick

# compute corrected slowness and azimuth
set res=`$SH_UTIL/relonset2slo $f_corr_resid`
if  ($#res < 4)  then
	echo "P: _E_ illegal beam result for pickfile.  Abort."
	exit
endif
set slo=$res[1]
set baz=$res[2]
set dist=`$SH_UTIL/slo2distance P $slo $defaultdepth`
if  ("$dist" == "0.00")  then
	echo "P: _E_ No location possible with P-slowness $slo.  Abort."
	exit
endif
set res=`$SH_UTIL/locadd $defaultstation $dist $baz | grep deg`
if  ($#res < 6)  then
	echo "P: _E_ Illegal output of locadd program.  Abort."
	exit
endif
set prelat=$res[2]
set prelon=$res[5]
set xprelat=`echo $prelat | sed 's/^-/s/'`
set xprelon=`echo $prelon | sed 's/^-/w/'`

# location with corrected file
echo "P: _I_ locating with P picks"
if  (-e $f_presults)  \rm $f_presults
$SH_UTIL/fit_travel2 -fixdepth -ftol=0.001 $f_corr_resid \
	$xprelat $xprelon $defaultdepth >$f_presults
set res=`tail -1 $f_presults`

# read results
if  ($#res < 11)  then
	echo "P: _E_ failure in fit_travel2 on $relonset_alt"
	exit
endif
set p_rms=$res[3]
set p_lat=$res[5]
set p_lon=$res[7]
set p_orig=$res[11]

# compute distance and baz for location and defaultstation
set p_xlat=`echo $p_lat | sed 's/^-/s/'`
set p_xlon=`echo $p_lon | sed 's/^-/w/'`
set res=`$SH_UTIL/locdiff $p_xlat $p_xlon $defaultstation | grep distance`
if  ($#res < 3)  then
	echo "P: _E_ Illegal output of locdiff.  Abort."
	exit
endif
set dist=$res[2]
set res=`$SH_UTIL/locdiff $p_xlat $p_xlon $defaultstation | grep backazimuth`
set baz=$res[2]

# compute theoretical S arrival and S slowness
set s_travel=`$SH_UTIL/traveltime S $dist $defaultdepth`
if  ("$s_travel" == "0.00")  then
	echo "P: _E_ Cannot compute S travel time for distance $dist, depth $defaultdepth."
	exit
endif
set s_arrival=`$SH_UTIL/timename time_addsec $p_orig $s_travel`
set s_slow=`$SH_UTIL/traveltime -s S $dist $defaultdepth`
set scs_slow=`$SH_UTIL/traveltime -s ScS $dist $defaultdepth`

# write out results to files
if  (-e S_ARRIVAL.STX)  \rm S_ARRIVAL.STX
echo $s_arrival >S_ARRIVAL.STX
if  (-e S_SLOWNESS.STX)  \rm S_SLOWNESS.STX
echo $s_slow >S_SLOWNESS.STX
if  (-e SCS_SLOWNESS.STX)  \rm SCS_SLOWNESS.STX
echo $scs_slow >SCS_SLOWNESS.STX
if  (-e S_BAZIMUTH.STX)  \rm S_BAZIMUTH.STX
echo $beam_baz >S_BAZIMUTH.STX
if  (-e P_SLOWNESS.STX)  \rm P_SLOWNESS.STX
echo $beam_slo >P_SLOWNESS.STX
