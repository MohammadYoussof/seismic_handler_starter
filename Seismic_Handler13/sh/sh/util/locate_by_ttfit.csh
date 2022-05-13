#! /bin/csh
#
# file locate_by_ttfit.csh
#      ===================
#
# version 14, 4-Jun-2007
#
# Locate event via travel time fit.
# K. Stammler, 5-Feb-2001

if  ("$1" == "")  then
	echo "Usage: $0 <evtfile> [<lat>] [<lon>] [<width>] [<stepsize>] [<dep1>] [<dep2>] [<ddep>] [<weight>] [<method>]"
	exit
endif

#set echo

# get parameters
set evtfile=$1
set startlat=$2
set startlon=$3
set width=$4
set stepsize=$5
set dep1=$6
set dep2=$7
set ddep=$8
set weight=$9
set method=$10

if  ("$width" == "")  set width=5
if  ("$stepsize" == "")  set stepsize=0.5
if  ("$weight" == "")  set weight=1.0
if  ("$method" == "")  set method=simplex

# set constants
set wlat=$width
set wlon=$width
set dlat=$stepsize
set dlon=$stepsize

set resfile=locate_out.evt
set relfile=ttfit_rel.000
set fitxyz=ttfit_xyz.000
set psfile=ttfit.ps
set textresult=ttfit_edit.txt
set cpt=ttfit.cpt
set citycol=200/0/150
#set fmt=-Jx1.0
set imagsize=5
set fmt=-JX$imagsize/$imagsize
#set fmt=-JM$imagsize

chdir $SH_SCRATCH

# delete previously existing result file
if  (-e $resfile)  \rm $resfile

set lab=1
set res=`$SH_UTIL/floatop $width gt 5.0`
if  ($res == 1)  set lab=2
set res=`$SH_UTIL/floatop $width gt 10.0`
if  ($res == 1)  set lab=5

if  (! -e $evtfile)  then
	echo "evt file $evtfile not found.  Abort."
	exit
endif

# check number of phases
set res=`grep -c '^Phase name' $evtfile`
if  ($res < 4)  then
	echo "$0 : need at least 4 phases.  Abort."
	exit
endif

# find main phase
set mainphase=""
set tmp=`grep -c '^Phase name             : Pn$' $evtfile`
if  ($tmp > 3)  set mainphase=Pn
set tmp=`grep -c '^Phase name             : Pg$' $evtfile`
if  ($tmp > 3)  set mainphase=Pg
set tmp=`grep -c '^Phase name             : PP$' $evtfile`
if  ($tmp > 3)  set mainphase=PP
set tmp=`grep -c '^Phase name             : P$' $evtfile`
if  ($tmp > 3)  set mainphase=P
# check for PKP
if  ("$mainphase" == "")  then
	set tmp=`grep -c '^Phase name             : PKPdf$' $evtfile`
	if  ($tmp > 4)  set mainphase=PKPdf
	set tmp=`grep -c '^Phase name             : PKPab$' $evtfile`
	if  ($tmp > 4)  set mainphase=PKPab
	set tmp=`grep -c '^Phase name             : PKPbc$' $evtfile`
	if  ($tmp > 4)  set mainphase=PKPbc
endif

echo mainphase $mainphase

# create relative onset file
if  (-e $relfile)  \rm $relfile
$SH_UTIL/evt2reltimes $evtfile $relfile $mainphase

# read preliminary location
if  ("$startlat" == "")  then
	set lat=`grep latitude $relfile`
	if  ($#lat < 3)  then
		echo "illegal output of evt2reltimes.  Abort."
		#\rm $relfile
		set lat=0
		exit
	else
		set lat=$lat[3]
	endif
else
	set lat=$startlat
endif
if  ("$startlon" == "")  then
	set lon=`grep longitude $relfile`
	if  ($#lon < 3)  then
		echo "illegal output of evt2reltimes.  Abort."
		#\rm $relfile
		set lon=0
		exit
	else
		set lon=$lon[3]
	endif
else
	set lon=$startlon
endif

# make nice numbers
set lat=`echo $lat | sed 's/\./ /'`
set lat=$lat[1].5
set lon=`echo $lon | sed 's/\./ /'`
set lon=$lon[1].5

#set echo

# call travel time fit
if  (-e $fitxyz)  \rm $fitxyz
if  ("$method" == "grid")  then
	$SH_UTIL/fit_travel -infoline -xyz -clat=$lat -clon=$lon -wlat=$wlat \
		-wlon=$wlon -dlat=$dlat -dlon=$dlon -dep1=$dep1 -dep2=$dep2 \
		-ddep=$ddep $relfile $weight >$fitxyz
else
	if  ("$dep1" == "$dep2")  then
		set fixdepth="-fixdepth"
	else
		set fixdepth=""
	endif
	set xlat=`echo $lat | sed 's/-/s/'`
	set xlon=`echo $lon | sed 's/-/w/'`
	$SH_UTIL/fit_travel2 -trace $fixdepth -ftol=0.001 $relfile $xlat $xlon \
		$dep1 $weight >$fitxyz
endif
#\rm $relfile

set infoline=`tail -1 $fitxyz`
if  ($#infoline < 11)  then
	echo "infoline of $fitxyz has unknown syntax:"
	echo "$infoline"
	exit
endif
set minz=$infoline[3]
set minlat=$infoline[5]
set minlon=$infoline[7]
set mindep=$infoline[9]
set origtime=$infoline[11]
if  ("$method" == "grid")  then
	set maxz=`grep -v '^\!' $fitxyz | grep " $mindep depth" | awk '{print $3}' | sort -n | tail -1`
else
	set maxz=`grep -v '^\!' $fitxyz | awk '{print $3}' | sort -n | tail -1`
endif

# check for longitude larger than 180
set xtmp=`echo $minlon | sed 's/\./ /'`
set xtmp=$xtmp[1]
if  ($xtmp >= 180)  then
	set minlon=`echo "scale=3; $minlon - 360.0" | bc`
endif

# compute bounds of display
set wlat2=`echo "scale=3; $wlat / 2" | bc`
set wlon2=`echo "scale=3; $wlon / 2" | bc`
set dlat2=`echo "scale=3; $dlat / 2" | bc`
set dlon2=`echo "scale=3; $dlon / 2" | bc`
set lolat=`echo "scale=3; $minlat - $wlat2" | bc`
set hilat=`echo "scale=3; $minlat + $wlat2" | bc`
set lolon=`echo "scale=3; $minlon - $wlon2" | bc`
set hilon=`echo "scale=3; $minlon + $wlon2" | bc`

set lolat=`echo "scale=3; $lolat - $dlat2" | bc`
set hilat=`echo "scale=3; $hilat + $dlat2" | bc`
set lolon=`echo "scale=3; $lolon - $dlon2" | bc`
set hilon=`echo "scale=3; $hilon + $dlon2" | bc`

# create text output
if  (-e $textresult)  \rm $textresult
touch $textresult
echo "Best fit for"                   >>$textresult
echo "$origtime"                      >>$textresult
echo "lat=$minlat lon=$minlon"        >>$textresult
if  ("$dep1" != "$dep2")  then
	echo "Found depth $mindep km"      >>$textresult
else
	echo "Depth given as $mindep km"   >>$textresult
endif
echo "Min-rms=$minz (orange)"         >>$textresult
echo "Max-rms=$maxz (blue)"           >>$textresult
echo ""                               >>$textresult
echo "Found residuals:"               >>$textresult
echo ""                               >>$textresult
grep '^\!' $fitxyz | grep -v 'minsq:' >>$textresult
# $SH_TEXTEDIT $textresult &

# create postscript output
if  (-e $psfile)  \rm $psfile

# create cpt file, make the best 3% turn orange
set levz=`echo "scale=5; $maxz - $minz" | bc `
set levz=`echo "scale=5; $levz * 0.03" | bc `
set levz=`echo "scale=5; $levz + $minz" | bc `
if  (-e $cpt)  \rm $cpt
touch $cpt
echo "$minz 255 150 0    $levz 255 255 0"  >>$cpt
echo "$levz 255 255 0  $maxz 0 0 255"  >>$cpt

# compute symbol size
set symsiz=`echo "scale=5; $stepsize / $width * $imagsize" | bc`

set xlat=`echo $minlat | sed 's/-/s/'`
set xlon=`echo $minlon | sed 's/-/w/'`
set areaname="`$SH_UTIL/fereg $xlat $xlon`"

touch $psfile

if  ("$method" == "grid")  then
	grep -v '^\!' $fitxyz | grep " $mindep depth" | \
		psxy -: -R$lolon/$hilon/$lolat/$hilat \
		$fmt -Ss$symsiz -C$cpt -U \
		-B${lab}:longitude:/${lab}:latitude::."$areaname":8 \
		-K >>$psfile
else
	grep -v '^\!' $fitxyz | \
		psxy -: -R$lolon/$hilon/$lolat/$hilat \
		$fmt -Ss$symsiz -C$cpt -U \
		-B${lab}:longitude:/${lab}:latitude::."$areaname":8 \
		-K >>$psfile
endif

# recompute longitude from negative values to 180..360
set xhilon=`echo $hilon | sed 's/-/m/'`
set res=`$SH_UTIL/floatop $xhilon lt 0.0`
if ($res == 1)  set xhilon=`echo "scale=3; $hilon + 360.0" | bc`
set xlolon=`echo $lolon | sed 's/-/m/'`
set res=`$SH_UTIL/floatop $xlolon lt 0.0`
if ($res == 1)  set xlolon=`echo "scale=3; $lolon + 360.0" | bc`

# plot coastlines and borders
pscoast -O -R$xlolon/$xhilon/$lolat/$hilat $fmt -A10 \
	-Df -N1/4/0/0/0/dashed -W1/0/0/0/solid -Ia/0.1/100 -K >>$psfile

# plot cities
awk '{print $1,$2}' $SH_UTIL/cities.dat \
	| psxy -: -O -K -R$lolon/$hilon/$lolat/$hilat $fmt \
	-W5/$citycol -G$citycol -Sc0.1 >>$psfile
awk '{print $1,$2,"14","0","4","1","\ ",$3}' $SH_UTIL/cities.dat \
	| pstext  -: -O -K -R$lolon/$hilon/$lolat/$hilat $fmt -G$citycol >>$psfile

# integrate text into psfile
set lcnt=`wc -l $textresult`
set lcnt=$lcnt[1]
set ypos=0
set cont=-K
while  ($lcnt > 0)
	set line="`sed -n $lcnt'p' $textresult`"
	if  ("$line" == "")  set line="\ "
	if  ($lcnt == 1)  set cont=""
	echo "6.0 $ypos.0 12 0.0 8 1  $line" | \
		pstext -O $cont -JX10/8 -R0/10/0/100 >>$psfile
	@ lcnt = $lcnt - 1
	@ ypos = $ypos + 2
end

touch $resfile

echo "Latitude               : $minlat"               >>$resfile
echo "Longitude              : $minlon"               >>$resfile
echo "Depth (km)             : $mindep"               >>$resfile
echo "Origin time            : $origtime"             >>$resfile
echo "Location method        : relative travel times" >>$resfile
echo "--- End of Phase ---"                           >>$resfile

if  ($?PSVIEW == 0)  setenv PSVIEW pageview

# pop up window
#$PSVIEW -right $PWD/$psfile & # this dies after the shell terminates (on Sun)

set xtmp=`whoami | cut -c1-6`

if  ("$xtmp" == "tmpusr")  then

	$PSVIEW $PWD/$psfile &

else

	set pos=""
	if  ("`hostname`" == "ersn24")  set pos="-geometry 940x780+0+0"

	rsh -n `hostname` "$PSVIEW -right -display $DISPLAY $pos $PWD/$psfile" &

endif

#\rm $fitxyz


