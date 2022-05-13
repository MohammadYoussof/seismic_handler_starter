#! /bin/csh
#
# file analyse_s_picks.csh
#      ===================
#
# version 2, 28-Mar-2001
#
# analyze S picks
# K. Stammler, 26-Mar-2001

if  ("$3" == "")  then
	echo "Usage: $0 <raw-p-picks> <corr-p-picks> <s-picks>"
	exit
endif

#set echo

# get parameters
set rppicks=$1
set cppicks=$2
set spicks=$3

# set constants
set maxssloerr=0.8
set maxsbazerr=4.0
set maxresid=3.5
set sweight=0.2
set traveltol=20.0
set corrfile=S_PICKS_CORR.STX
set pcorrfile=P_PICKS_CORR.STX
set locfile=locphases.txt
set plocate="p-locate.txt"
set pslocate="ps-locate.txt"
set psdlocate="psd-locate.txt"
set sptime="S_P_TIME.STX"
set sstation="S_STATION.STX"

if  (! -e $rppicks)  then
	echo "S: _E_ Input file $rppicks not found.  Abort."
	exit
endif
if  (! -e $cppicks)  then
	echo "S: _E_ Input file $cppicks not found.  Abort."
	exit
endif
if  (! -e $spicks)  then
	echo "S: _E_ Input file $spicks not found.  Abort."
	exit
endif

# check number of S-picks
set pcnt=`grep '^[A-Z]' $spicks | wc -l`
set pcnt=$pcnt[1]
if  ($pcnt < 4)  then
	echo "S: _E_ only $pcnt S-picks found.  S not used."
	exit
endif

set res=`$SH_UTIL/relonset2slo $spicks`
if  ($#res < 4)  then
	echo "S: _E_ Illegal output of relonset2slo (S).  Abort."
	exit
endif
set sslo=$res[1]
set sbaz=$res[2]
set ssloerr=$res[3]
set sbazerr=$res[4]

# check for large errors
set d=`$DPROG/floatop $ssloerr gt $maxssloerr`
if  ($d == 1)  then
	echo "S: _E_ S slowness error $ssloerr > $maxssloerr.  S not used."
	exit
endif
set d=`$DPROG/floatop $sbazerr gt $maxsbazerr`
if  ($d == 1)  then
	echo "S: _E_ S azimuth error $sbazerr > $maxsbazerr.  S not used."
	exit
endif

# check for deviations of P-azimuth and predicted slowness
set res=`$SH_UTIL/relonset2slo $cppicks`
if  ($#res < 4)  then
	echo "S: _E_ Illegal output of relonset2slo (P).  Abort."
	exit
endif
set pslo=$res[1]
set pbaz=$res[2]
set theo_sslo=`cat S_SLOWNESS.STX`
set d=`$DPROG/floatop $sslo minus $theo_sslo`
set d=`echo $d | sed 's/^-//'`
set d=`$DPROG/floatop $d gt $maxssloerr`
if  ($d == 1)  then
	echo "S: _E_ S slowness $sslo deviates from predicted $theo_sslo.  S not used."
	exit
endif
set d=`$DPROG/floatop $sbaz minus $pbaz`
set d=`echo $d | sed 's/^-//'`
set tmp=`$DPROG/floatop $d gt 350.0`
if  ($tmp == 1)  set d=`$DPROG/floatop $d minus 360.0`
set d=`echo $d | sed 's/^-//'`
set d=`$DPROG/floatop $d gt $maxsbazerr`
if  ($d == 1)  then
	echo "S: _E_ S azimuth $sbaz deviates from P $pbaz.  S not used."
	exit
endif

# remove picks with residuals more than $maxresid sec
set slist=`$SH_UTIL/relonset2slo -resid=$theo_sslo,$pbaz $spicks | grep '^[A-Z]' | sed 's/^-//' | $DPROG/colrange 2 $maxresid none | awk '{print $1}'`
if  (-e $corrfile)  \rm $corrfile
if  (-e $corrfile.x)  \rm $corrfile.x
cp $spicks $corrfile.x
foreach s ($slist)
	grep -v ^$s $corrfile.x >$corrfile
	\rm $corrfile.x
	mv $corrfile $corrfile.x
end
mv $corrfile.x $corrfile

# read P location from P location file
set res=`tail -1 $plocate`
if  ($#res < 9)  then
	echo "S: _E_ cannot find P location.  Abort."
	exit
endif
set plat=`echo $res[5] | sed 's/^-/s/'`
set plon=`echo $res[7] | sed 's/^-/w/'`
set pdep=`echo $res[9] | sed 's/^-/m/'`

# compute theoretical S-P and ScS-P times for reference station
set refstat=`cat $sstation`
set pdist=`$SH_UTIL/locdiff $plat $plon $refstat | grep distance`
if  ($#pdist < 3)  then
	echo "S: _E_ cannot compute distance to refstation."
	exit
endif
set pdist=$pdist[2]
set res=`$SH_UTIL/traveltime P $pdist $pdep`
set thsrel=`$SH_UTIL/traveltime S $pdist $pdep`
if  ("$thsrel" == "0.00")  then
	set thsrel=""
else
	set thsrel=`$DPROG/floatop $thsrel minus $res`
endif
set thscsrel=`$SH_UTIL/traveltime ScS $pdist $pdep`
if  ("$thscsrel" == "0.00")  then
	set thscsrel=""
else
	set thscsrel=`$DPROG/floatop $thscsrel minus $res`
endif

# get beam picks for S-P and ScS-P
if  (! -e $sptime)  then
	echo "S: _E_ no $sptime."
	exit
endif
set res=`cat $sptime`
if  ($#res < 1)  then
	echo "S: _E_ empty $sptime"
	exit
endif
set beamsrel=$res[1]
if  ($#res > 1)  then
	set beamscsrel=$res[2]
	if  ("$beamscsrel" == "POS" || "$beamscsrel" == "NEG")  set beamscsrel=""
else
	set beamscsrel=""
endif

# check deviations from theoretical value for S-P and ScS-P
set res=`$DPROG/floatop $thsrel minus $beamsrel | sed 's/^-//'`
set res=`$DPROG/floatop $res gt $traveltol`
if  ($res == 1)  set beamsrel=""
set res=`$DPROG/floatop $thscsrel minus $beamscsrel | sed 's/^-//'`
set res=`$DPROG/floatop $res gt $traveltol`
if  ($res == 1)  set beamscsrel=""

# create file with S and P picks for fit_travel
# first read absolute times in P and S files
set res=`grep reftime $rppicks`
if  ($#res < 3)  then
	echo "S: _E_ cannot read reference time in $rppicks.  Abort."
	exit
endif
set preftime=$res[3]
set res=`grep reftime $spicks`
if  ($#res < 3)  then
	echo "S: _E_ cannot read reference time in $spicks.  Abort."
	exit
endif
set sreftime=$res[3]

# compute time shift for S-P to adjust the reference station
if  (-e $sstation)  then
	set station=`cat $sstation`
	if  ("$station" == "")  then
		echo "S: _E_ illegal $sstation."
		exit
	endif
	# S entry
	set res=`grep ^$station $corrfile`
	if  ($#res < 2)  then
		echo "S: _E_ illegal $corrfile."
		exit
	endif
	set srel=`echo $res[2] | sed 's/^-/m/'`
	# just check whether reference station is in the corrected file ...
	set res=`grep ^$station $cppicks`
	if  ($#res < 2)  then
		echo "S: _E_ no P pick found for reference $station."
		exit
	endif
	# ... but take the uncorrected P pick (it must be there)
	set res=`grep ^$station $rppicks`
	set prel_u=`echo $res[2] | sed 's/^-/m/'`
	set psrefdiff=`$SH_UTIL/timename time_floatdiff $sreftime $preftime | sed 's/^-/m/'`
	set d=`$DPROG/floatop $psrefdiff + $srel | sed 's/^-/m/'`
	set d=`$DPROG/floatop $d minus $prel_u | sed 's/^-/m/'`
	if  (! -e $sptime)  then
		echo "S: _E_ no $sptime."
		exit
	endif
	set adjusttime=`cat $sptime | sed 's/^-/m/'`
	if  ("$adjusttime" == "")  then
		echo "S: _E_ empty $sptime"
		exit
	endif
	set adjusttime=$adjusttime[1]
	set psshift=`$DPROG/floatop $adjusttime minus $d | sed 's/^-/m/'`
endif

# loop all entries in P file and compute S-P if available
if  (-e $locfile)  \rm $locfile
touch $locfile
set scnt=1  # counter for remaining S-picks
set cnt=1
while  (1 > 0)  # forever

	# read next line, check for comments and EOF, parse line
	# use the corrected P file just to see whether the station is listed there
	set line=`sed -n $cnt"p" $cppicks`
	@ cnt = $cnt + 1
	if  ("$line[1]" == "\!")  break
	if  ($#line < 3)  break
	set station=$line[1]
	set prel=$line[2]
	# read the uncorrected P pick for S-P time
	set res=`grep ^$station $rppicks`
	if  ($#res < 2)  then
		echo "S: _E_ inconsistent program. No uncorrected P pick."
		exit
	endif
	set prel_u=$res[2]

	# take P entry unchanged, here use corrected P pick
	echo "$station $prel P" >>$locfile

	# look for corresponding S entry
	set res=`grep ^$station $corrfile`
	if  ($#res < 2)  continue
	set srel=$res[2]

	# compute absolute P time
	set sign=`echo $prel_u | grep -`
	set prel_u=`echo $prel_u | sed 's/^-//'`
	if  ("$sign" == "")  then
		set pabs=`$SH_UTIL/timename time_addsec $preftime $prel_u`
	else
		set pabs=`$SH_UTIL/timename time_subsec $preftime $prel_u`
	endif

	# compute absolute S time
	set sign=`echo $srel | grep -`
	set srel=`echo $srel | sed 's/^-//'`
	if  ("$sign" == "")  then
		set sabs=`$SH_UTIL/timename time_addsec $sreftime $srel`
	else
		set sabs=`$SH_UTIL/timename time_subsec $sreftime $srel`
	endif

	# compute and write S-P
	set d=`$SH_UTIL/timename time_floatdiff $sabs $pabs | sed 's/^-/m/'`
	# set d=`$DPROG/floatop $d + $psshift`
	echo "$station $d S-P"  >>$locfile
	@ scnt = $scnt + 1

end

# write ScS-P for refstation
if  ("$beamscsrel" != "")  then
	echo "$refstat $beamscsrel ScS-P"  >>$locfile
endif

echo "! reftime $preftime" >>$locfile

if  ($scnt > 2)  then
	set fixdepth=""
	echo "S: _I_ locating with P+S picks (depth free)"
else
	set fixdepth="-fixdepth"
	echo "S: _I_ locating with P+S picks (depth fixed at $pdep)"
endif
if  (-e $pslocate)  \rm $pslocate
$SH_UTIL/fit_travel2 -ftol=0.001 $fixdepth $locfile \
	$plat $plon $pdep $sweight >$pslocate

# make another location using P only, but take the depth from P+S location
if  ($scnt > 2)  then
	set res=`tail -1 $pslocate`
	if  ($#res < 9)  then
		echo "S: _E_ illegal output of fit_travel2"
		exit
	endif
	set pslat=`echo $res[5] | sed 's/^-/s/'`
	set pslon=`echo $res[7] | sed 's/^-/w/'`
	set psdep=`echo $res[9] | sed 's/^-//'`
	echo "S: _I_ locating with P picks, depth $psdep (taken from P+S)"
	if  (-e $psdlocate)  \rm $psdlocate
	$SH_UTIL/fit_travel2 -fixdepth -ftol=0.001 $pcorrfile \
		$pslat $pslon $psdep >$psdlocate
endif
