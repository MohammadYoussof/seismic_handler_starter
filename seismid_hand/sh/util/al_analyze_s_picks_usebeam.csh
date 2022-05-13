#! /bin/csh
#
# file analyse_s_picks.csh
#      ===================
#
# version 1, 26-Mar-2001
#
# analyze S picks
# K. Stammler, 26-Mar-2001

if  ("$3" == "")  then
	echo "Usage: $0 <raw-p-picks> <corr-p-picks> <s-picks>"
	exit
endif

# get parameters
set rppicks=$1
set cppicks=$2
set spicks=$3

# set constants
set maxssloerr=0.8
set maxsbazerr=4.0
set maxresid=3.5
set corrfile=S_PICKS_CORR.STX
set locfile=locphases.txt
set plocate="p-locate.txt"
set pslocate="ps-locate.txt"
set sptime="S_P_TIME.STX"

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

# create file with S and P picks for fit_travel
# first read absolute times in P and S files
set res=`grep reftime $cppicks`
if  ($#res < 3)  then
	echo "S: _E_ cannot read reference time in $cppicks.  Abort."
	exit
endif
set preftime=$res[3]
set res=`grep reftime $spicks`
if  ($#res < 3)  then
	echo "S: _E_ cannot read reference time in $spicks.  Abort."
	exit
endif
set sreftime=$res[3]

# loop all entries in P file and compute S-P if available
if  (-e $locfile)  \rm $locfile
touch $locfile
set scnt=1  # counter for remaining S-picks
set cnt=1
while  (1 > 0)  # forever

	# read next line, check for comments and EOF, parse line
	set line=`sed -n $cnt"p" $cppicks`
	@ cnt = $cnt + 1
	if  ("$line[1]" == "\!")  break
	if  ($#line < 3)  break
	set station=$line[1]
	set prel=$line[2]

	# take P entry unchanged
	echo "$station $prel P" >>$locfile

	@ scnt = $scnt + 1

end

# search for S-P
set res=""
if  (-e $sptime)  set res=`cat $sptime`
if  ($#res > 0)  then
	echo "BEAM $res[1] S-P" >>$locfile
endif

echo "! reftime $preftime" >>$locfile

# read P location from P location file
set res=`tail -1 $plocate`
if  ($#res < 9)  then
	echo "S: _E_ cannot find P location.  Abort."
	exit
endif
set plat=`echo $res[5] | sed 's/^-/m/'`
set plon=`echo $res[7] | sed 's/^-/m/'`
set pdep=`echo $res[9] | sed 's/^-/m/'`

#if  ($scnt > 2)  then
#	set fixdepth=""
#	echo "S: _I_ locating with P+S picks (depth free)"
#else
	set fixdepth="-fixdepth"
	echo "S: _I_ locating with P+S picks (depth fixed at $pdep)"
#endif
if  (-e $pslocate)  \rm $pslocate
$SH_UTIL/fit_travel2 -ftol=0.001 $fixdepth $locfile $plat $plon $pdep >$pslocate
