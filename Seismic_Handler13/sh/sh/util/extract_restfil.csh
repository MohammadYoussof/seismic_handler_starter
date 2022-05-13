#! /bin/csh
#
# file extract_restfil.csh
#      ===================
#
# version 1, 7-Mar-2006
#
# Extract restitution filters from SEED file.
# K. Stammler, 7-Mar-2006

if  ("$4" == "")  then
	echo "Usage: $0 <seedfile> <station> <year> <doy>"
	exit
endif

# get parameters
set seedfile=$1
set station=$2
set year=$3
set doy=$4

if  (! -e $seedfile)  then
	echo "$0 : input file $seedfile not found.  Abort."
	exit
endif

# set constants
set per1=1000.0
set per2=0.05
set tmpdir=$HOME/xresp
set tmpfile=fil_$$.000
set chanlist=chanlist.000
set rdseed=rdseed4.6
set evalresp=evalresp

set res=`which $rdseed`
if  ("`echo $res | grep 'not found'`" != "")  then
	echo "$0 : no program rdseed ($rdseed) found.  Abort."
	exit
endif
set res=`which $evalresp`
if  ("`echo $res | grep 'not found'`" != "")  then
	echo "$0 : no program evalresp ($evalresp) found.  Abort."
	exit
endif

if  (-e $tmpdir)  \rm -rf $tmpdir
mkdir $tmpdir
cd $tmpdir

if  (-e $chanlist)  \rm $chanlist
$SEED_PROG/seedquickdump -locid $seedfile | awk '{print $4,$7,$10}' \
	| grep -i "^$station-" | sort -u | awk '{print $1,1.0/$2,$3}' >$chanlist

set frq1=`echo "scale=3; 1.0 / $per1" | bc`
set frq2=`echo "scale=3; 1.0 / $per2" | bc`
echo "frq range $frq1 $frq2"

set upstat=`$DPROG/stringop upper $station`

# get response files of station
$rdseed <<END >/dev/null
$seedfile


R
$station



quit
END

$evalresp $upstat '*' $year $doy $frq1 $frq2 1000 -f $PWD -u dis

#set echo
set cnt=1
while  (1 > 0)
	set line=`sed -n "$cnt"p $chanlist | sed 's/-/ /g'`
	if  ("$line" == "")  break
	set chan=`$DPROG/stringop upper $line[2]$line[3]`
	set nyq=`$DPROG/floatop $line[4] div 2.0`
	if  ($#line >= 5)  then
		set locid=`$DPROG/stringop upper $line[5]`
		set f="*.$upstat.$locid.$chan"
	else
		set locid="__"
		set f="*.$upstat.*.$chan"
	endif
	set resp=`ls AMP.$f`
	set phase=`ls PHASE.$f`
	if  ($#resp > 1)  then
		echo "more than one $f"
	else if  ($#resp == 1 && $#phase == 1)  then
		set rnum=`wc -l $resp`
		set rnum=$rnum[1]
		set pnum=`wc -l $phase`
		set pnum=$pnum[1]
		if  ($rnum != $pnum)  then
			echo "different number of lines in AMP and PHASE file ($resp)"
		else
			set fname=`$DPROG/stringop upper ${station}:${chan}:$locid.FLT`
			# remove all filter entries above 92% of the nyquist frequency
			# otherwise the FIR filter stages create very large numbers
			# in the inversion filter towards the Nyquist frequencies
			set maxfrq=`$DPROG/floatop $nyq mul 0.92`
			if  (-e $tmpfile)  \rm $tmpfile
			paste $resp $phase | awk '{print $1,1.0e9/$2,-1.0*$4}' \
				| $DPROG/colrange 1 0 $maxfrq >$tmpfile
			set linenum=`wc -l $tmpfile`
			set linenum=$linenum[1]
			if  (-e $fname)  \rm $fname
			# two more line for frequencies 0 and $nyq
			@ linenum = $linenum + 2
			touch $fname
			echo "1357913578" >>$fname
			echo "2"          >>$fname
			echo "$linenum"   >>$fname
			# the first line with zeroes is needed to prevent out of range errors
			# But the filter function for periods larger than $per1 are wrong!
			echo "0.0 0.0 0.0">>$fname
			cat $tmpfile      >>$fname
			\rm $tmpfile
			echo "$nyq 0.0 0.0">>$fname
		endif
	endif
	@ cnt ++
end

cd $HOME
#\rm -rf $tmpdir
