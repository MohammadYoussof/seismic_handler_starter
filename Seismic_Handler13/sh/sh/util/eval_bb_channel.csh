#! /bin/csh
#
# file eval_bb_channel.csh
#      ===================
#
# version 1, 7-Mar-2006
#
# evaluates channels for BB properties.  Evaluates amplitude transfer function
# for two periods
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
set per1=100.0
set per2=5.0
set tmpdir=$HOME/xresp_$$
set chanlist=chanlist.000
set rdseed=rdseed
set evalresp=/home/klaus/evalresp-3.2.32/evalresp

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

$evalresp $upstat '*' $year $doy $frq1 $frq2 100 -f $PWD

set cnt=1
while  (1 > 0)
	set line=`sed -n "$cnt"p $chanlist | sed 's/-/ /g'`
	if  ("$line" == "")  break
	set chan=`$DPROG/stringop upper $line[2]$line[3]`
	if  ($#line >= 5)  then
		set locid=`$DPROG/stringop upper $line[5]`
		set f="AMP.*.$upstat.$locid.$chan"
	else
		set locid=".."
		set f="AMP.*.$upstat.*.$chan"
	endif
	set resp=`ls $f`
	if  ($#resp > 1)  then
		echo "more than one $f"
	else if  ($#resp == 1)  then
		set mid=`wc -l $resp`
		set mid=$mid[1]
		@ mid = $mid / 2
		set amp1=`head -1 $resp | awk '{print $2}'`
		set ampm=`sed -n "$mid"p $resp | awk '{print $2}'`
		set amp2=`tail -1 $resp | awk '{print $2}'`
		set num=`$DPROG/floatop $amp1 div $amp2`
		set num2=`$DPROG/floatop $amp1 div $ampm`
		echo "$station $chan $locid $num $num2"
	endif
	@ cnt ++
end

cd $HOME
\rm -rf $tmpdir
