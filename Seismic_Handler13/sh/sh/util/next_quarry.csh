#! /bin/csh
#
# file next_quarry.csh
#      ===============
#
# version 1, 9-Apr-2003
#
# Shows distance to next quarry location
# K. Stammler, 9-Apr-2003

if  ("$2" == "")  then
	echo "Usage: $0 <lat> <lon> [<quarryfile>]"
	exit
endif

# get parameters
set epilat=`echo $1 | sed 's/^-/s/'`
set epilon=`echo $2 | sed 's/^-/w/'`
set qfile=$3

# set constants
set tmpfile=$HOME/nexqu_$$.000

if  ("$qfile" == "")  set qfile=$SH_INPUTS/german_quarries.dat

if  (-e $tmpfile)  \rm $tmpfile
touch $tmpfile
set cnt=0
while  (1 > 0)  # forever
	@ cnt = $cnt + 1
	set qline=`sed -n $cnt"p" $qfile`
	if  ("$qline" == "")  break
	if  ($#qline < 2)  continue
	set qlat=`echo $qline[1] | sed 's/^-/s/'`
	set qlon=`echo $qline[2] | sed 's/^-/w/'`
	set res=`$SH_UTIL/locdiff $epilat $epilon $qlat $qlon`
	if  ($#res < 2)  continue
	set dist=$res[2]
	echo "$dist $qlat $qlon" >>$tmpfile
	echo "$dist $qlat $qlon"
end

echo ""
echo "next five quarries:"

sort -n -k 1 $tmpfile | head -5

\rm $tmpfile
