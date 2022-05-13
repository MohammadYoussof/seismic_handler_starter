#! /bin/csh
#
# file remove_button_set.csh
#      =====================
#
# version 1, 15-Nov-2003
#
# remove complete button set
# K. Stammler, 15-Nov-2003

set xprog=$SH_ROOT/setup

set maxset=`$xprog/noof_button_sets.csh`
if  ($maxset <= 1)  then
	echo "At least one button set must remain."
	exit
endif

echo -n "Enter number of button set (between 1 and $maxset): "
set setnum="$<"

if  ("$setnum" == "" || "$setnum" == "q")  then
	echo "Operation aborted."
	exit
endif
if  ($setnum > $maxset)  then
	echo "Maximum number of button set is $maxset.  Input not accepted."
	exit
endif

echo ""
echo "removing button set number $setnum"
echo ""

cd $SH_GLOBALS

if  ($setnum == $maxset)  then
	set setnum=`printf "%02d" $setnum`
	\rm STATLIST_$setnum.STX
	exit
endif

@ nextnum = $setnum + 1
while  ($nextnum <= $maxset)
	set thisname=`printf "STATLIST_%02d.STX" $setnum`
	set nextname=`printf "STATLIST_%02d.STX" $nextnum`
	\rm $thisname
	mv $nextname $thisname
	@ nextnum = $nextnum + 1
	@ setnum = $setnum + 1
end
