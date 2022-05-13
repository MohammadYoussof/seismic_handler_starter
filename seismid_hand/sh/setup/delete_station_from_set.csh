#! /bin/csh
#
# file delete_station_from_set.csh
#      ===========================
#
# version 1, 15-Nov-2003
#
# Deletes station from button set, interactive routine
# K. Stammler, 15-Nov-2003

set xprog=$SH_ROOT/setup
set maxset=`$xprog/noof_button_sets.csh`

echo -n "Enter dialog box number (between 1 and $maxset): "
set setnum="$<"
if  ("$setnum" == "" || "$setnum" == "q")  then
	echo "Operation aborted."
	exit
endif
if  ($setnum > $maxset)  then
	echo "Maximum dialog box number is $maxset.  Input not accepted."
	exit
endif

echo -n "Enter button number: "
set pos="$<"
if  ("$pos" == "" || "$pos" == "q")  then
	echo "Operation aborted."
	exit
endif
if  ($pos > 30)  then
	echo "Maximum button number is 30.  Input not accepted."
	exit
endif

echo ""
echo "deleting position $pos in dialog box $setnum"
echo ""

cd $SH_GLOBALS
set setnum=`printf "%02d" $setnum`
set fname=STATLIST_$setnum.STX
if  (! -e $fname)  then
	echo "Cannot find button set $setnum.  Program bug."
	exit
endif

set offset=`grep -n '*' $fname | head -1 | sed 's/:/ /' | awk '{print $1}'`
@ offset = $offset - 1
@ line = $offset + $pos
if  (-e $fname.x)  \rm $fname.x
sed ${line}c"* ---" $fname >$fname.x
\rm $fname
mv $fname.x $fname
