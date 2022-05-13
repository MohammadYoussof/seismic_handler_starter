#! /bin/csh
#
# file add_station_to_set.csh
#      ======================
#
# version 1, 15-Nov-2003
#
# Adds station name to button set, interactive routine
# K. Stammler, 15-Nov-2003

set xprog=$SH_ROOT/setup

set res=`$xprog/find_free_station_entry.csh`
set maxset=`$xprog/noof_button_sets.csh`

if  ($#res != 2)  then
	echo "Program bug in setup management. ($0)"
	exit
endif

set setnum=$res[1]
set pos=$res[2]

echo -n "Enter station name (max. five characters): "
set station="$<"
set res=`echo $station | wc -c`
if  ($res > 5)  then
	echo "Too many characters.  Input not accepted."
	exit
endif
set station=`echo $station | sed y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/`

echo -n "Enter dialog box number [default: $setnum]: "
set inp="$<"
if  ("$inp" != "")  set setnum=$inp
if  ($setnum > $maxset)  then
	echo "Maximum dialog box number is $maxset.  Input not accepted."
	exit
endif

echo -n "Enter button number [default: $pos]: "
set inp="$<"
if  ("$inp" != "")  set pos=$inp
if  ($pos > 30)  then
	echo "Maximum button number is 30.  Input not accepted."
	exit
endif

echo ""
echo "adding $station in dialog box $setnum at position $pos"
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
sed ${line}c"* $station" $fname >$fname.x
\rm $fname
mv $fname.x $fname
