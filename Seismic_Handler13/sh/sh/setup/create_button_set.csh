#! /bin/csh
#
# file create_button_set.csh
#      =====================
#
# version 1, 15-Nov-2003
#
# Creates new button set with empty entries
# K. Stammler, 15-Nov-2003

set xprog=$SH_ROOT/setup

set maxset=`$xprog/noof_button_sets.csh`

cd $SH_GLOBALS

# check whether last button set is empty
set lastname=`printf "STATLIST_%02d.STX" $maxset`
set res=`grep -c '* ---' $lastname`
if  ($res >= 30)  then
	echo "Last button set (number $maxset) is empty.  Use this first."
	exit
endif

@ maxset = $maxset + 1

set newname=`printf "STATLIST_%02d.STX" $maxset`

echo ""
echo "creating button set number $maxset ($newname)"
echo ""

if  (-e $newname)  \rm $newname
touch $newname
echo "! file $newname"              >> $newname
echo "!      ==============="       >> $newname
echo "!"                            >> $newname
echo "! created using $0 at `date`" >> $newname
echo "!"                            >> $newname
echo ""                             >> $newname
echo "set1:  0 14 HALF1"            >> $newname
echo "set2: 15 29 HALF2"            >> $newname
echo ""                             >> $newname
set cnt=1
while  ($cnt <= 30)
	echo "* ---"                        >> $newname
	@ cnt = $cnt + 1
end
