#! /bin/csh
#
#
# file noof_button_sets.csh
#      ====================
#
# version 1, 14-Nov-2003
#
# find number of station sets defined
# K. Stammler, 14-Nov-2003

set cnt=1
while  (1 > 0)  # forever
	set cnt=`printf "%02d" $cnt`
	set fname=$SH_GLOBALS/STATLIST_$cnt.STX
	if  (! -e $fname)  break
	@ cnt = $cnt + 1
end

@ cnt = $cnt - 1
echo $cnt
