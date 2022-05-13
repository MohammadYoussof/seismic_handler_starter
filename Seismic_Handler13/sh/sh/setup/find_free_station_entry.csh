#! /bin/csh
#
# file find_free_station_entry.csh
#      ===========================
#
# version 1, 15-Nov-2003
#
# Finds free station entry in station sets
# K. Stammler 15-Nov-2003

set xprog=$SH_ROOT/setup

set setnum=`$xprog/noof_button_sets.csh`

set num=1
while  ($num <= $setnum)
	set num=`printf "%02d" $num`
	set fname=$SH_GLOBALS/STATLIST_$num.STX
	set res=`grep '^*' $fname | grep -n '* ---' | head -1 | sed 's/:/ /' | awk '{print $1}'`
	if  ("$res" != "")  then
		echo $num $res
		exit
	endif
	@ num = $num + 1
end

echo 0 0
