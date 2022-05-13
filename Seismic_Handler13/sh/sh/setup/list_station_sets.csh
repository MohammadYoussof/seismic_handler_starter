#! /bin/csh
#
# file list_station_sets.csh
#      =====================
#
# version 1, 14-Nov-2003
#
# List station sets
# K. Stammler. 14-Nov-2003

set xpath=$SH_ROOT/setup

set num=`$xpath/noof_button_sets.csh`

set cnt=1
while  ($cnt <= $num)
	echo ""
	echo "set $cnt"
	$xpath/list_station_set.csh $cnt
	@ cnt = $cnt + 1
end
