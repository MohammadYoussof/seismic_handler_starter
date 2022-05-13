#! /bin/csh
#
#
# file list_station_buttons.csh
#      ========================
#
# version 1, 14-Nov-2003
#
# Lists all stations of a statlist file
# K. Stammler, 14-Nov-2003

if  ("$1" == "")  then
	echo "Usage $0 <fileno>"
	exit
endif

# get parameters
set fileno=`printf "%02d" $1`

set statfile=$SH_GLOBALS/STATLIST_$fileno.STX

if  (! -e $statfile)  then
	echo "$0 : cannot find statlist file $statfile.  Abort."
	exit
endif

set slist=`grep '\*' $statfile | awk '{print $2}'`

set cnt=1
foreach s ($slist)
	printf "%02d:%-5s " $cnt $s
	if  ($cnt % 8 == 0)  echo ""
	@ cnt = $cnt + 1
end
echo ""
