#! /bin/csh
#
# file no_of_phases_in.csh
#      ===================
#
# version 1, 7-Jan-2004
#
# Returns number of phases between two given times
# K. Stammler, 7-Jan-2004

if  ("$3" == "")  then
	echo "Usage: $0 <evtfile> <ts> <te>"
	exit
endif

# get parameters
set evtfile=$1
set ts=$2
set te=$3

if  (! -e $evtfile)  then
	echo "$0 : cannot find input file $evtfile.  Abort."
	exit
endif

set onslist=`grep 'Onset time' $evtfile | awk '{print $4}'`

set cnt=0
foreach ons ($onslist)
	set adiff=`$SH_UTIL/timename time_intdiff $te $ons`
	set bdiff=`$SH_UTIL/timename time_intdiff $ons $ts`
	if  ($adiff >= 0 && $bdiff >= 0)  then
		@ cnt = $cnt + 1
	endif
end

echo $cnt
