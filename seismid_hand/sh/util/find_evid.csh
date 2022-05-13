#! /bin/csh
#
# file find_evid.csh
#      =============
#
# version 1, 28-Dec-2005
#
# Find event id number for a given origin time string
# K. Stammler, 28-Dec-2005

if  ("$1" == "")  then
	echo "Usage: $0 <origstr>"
	echo "  Example: $0 051228_1253"
	exit
endif

# get parameters
set origstr=$1

# set constants
set evtpath=$EVT/evt_archive

# get time
set origtime=`$SH_UTIL/timename name_time xxx_$origstr`
if  ($#origtime != 2)  exit
set origtime=$origtime[2]

# get day string
set daystr=`echo $origstr | sed 's/_/ /'`
set daystr=$daystr[1]

set found = ( )

set evtlist=`ls $evtpath/shm_1$daystr*`
foreach evt ($evtlist)
	set otime=`grep 'Origin time' $evt | head -1 | awk '{print $4}'`
	if  ("$otime" == "")  continue
	set diff=`$SH_UTIL/timename time_intdiff $otime $origtime | sed 's/^-//'`
	if  ($diff < 60)  set found = ( $found $evt )
end

echo $found
