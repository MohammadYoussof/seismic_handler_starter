#! /bin/csh
#
# file call_mapquest.csh
#      =================
#
# version 1, 16-Dec-2005
#
# Calls mapping program mapquest for displaying locations
# K. Stammler, 16-Dec-2005

if  ("$1" == "")  then
	echo "Usage: $0 <evtfile>"
	exit
endif

# get parameters
set evt=$1

# set constants

if  (! -e $evt)  then
	echo "$0 : Input evtfile $evt not found.  Abort."
	exit
endif

set lat=`grep ^Latitude $evt`
set lon=`grep ^Longitude $evt`

if  ($#lat < 3  ||  $#lon < 3)  then
	echo "$0 : No location found in Evt-file.  Abort."
	exit
endif
set lat=`echo $lat[3] | sed 's/^+//'`
set lon=`echo $lon[3] | sed 's/^+//'`

$SH_UTIL/display_mapquest.csh $lat $lon &
