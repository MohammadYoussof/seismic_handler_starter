#! /bin/csh
#
# file call_stadtplandienst.csh
#      ========================
#
# version 1, 14-Dec-2005
#
# Calls mapping program stadtplandienst for displaying locations in Germany
# K. Stammler, 14-Dec-2005

if  ("$1" == "")  then
	echo "Usage: $0 <evtfile>"
	exit
endif

# get parameters
set evt=$1

# set constants
set lastsid=$SH_SCRATCH/last_stadtplandienst_sid.000

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

echo $lat $lon
set tmp=`$SH_UTIL/deg2dms $lat`
set latdeg=$tmp[1]
set latmin=$tmp[2]
set latsec=$tmp[3]
set tmp=`$SH_UTIL/deg2dms $lon`
set londeg=$tmp[1]
set lonmin=$tmp[2]
set lonsec=$tmp[3]

if  (-e $lastsid)  then
	set sid=`cat $lastsid`
else
	set sid=0
endif

set fage=`$SH_UTIL/fileage.csh $lastsid`
if  ($fage > 600)  then
	$SH_UTIL/get_stadtplandienst_sid.csh
endif
set adr="http://www.stadtplandienst.de/map.asp?sid=$sid&start.x=5&CoordXDeg=$londeg&CoordXMin=$lonmin&CoordXSec=$lonsec&CoordYDeg=$latdeg&CoordYMin=$latmin&CoordYSec=$latsec&Abschicken=GO"

if  ("`uname`" == "SunOS")  then
	/programs/sol2/netscape-7.0/SUNWns/netscape "$adr" &
else
	firefox "$adr" &
endif
