#! /bin/csh
#
# file display_mapquest.csh
#      ====================
#
# version 2, 18-Dec-2005
#
# Calls mapping program stadtplandienst for displaying locations in Germany
# K. Stammler, 14-Dec-2005

if  ("$2" == "")  then
	echo "Usage: $0 <lat> <lon>"
	exit
endif

#set echo

# get parameters
set lat=$1
set lon=$2

# set constants
set tmpfile=$HOME/dmapq_$$.000

#set adr="http://www.mapquest.com/maps/map.adp?searchtype=address&formtype=address&latlongtype=decimal&latitude=$lat&longitude=$lon"
set adr="http://www.mapquest.com/maps/map.adp?searchtype=address&latlongtype=decimal&latitude=$lat&longitude=$lon"

set browser=`$SH_UTIL/which_browser.csh`

$SH_UTIL/get_html_text.csh http://www.mapquest.com/maps/latlong.adp $tmpfile
$browser "$adr" &
\rm $tmpfile
