#! /bin/csh
#
# file get_isc_events.csh
#      ==================
#
# version 2, 16-Jun-2006
#
# Returns events list from ISC
# K. Stammler, 9-Apr-2006

if  ("$3" == "")  then
	echo "Usage: $0 <fromtime> <totime> <outfile> [<minmagn>]"
	exit
endif

# get parameters
set time1=$1
set time2=$2
set out=$3
set minmagn=$4

# set constants
set rootadr="http://www.isc.ac.uk/cgi-bin/bull-link"

# if <fromtime> is too close to today, no ISC data can be expected
set today=`date +%d-%h-%Y`
set diff=`$SH_UTIL/timename time_daydiff $today $time1`
if  ($diff < 365)  then
	echo "$0 : event too new.  ISC not searched."
	touch $out
	exit
endif

# increment end day due to truncation of hours
set time2=`$SH_UTIL/timename time_addsec $time2 86400`
set time1=`$SH_UTIL/timename time_subsec $time1 7200`

set res1=`$SH_UTIL/timename time_to_int $time1`
set res2=`$SH_UTIL/timename time_to_int $time2`

if  ($#res1 < 7 || $#res2 < 7)  then
	echo "$0 : error parsing time: $time1 $time2.  Abort."
	exit
endif

set req="$rootadr?Year=$res1[1]&Month=$res1[2]&start_day=$res1[3]"
set req="$req&start_hour=$res1[4]&end_day=$res2[3]&end_hour=$res2[4]"
set req="$req&min_dep=&max_dep=&min_def=&max_def=&min_mag=$minmagn&max_mag="
set req="$req&req_mag_type=&req_mag_agcy=&min_amp=&max_amp=&max_lat="
set req="$req&ctr_lat=&ctr_lon=&min_lon=&max_lon=&max_dist=&min_lat="
set req="$req&include_magnitudes=on"
set req="$req&include_headers=on"

if  (-e $out)  \rm $out
$SH_UTIL/get_html_text.csh "$req" $out 132
