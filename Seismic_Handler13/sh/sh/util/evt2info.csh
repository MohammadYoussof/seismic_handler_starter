#! /bin/csh
#
# file evt2info.csh
#      ============
#
# version 1, 28-Dec-2005
#
# Extracts event info summary from evt file
# K. Stammler, 28-Dec-2005

if  ("$1" == "")  then
	echo "Usage: $0 <evtfile>"
	exit
endif

# get parameters
set evt=$1

if  (! -e $evt)  then
	echo "$0 : evt file $evt not found.  Abort."
	exit
endif

set origtime=`grep 'Origin time' $evt | head -1 | awk '{print $4}'`
set region=`grep 'Source region' $evt | head -1 | sed 's/Source region          : //'`
set lat=`grep 'Latitude               :' $evt | head -1 | awk '{print $3}'`
set lon=`grep 'Longitude              :' $evt | head -1 | awk '{print $3}'`

set mb=`grep 'Magnitude mb           :' $evt`
if  ("$mb" != "")  set mb=`grep 'Magnitude mb           :' $evt | awk '{print $4}' | $DPROG/stddev - | awk '{print $5}'`
set ms=`grep 'Magnitude ms           :' $evt`
if  ("$ms" != "")  set ms=`grep 'Magnitude ms           :' $evt | awk '{print $4}' | $DPROG/stddev - | awk '{print $5}'`
set mbb=`grep 'Broadband Magnitude    :' $evt`
if  ("$mbb" != "")  set mbb=`grep 'Broadband Magnitude    :' $evt | awk '{print $4}' | $DPROG/stddev - | awk '{print $5}'`

echo "Origin time: $origtime<br>"
printf "Epicentre Latitude: %5.1f<br>\n" $lat
printf "Epicentre Longitude: %6.1f<br>\n" $lon
echo "Source region: $region<br>"
if  ("$mb" != "")  printf "Mean Magnitude mb: %3.1f<br>\n" $mb
if  ("$ms" != "")  printf "Mean Magnitude ms: %3.1f<br>\n" $ms
if  ("$mbb" != "")  printf "Mean Broadband Magnitude: %3.1f<br>\n" $mbb
