#! /bin/csh
#
# file app_velocities.csh
#      ==================
#
# version 3, 26-Aug-2005
#
# Compute apparent velocities (km/s) for phases in evtfile
# K. Stammler, 1-Apr-2003

if  ("$2" == "")  then
	echo "Usage: $0 <evtfile> <minphases>"
	exit
endif

# get parameters
set evtfile=$1
set minphase=$2

# set constants
set relfile=appvel_$$.000

#set echo

if  (! -e $evtfile)  then
	echo "$0 : input file $evtfile not found.  Abort."
	exit
endif

if  (-e $relfile)  \rm $relfile
$SH_UTIL/evt2phasetimes $evtfile >$relfile

if  (-z $relfile)  then
	echo "$0 : $evtfile cannot be displayed.  Abort."
	\rm $relfile
	exit
endif
set res=`wc -l $relfile`
set res=$res[1]
if  ($res < 3)  then
	echo "$0 : Need at least 3 phases.  Abort."
	\rm $relfile
	exit
endif

set phaselist=`awk '{print $1}' $relfile | sort -u`

set cnt = 0
foreach phase ($phaselist)

	@ cnt = $cnt + 1

	# fit line to data points
	#grep "^$phase" $relfile | awk '{print $3,$4}' >$phase.$$.000
	set res=`grep "^$phase " $relfile | awk '{print $3,$4}' | $SH_UTIL/fit_to_line $minphase`
	if  ($#res < 4)  then
		echo $phase
	else if  ("$res" == "NaN NaN NaN NaN")  then
		echo $phase
	else
		printf "$phase  v= %5.2f  d= %6.1f\n" $res[2] $res[1]
	endif

end

\rm $relfile

# print epicenter info if available
set origin=`grep 'Origin time' $evtfile`
if  ($#origin > 3)  then
	set origin=$origin[4]
else
	set origin=""
endif
set lat=`grep '^Latitude' $evtfile`
if  ($#lat > 2)  then
	set lat=$lat[3]
else
	set lat=""
endif
set lon=`grep '^Longitude' $evtfile`
if  ($#lon > 2)  then
	set lon=$lon[3]
else
	set lon=""
endif
set srcreg="`grep '^Source region' $evtfile`"
if  ("$srcreg" != "")  then
	set srcreg=`echo "$srcreg" | cut -c26-80`
else
	set srcreg=""
endif
echo "\! $origin"
echo "\! epi: $lat, $lon"
echo "\! $srcreg"
