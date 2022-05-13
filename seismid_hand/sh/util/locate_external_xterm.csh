#! /bin/csh
#
# file locate_external_xterm.csh
#      =========================
#
# version 5, 25-Feb-2001
#
# Location routine
# K. Stammler, 6-Feb-2001

if  ("$1" == "")  then
	echo "Usage: $0 <evtfile>"
	exit
endif

#set echo

# set parameters
set evtfile=$1

if  (! -e $evtfile)  then
	echo "$0 : input file $evtfile not found.  Abort."
	exit
endif

chdir $SH_SCRATCH

set method=simplex

set lat=`grep 'Latitude               :' $evtfile | head -1 | sed 's/+//'`
if  ($#lat < 3)  then
	set lat=""
else
	set lat=$lat[3]
endif
set lon=`grep 'Longitude              :' $evtfile | head -1 | sed 's/+//' `
if  ($#lon < 3)  then
	set lon=""
else
	set lon=$lon[3]
endif
set width=3.0
set step=0.1

set dep1=`grep 'Depth (km)             :' $evtfile | head -1`
if  ($#dep1 < 4)  then
	set dep1=33.0
else
	set dep1=$dep1[4]
endif
set dep2=$dep1
set ddep=50.0
set weight=1.0

while  (1 > 0)  # forever

	echo ""
	echo "Parameter settings"
	echo ""
	echo "(m) algorithm:                  $method"
	echo "(a) center latitude (deg):      $lat"
	echo "(o) center longitude (deg):     $lon"
	echo "(w) width of search area (deg): $width"
	echo "(s) step size (deg):            $step"
	echo "(h) weight of diff phases:      $weight"
	echo "(d) start depth (km):           $dep1"
	echo "(e) end depth (km):             $dep2"
	echo "(t) depth step (km):            $ddep"
	echo ""
	echo -n "cmd (<Return> starts computing): "

	set inp=$<
	if  ("$inp" == "")  break
	if  ("$inp" == "a")  then
		echo -n "new center latitude (deg): "
		set lat=$<
	else if  ("$inp" == "o")  then
		echo -n "new center longitude (deg): "
		set lon=$<
	else if  ("$inp" == "w")  then
		echo -n "new width of search area (deg): "
		set width=$<
	else if  ("$inp" == "s")  then
		echo -n "new step size (deg): "
		set step=$<
	else if  ("$inp" == "h")  then
		echo -n "new weight of diff phases: "
		set weight=$<
	else if  ("$inp" == "d")  then
		echo -n "new start,end depth (km): "
		set dep1=$<
		set dep2=$dep1
	else if  ("$inp" == "e")  then
		echo -n "new end depth (km): "
		set dep2=$<
	else if  ("$inp" == "t")  then
		echo -n "new depth step size (km): "
		set ddep=$<
	else if  ("$inp" == "m")  then
		echo -n "algorithm ('s'=simplex, 'g'=gridsearch): "
		set method=$<
		if  ("$method" == "g")  then
			set method=grid
		else
			set method=simplex
		endif
	else
		echo ""
		echo "Illegal input.  Try again."
	endif

end

$SH_UTIL/locate_by_ttfit.csh $evtfile $lat $lon $width $step \
	$dep1 $dep2 $ddep $weight $method

#echo -n '<Return> eingeben ...'
#set inp=$<

sleep 10

