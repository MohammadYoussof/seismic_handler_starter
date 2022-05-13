#! /bin/csh
#
# file insert_station_location.csh
#      ===========================
#
# version 1, 15-Nov-2003
#
# Inserts/Modifies station location information ($SH_INPUTS/STATINF.DAT)
# K. Stammler, 15-Nov-2003

set xprog=$SH_ROOT/setup

set infofile=$SH_INPUTS/STATINF.DAT

echo ""
echo -n "Station name: "
set station="$<"
set station=`echo $station | sed y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/`

set res=`grep "^$station " $infofile`

if  ("$res" == "")  then

	set new_entry=1
	set inp="y"

else

	set new_entry=0

	echo "Current entry of $station :"
	echo $res
	echo -n "change ? [y/n/d]: "
	set inp="$<"
	if  ("$inp" != "y" && "$inp" != "d")  then
		echo "No changes made."
		exit
	endif

endif

if  ("$inp" != "d")  then
	echo -n "Latitude of ${station} (southern latitudes negative please): "
	set lat="$<"
	echo -n "Longitude of ${station} (western longitudes negative please): "
	set lon="$<"
	echo -n "Elevation in m: "
	set elev="$<"

	if  ("$elev" == "")  set elev="0.0"
	if  ("$lat" == "" || "$lon" == "")  then
		echo "Illegal input.  No changes made."
		exit
	endif
endif

if  ($new_entry == 1)  then

	if  (-e $infofile.x)  \rm $infofile.x
	mv $infofile $infofile.x

else

	if  (-e $infofile.x)  \rm $infofile.x
	grep -v "^$station " $infofile >$infofile.x
	\rm $infofile
	if  ("$inp" == "d")  then
		mv $infofile.x $infofile
		echo "Station $station deleted from location file."
		exit
	endif

endif

echo "$station lat:$lat	lon:$lon	elevation: $elev" >$infofile
cat $infofile.x >>$infofile
\rm $infofile.x

