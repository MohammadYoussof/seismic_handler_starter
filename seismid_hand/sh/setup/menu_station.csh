#! /bin/csh
#
# file menu_station.csh
#      ================
#
# version 1, 15-Nov-2003
#
# station management in SHM
# K. Stammler, 15-Nov-2003

set xprog=$SH_ROOT/setup

set cmd=""
while  (1 > 0)  # forever

	echo ""
	echo "Station Submenu"
	echo ""
	echo "(h)   help text about this menu"
	echo "(l)   list existing read dialog button sets"
	echo "(a)   add station name to button set"
	echo "(d)   delete station from button set"
	echo "(c)   create empty button set"
	echo "(r)   remove complete button set"
	echo "(s)   add/modify/delete station location information"
	echo "(g)   assign transfer function and gain factor for a station"
	echo "(t)   define new transfer function class"
	echo "(q)   exit Station Submenu"
	echo ""
	echo -n "cmd: "
	set cmd="$<"

	if  ("$cmd" == "q")  exit
	if  ("$cmd" == "l")  then
		$xprog/list_station_sets.csh
	else if  ("$cmd" == "a")  then
		$xprog/add_station_to_set.csh
	else if  ("$cmd" == "d")  then
		$xprog/delete_station_from_set.csh
	else if  ("$cmd" == "c")  then
		$xprog/create_button_set.csh
	else if  ("$cmd" == "r")  then
		$xprog/remove_button_set.csh
	else if  ("$cmd" == "s")  then
		$xprog/insert_station_location.csh
	else if  ("$cmd" == "g")  then
		$xprog/station_tf_and_gain.csh
	else if  ("$cmd" == "t")  then
		$xprog/define_tf_class.csh
	else if  ("$cmd" == "h")  then
		echo ""
		echo "The procedure to add a complete new station is:"
		echo "1. Decide to which dialog box (button set number) and on which"
		echo "   position (1 - 30) the station name should appear or accept"
		echo "   defaults in next step.  For an overwiev on the existing station"
		echo "   entries use menu item (l).  If no more free entries are available"
		echo "   or if you want to have a separate button set, create a new button"
		echo "   set using menu item (c)."
		echo "2. Add station name to button set, menu item (a).  If you accept the"
		echo "   defaults the station name will appear at the first free button."
		echo "3. Add station location information to the SH/SHM station data file"
		echo "   using menu item (s)."
		echo "4. Assign transfer function and gain factor to a station using menu"
		echo "   item (g).  Only predefined transfer function classes may be entered"
		echo "   here.  A list of these predefined transfer function classes is"
		echo "   shown when executing this function."
		echo "5. If no predefined transfer function class can be used for your"
		echo "   station define a new transfer function class using menu item (t)."
		echo "   You need the transfer function of your recording system for"
		echo "   velocity input as poles and zeros and a normalization factor."
	endif

	echo ""
	echo -n "Enter <Return> ..."
	set inp="$<"

end

