#! /bin/csh
#
# file menu_main.csh
#      =============
#
# version 1, 15-Nov-2003
#
# main menu of SHM setup
# K. Stammler, 15-Nov-2003

set xprog=$SH_ROOT/setup

set cmd=""
while  (1 > 0)  # forever

	echo ""
	echo "SHM setup menu"
	echo ""
	echo "(s)   station setup submenu"
	echo "(p)   plugin submenu"
	echo "(q)   quit"
	echo ""
	echo -n "cmd: "
	set cmd="$<"

	if  ("$cmd" == "q")  exit
	if  ("$cmd" == "s")  then
		$xprog/menu_station.csh
	else if  ("$cmd" == "p")  then
		$xprog/menu_plugin.csh
	else if  ("$cmd" == "h")  then
		echo ""
		echo "(s) Station setup menu."
	endif

	echo ""
	echo -n "Enter <Return> ..."
	set inp="$<"

end

