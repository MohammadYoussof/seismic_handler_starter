#! /bin/csh
#
# file menu_plugin.csh
#      ===============
#
# version 1, 25-Nov-2003
#
# plugin menu
# K. Stammler, 25-Nov-2003

set xprog=$SH_ROOT/setup

set cmd=""
while  (1 > 0)  # forever

	echo ""
	echo "plugin menu"
	echo ""
	echo "(l)   list installed plugins"
	echo "(i)   install plugin"
	echo "(d)   delete plugin"
	echo "(q)   exit this submenu"
	echo ""
	echo -n "cmd: "
	set cmd="$<"

	if  ("$cmd" == "q")  exit
	if  ("$cmd" == "l")  then
		$xprog/plugin_list.csh
	else if  ("$cmd" == "i")  then
		$xprog/plugin_insert.csh
	else if  ("$cmd" == "d")  then
		$xprog/plugin_delete.csh
	endif

	echo ""
	echo -n "Enter <Return> ..."
	set inp="$<"

end

