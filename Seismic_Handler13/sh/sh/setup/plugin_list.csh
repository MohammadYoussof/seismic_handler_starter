#! /bin/csh
#
# file plugin_list.csh
#      ===============
#
# version 2, 23-May-2006
#
# Lists installed plugins
# K. Stammler, 25-Nov-2003

set setup=$SH_USERROOT/private/plugins.txt
if  (! -e $setup)  set setup=$SH_USERROOT/private/external_processes.txt

if  (! -e $setup)  then
	echo ""
	echo "No plugins defined in menu."
	echo ""
	exit
endif

printf "\nNumber\tName\n\n"
grep ^label $setup | awk '{print $2 "\t" $3}'
echo ""
