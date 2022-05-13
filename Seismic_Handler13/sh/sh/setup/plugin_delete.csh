#! /bin/csh
#
# file plugin_delete.csh
#      =================
#
# version 1, 25-Nov-2003
#
# Deletes plugin from menu
# K. Stammler, 25-Nov-2003

# get number from command line or prompt
set posnum=$1

if  ("$posnum" == "")  then
	echo -n "Enter position number: "
	set posnum="$<"
endif

# set constants
set setup=$SH_USERROOT/private/plugins.txt
if  (! -e $setup)  set setup=$SH_USERROOT/private/external_processes.txt

if  ($posnum < 1 || $posnum > 20)  then
	echo "Illegal menu position number.  Abort."
	exit
endif

if  (-e $setup.x)  \rm $setup.x
grep -v " $posnum " $setup >$setup.x
\rm $setup
mv $setup.x $setup
