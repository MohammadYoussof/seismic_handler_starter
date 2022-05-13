#! /bin/csh
#
# file locate_external.csh
#      ===================
#
# version 1, 6-Feb-2001
#
# Location routine
# K. Stammler, 6-Feb-2001

if  ("$1" == "")  then
	echo "Usage: $0 <evtfile>"
	exit
endif

# set parameters
set evtfile=$1

if  (! -e $evtfile)  then
	echo "$0 : input file $evtfile not found.  Abort."
	exit
endif

chdir $SH_SCRATCH

xterm -e ${SH_UTIL}locate_external_xterm.csh $evtfile

