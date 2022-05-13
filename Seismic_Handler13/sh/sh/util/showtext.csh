#! /bin/csh
#
# file showtext.csh
#      ============
#
# version 1, 1-Dec-97
#
# Displays text in an xterm and waits for <Return>.
# K. Stammler, 1-Dec-97

if  ("$1" == "")  then
	echo "$0 <file1> [<file2> ...]
	exit
endif

xterm -c "$SH_UTIL/cat_and_wait.csh $*"

