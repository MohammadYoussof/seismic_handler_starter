#! /bin/csh
#
# file cat_and_wait.csh
#      ================
#
# version 2, 27-Aug-97
#
# displays text and waits for return
# K. Stammler, 11-Mar-96

if  ("$1" == "")  then
	echo "Usage $0 <file> [<file2>]"
else
	cat $*
endif

echo ""
echo -n "enter <Return> ..."
set inp=$<

