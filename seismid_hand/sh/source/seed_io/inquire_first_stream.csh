#! /bin/csh
#
# file inquire_first_stream.csh
#      ========================
#
# version 1, 4-Mar-96
#
# returns first stream found in sfdfile
# K. Stammler, 4-Mar-96

if  ("$1" == "")  then
	echo "Usage: $0 <sfdfile>"
	exit
endif

# get parameters
set sfdfile=$1

if  (! -r $sfdfile)  then
	echo "$0 : couldn't open sfdfile $sfdfile. Abort"
	exit
endif

set line=`head -1 $sfdfile`
set stream=`$DPROG/stringop find "$line" 's>'`
set stream=`$DPROG/stringop extract $stream[1] 2 -1`
echo $stream
