#! /bin/csh
#
# file identify_seed.csh
#      =================
#
# version 1, 11-Aug-98
#
# Returns first stream and start time found in SEED file.
# K. Stammler, 11-Aug-98

if  ("$1" == "")  then
	echo "Usage: $0 <seedfile> [<recordlth>]"
	exit
endif

# get parameters
set seedfile=$1
set reclth=$2

if  (! -e $seedfile)  then
	echo "$0 : Input file $seedfile not found. Abort."
	exit
endif

set tmp=""
if  ("$reclth" != "")  set tmp="-seedrec=$reclth"
set res=`$SEED_PROG/seedquickdump $tmp $seedfile | grep ' D ' | head -1`
if  ($#res < 5)  then
	echo "$0 : Syntax error in output.  File $seedfile.   Abort."
	exit
endif

set string=`echo $res[4] | sed 's/-/ /g'`

echo $string $res[5]
