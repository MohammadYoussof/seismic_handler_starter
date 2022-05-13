#! /bin/csh
#
# file seedgaps_file.csh
#      =================
#
# version 1, 4-Mar-96
#
# finds gaps in a single SEED file, creating its own sfdfile
# K. Stammler, 4-Mar-96


if  ("$1" == "")  then
	echo "Usage: $0 <seedfile>"
	exit
endif

# get parameters
set seedfile=$1

# set constants
set sfdfile=$HOME/sgapsf_$$.000

if  (! -r $seedfile)  then
	echo "$0 : cannot read input file $seedfile.  Abort"
	exit
endif

if  (-e $sfdfile)  \rm $sfdfile
$SEED_PROG/sfdline $seedfile >$sfdfile
set span=`$SEED_PROG/inquire_max_span $sfdfile`
set stream=`$SEED_PROG/inquire_first_stream.csh $sfdfile`
$SEED_PROG/seedgaps -sfdfile=$sfdfile $stream $span[1] $span[2]

\rm $sfdfile
