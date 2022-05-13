#! /bin/csh
#
# file al_autolocate.csh
#      =================
#
# version 1, 26-Mar-2001
#
# Performs automatic location
# K. Stammler, 26-Mar-2001

if  ("$2" == "")  then
	echo "Usage: $0 <station> <p-pick> [<inpath>]"
	exit
endif

# get parameters
set station=$1
set ppick=$2
set inpath=$3

# constants
set inpfile=SHINP.TXT
set outfile=SHOUT.TXT

if  (-e $inpfile)  \rm $inpfile
touch $inpfile
echo "hopen;;"                          >>$inpfile
echo "al_proc $station $ppick $inpath"  >>$inpfile
echo "quit y"                           >>$inpfile

if  (-e $outfile)  \rm $outfile
SH <$inpfile >$outfile
