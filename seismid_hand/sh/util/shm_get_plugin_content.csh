#! /bin/csh
#
# file shm_get_plugin_content.csh
#      ==========================
#
# version 1, 23-May-2006
#
# Retrieves content file of available plugins and puts it to $1
# K. Stammler, 23-May-2006

if  ("$2" == "")  then
	echo "Usage: $0 <webadr> <outfile>"
	exit
endif

# get parameters
set webadr=$1
set outfile=$2

cd $SH_SCRATCH

if  (-e $outfile)  \rm $outfile
if  (-e content.txt)  \rm content.txt
wget $webadr/content.txt

if  (! -e content.txt)  exit
mv content.txt $outfile
