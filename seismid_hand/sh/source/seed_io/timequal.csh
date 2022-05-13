#! /bin/csh
#
# file timequalmin.csh
#      ===============
#
# version 1, minimum of time quality in a seed file
# K. Stammler, 7-Aug-2000

if  ("$2" == "")  then
	echo "Usage: $0 <minmax> <seedfile>"
	exit
endif

# get parameters
set mode=$1
set seedfile=$2

if  (! -e $seedfile)  then
	echo 0
	echo "Seed file $seedfile not found."
	exit
endif

if  ("$mode" == "min")  then
	$SEED_PROG/seedquickdump $seedfile | grep -v '000001 V' | awk '{print $8}' | sort -n | head -1
else
	$SEED_PROG/seedquickdump $seedfile | awk '{print $8}' | sort -n | tail -1
endif

