#! /bin/csh
#
# file check_for_event_archived.csh
#      ============================
#
# version 1, 3-Mar-2005
#
# Checks whether event has been archived if many phases picked
# K. Stammler, 3-Mar-2005

if  ("$1" == "")  then
	echo "Usage: $0 <evtfile>"
	exit
endif

# get parameters
set evt=$1

if  (! -e $evt)  then
	echo "$0 : evt file $evt not found.  Abort."
	exit
endif

set orig=`grep 'Origin time' $evt | head -1 | awk '{print $4}'`
if  ("$orig" == "")  exit

set origstr=`$SH_UTIL/timename time_digit10 $orig`

set fname="EEU_$origstr.GSE"

set arch=0
if  (-e $DATA/seismo/events/current/$fname)  set arch=1
if  (-e $DATA/seismo/events/current/$fname.gz)  set arch=1

if  ($arch == 1)  exit

set pnum=`grep -c 'Phase name' $evt`
set ptyp=`grep 'Phase name' $evt | awk '{print $4}' | sort -u | wc -l`

if  ($pnum > 30 || $ptyp > 3)  then
	echo ""
	echo "Event not archived.  Consider calling 'Archive Event'"
endif

