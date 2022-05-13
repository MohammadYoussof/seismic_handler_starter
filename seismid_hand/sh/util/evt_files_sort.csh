#! /bin/csh
#
# file evt_files_sort.csh
#      ==================
#
# version 3, 28-Nov-97
#
# sorts list of evt-files by origin time
# K. Stammler, 23-Jun-95

if  ("$1" == "")  then
	echo "Usage: $0 <listfile>"
	exit
endif

#set echo

# set constants
set detecprog=$SH_UTIL
set scrfile=$SH_SCRATCH/evt_files_sort_$$.000
set scrfile2=$SH_SCRATCH/evt_files_sorted_$$.000

# get parameters
set inplist=$1

# prepare loop
set evtlist=`cat $inplist`
if  (-e $scrfile)  \rm $scrfile
if  (-e $scrfile2)  \rm $scrfile2
touch $scrfile

# find first onset time in each file
foreach i ($evtlist)
	set line=`grep 'Onset time' $i | head -1`
	if  ("$line" == "")  continue
	set timestr=`$detecprog/timename time_digit12 $line[4]`
	echo $timestr $i >>$scrfile
end

# sort by onset times
sort $scrfile >$scrfile2

set sortedlist=`awk '{print $2}' $scrfile2`
foreach i ($sortedlist)
	echo $i
end

\rm $scrfile $scrfile2
