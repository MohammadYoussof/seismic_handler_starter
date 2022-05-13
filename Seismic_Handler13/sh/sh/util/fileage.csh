#! /bin/csh
#
# file fileage.csh
#      ===========
#
# version 4, 28-Dec-2006
#
# Returns age of file in sec
# K. Stammler, 8-May-2000

if  ("$1" == "")  then
	echo "Usage: $0 <file>"
	exit
endif

# get parameters
set fname=$1

#set echo

if  ("`uname`" == "Linux")  then

	if  (! -e $fname)  exit

	set ftime=`ls -l --time-style=+%d-%h-%y_%T $fname | awk '{print $6}'`
	if  ("$ftime" == "")  then
		set res=`ls -l --full-time $fname`
		if  ($#res < 7)  exit
		set tm=`echo $res[7] | sed 's/:/ /g'`
		set res=`echo $res[6] | sed 's/-/ /g'`
		set secs=`echo $tm[3] | sed 's/\./ /'`
		set secs=$secs[1]
		set ftime=$res[3],$res[2],$res[1],$tm[1],$tm[2],$secs
		set ctime=`date +%d,%m,%y,%H,%M,%S`
		set tdiff=`$SH_UTIL/timename time_intdiff $ctime $ftime`
		echo $tdiff
	else
		$SH_UTIL/timename time_intdiff `date +%d-%h-%y_%T` $ftime
	endif

else

	if  (! -e $fname)  then
		echo "-1 file $fname does not exist."
		exit
	endif
	
	set res=`ls -l $fname`
	if  ($#res < 8)  then
		echo "-1 Syntax error in output of ls"
		exit
	endif

	set month=$res[6]
	set day=$res[7]
	set tm=$res[8]
	set year=`date +%y`

	$SH_UTIL/timename time_intdiff `date +%d-%h-%y_%T` $day-$month-${year}_$tm

endif

