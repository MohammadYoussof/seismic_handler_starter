#! /bin/csh
#
# file archive_end_time.csh
#      ====================
#
# version 8, 14-Jan-2000
#
# computes end time for archiving SEED data
# K. Stammler, 23-Dec-94

if  ("$1" == "" || "$2" == "" || "$3" == "" || "$4" == "")  then
	echo "Usage: $0 <sfdfile> <starttime> <streamlist> <no-of-kB> <tol> <startspan>"
	exit
endif

#set echo

# copy command line parameters
set sfdfile=$1
set starttime=$2
set streamlistfile=$3
set kbnum=$4

# optional parameters
set kbtol=$5
if  ("$kbtol" == "")  set kbtol=400          # tolerance in number of kb
set startspan=$6
if  ("$startspan" == "")  set startspan=7200   # start time span

# set constants
set detecprog=$DPROG
set scrfile=$HOME/archive_end_time_$$.000

if  (-e $scrfile)  \rm $scrfile
touch $scrfile
set streamlist=`cat $streamlistfile`

set timespan=$startspan
set minkb
set maxkb
set loopcnt=1
@ minkb = $kbnum - $kbtol
@ maxkb = $kbnum + $kbtol

loop:

	# compute mean time/record with current span
	# echo span $timespan
	set endtime=`$SH_UTIL/timename time_addsec $starttime $timespan`
	set totkb = 0
	foreach stream ($streamlist)
		# check whether stream is available
		set res=`$SEED_PROG/inquire_avd $sfdfile $stream`
		if  ("$res" == "")  then
			# echo "stream $stream not available"
			continue
		endif
		set res=`$SEED_PROG/copy_recs $sfdfile $stream $starttime $endtime`
		if  ("$res" == "")  set res="0"
		@ totkb = $totkb + $res[1]
		# echo stream $stream kB $res[1]
	end
	# echo totkb $totkb
	echo "span $timespan, totkb $totkb, max $maxkb, min $minkb" >>$scrfile

	if  ($totkb < $maxkb && $totkb > $minkb)  goto loop_exit

	# compute new span
	set fact=`$detecprog/floatop $kbnum div $totkb`
	set timespan=`$detecprog/floatop $timespan mul $fact`

	# check loop counter
	@ loopcnt = $loopcnt + 1
	if  ($loopcnt > 100)  then
		echo "no convergence"
		if  (-e $scrfile)  \rm $scrfile
		exit
	endif

goto loop
loop_exit:

echo $endtime $timespan

\rm $scrfile
