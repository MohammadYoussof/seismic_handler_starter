#! /bin/csh
#
# file report_online_events.csh
#      ========================
#
# version 2, 25-Oct-99
#
# Reports online amplitude data (and onsets ?)
# K. Stammler, 9-Sep-99

#exit  # this disables reporting

# set constants
set lp_thresh1=60
set sp_thresh1=700
set lp_thresh2=40
set sp_thresh2=300
set tmpfile=$SH_SCRATCH/repev_$$.000
set lphist=$SH_SCRATCH/history_report_lp.dat
set sphist=$SH_SCRATCH/history_report_sp.dat
set alhist=$SH_SCRATCH/history_alarm.dat
set far_in_the_past="1-jan-80"
set dumpfile=$HOME/online_alarm.txt

if  (-e $tmpfile)  \rm $tmpfile

# get parameters
if  ("$3" == "")  then
	touch $tmpfile
	echo "Usage: $0 <time> <ampl> <filter>"  >>$tmpfile
	$SH_UTIL/online_alarm.csh $tmpfile
	exit
endif
set ampltime=$1
set ampl=$2
set filter=$3

if  ("`echo $filter | grep STANDARD_BP`" != "")  then
	set filter="SP"
else if  ("`echo $filter | grep LP`" != "")  then
	set filter="LP"
else if  ("`echo $filter | grep SP`" != "")  then
	set filter="SP"
endif

# get history
if  (-e $lphist)  then
	set res=`cat $lphist`
	if  ($#res == 5)  then
		set temp_lp_ampltime=$res[1]
		set temp_lp_ampl=$res[2]
		set last_lp_ampltime=$res[3]
		set last_lp_ampl=$res[4]
		set last_lp_alarm=$res[5]
	else
		set temp_lp_ampltime="$far_in_the_past"
		set temp_lp_ampl=0
		set last_lp_ampltime="$far_in_the_past"
		set last_lp_ampl=0
		set last_lp_alarm="$far_in_the_past"
	endif
else
	set temp_lp_ampltime="$far_in_the_past"
	set temp_lp_ampl=0
	set last_lp_ampltime="$far_in_the_past"
	set last_lp_ampl=0
	set last_lp_alarm="$far_in_the_past"
endif
if  (-e $sphist)  then
	set res=`cat $sphist`
	if  ($#res == 5)  then
		set temp_sp_ampltime=$res[1]
		set temp_sp_ampl=$res[2]
		set last_sp_ampltime=$res[3]
		set last_sp_ampl=$res[4]
		set last_sp_alarm=$res[5]
	else
		set temp_sp_ampltime="$far_in_the_past"
		set temp_sp_ampl=0
		set last_sp_ampltime="$far_in_the_past"
		set last_sp_ampl=0
		set last_sp_alarm="$far_in_the_past"
	endif
else
	set temp_sp_ampltime="$far_in_the_past"
	set temp_sp_ampl=0
	set last_sp_ampltime="$far_in_the_past"
	set last_sp_ampl=0
	set last_sp_alarm="$far_in_the_past"
endif

set new_lp_alarm=0
set new_sp_alarm=0
set lp_alarm=0
set sp_alarm=0
set tele_alarm=0
if  ("$filter" == "LP")  then
	if  ("$ampltime" != "$temp_lp_ampltime")  then
		set last_lp_ampltime=$temp_lp_ampltime
		set last_lp_ampl=$temp_lp_ampl
	endif
	if  ($ampl > $lp_thresh2 && $last_lp_ampl <= $lp_thresh2)  then
		# check for tele alarm
		set tdiff=`$SH_UTIL/timename time_intdiff $ampltime $last_sp_alarm`
		if  ($tdiff < 3600)  set tele_alarm=1
	endif
	if  ($ampl > $lp_thresh1 && $last_lp_ampl <= $lp_thresh1)  then
		# new alarm
		set new_lp_alarm=1
	endif
	if  ($ampl > $lp_thresh1)  set lp_alarm=1
	if  ($ampl > $lp_thresh2)  set last_lp_alarm=$ampltime
	if  (-e $lphist)  \rm $lphist
	echo "$ampltime $ampl $last_lp_ampltime $last_lp_ampl $last_lp_alarm" \
		>$lphist
endif
if  ("$filter" == "SP")  then
	if  ("$ampltime" != "$temp_sp_ampltime")  then
		set last_sp_ampltime=$temp_sp_ampltime
		set last_sp_ampl=$temp_sp_ampl
	endif
	if  ($ampl > $sp_thresh1 && $last_sp_ampl <= $sp_thresh1)  then
		# new alarm
		set new_sp_alarm=1
	endif
	if  ($ampl > $sp_thresh1)  set sp_alarm=1
	if  ($ampl > $sp_thresh2)  set last_sp_alarm=$ampltime
	if  (-e $sphist)  \rm $sphist
	echo "$ampltime $ampl $last_sp_ampltime $last_sp_ampl $last_sp_alarm" \
		>$sphist
endif



touch $tmpfile
echo "`date -u`" >>$tmpfile
if  ("$filter" == "LP")  then
	echo "$filter     : $ampltime	$ampl	<$lp_alarm,$new_lp_alarm;tele $tele_alarm>" >>$tmpfile
else
	echo "$filter     : $ampltime	$ampl	<$sp_alarm,$new_sp_alarm>" >>$tmpfile
endif
echo "last-lp: $last_lp_ampltime	$last_lp_ampl	($last_lp_alarm)" >>$tmpfile
echo "last-sp: $last_sp_ampltime	$last_sp_ampl	($last_sp_alarm)" >>$tmpfile

# dump data
if  (! -e $dumpfile)  touch $dumpfile
cat $tmpfile >>$dumpfile
echo "" >>$dumpfile

if  ($new_lp_alarm == 1 || $new_sp_alarm == 1 || $tele_alarm == 1)  then
	if  (-e $alhist)  then
		set prev_alarm=`cat $alhist`
	else
		set prev_alarm="$far_in_the_past"
	endif
	if  ("$ampltime" != "$prev_alarm")  then
		$SH_UTIL/online_alarm.csh $tmpfile
		\rm $alhist
		echo $ampltime >$alhist
	endif
endif

if  (-e $tmpfile)  \rm $tmpfile
