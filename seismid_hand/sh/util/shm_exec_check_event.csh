#! /bin/csh
#
# file shm_exec_check_event.csh
#      ========================
#
# version 3, 9-Apr-2003
#
# calls event check routine
# K. Stammler, 21-Jul-94

if  ("$1" == "")  then
	echo "Usage: $0 <evt-file>"
	exit
endif

# get parameters
set evtfile=$1

# set constants
set tmpfile="$SH_SCRATCH/chkapp_$$.000"
set chkout="$SH_SCRATCH/chkout_$$.000"

$SH_UTIL/check_evt $evtfile $chkout

# check for apparent velocities
if  (-e $tmpfile)  \rm $tmpfile
$SH_UTIL/app_velocities.csh $evtfile 3 | grep -v \! >$tmpfile
set res=`grep 'Need at least' $tmpfile`
if  ("$res" == "")  set res=`grep 'cannot be displayed' $tmpfile`
if  ("$res" != "")  then
	\rm $tmpfile
	touch $tmpfile
endif
if  (! -z $tmpfile)  then
	set cnt=0
	while  (1 > 0)  # forever
		@ cnt = $cnt + 1
		set line=`sed -n $cnt"p" $tmpfile`
		if  ("$line" == "")  break
		if  ($#line < 5)  continue
		set phase=$line[1]
		set appvel=$line[3]
		set appv=`echo "$appvel * 10.0" | bc`
		set appv=`printf "%3.0f" $appv`
		if  ("$phase" == "Pg")  then
			if  ($appv < 55 || $appv > 68)  then
				echo "Pg apparent velocity is $appvel km/s, please check" >>$chkout
			endif
		else if  ("$phase" == "Sg")  then
			if  ($appv < 32 || $appv > 40)  then
				echo "Sg apparent velocity is $appvel km/s, please check" >>$chkout
			endif
		else if  ("$phase" == "Pn")  then
			if  ($appv < 70 || $appv > 90)  then
				echo "Pn apparent velocity is $appvel km/s, please check" >>$chkout
			endif
		else if  ("$phase" == "Sn")  then
			if  ($appv < 40 || $appv > 52)  then
				echo "Pn apparent velocity is $appvel km/s, please check" >>$chkout
			endif
		else if  ("$phase" == "P")  then
			if  ($appv < 90 || $appv > 280)  then
				echo "P apparent velocity is $appvel km/s, please check" >>$chkout
			endif
		endif
	end
endif
\rm $tmpfile

# $SH_UTIL/check_for_event_archived.csh $evtfile >>$chkout

if  (-z $chkout)  then
	# echo "check file $chkout is zero"
else
	$SH_TEXTEDIT $chkout
endif

\rm $chkout
