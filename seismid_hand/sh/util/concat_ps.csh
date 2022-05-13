#! /bin/csh
#
# file concat_ps.csh
#      =============
#
# version 1, 13-Sep-94
#
# concatenates PS files
# K. Stammler, 13-Sep-94

if  ("$1" == "")  then
	echo "Usage: $0 <ps-list>"
	exit
endif

set pscnt=1
set psout=psout1.ps
if  (-e $psout)  \rm $psout
touch $psout
set taillines
set headlines
set test=1

#set echo

set firstfile=`sed -n 1p $1`
head -23 $firstfile >> $psout

set cnt = 1
loop_start:
	set cfile=`sed -n $cnt"p" $1`
	if  ("$cfile" == "")  goto loop_exit
	if  ($test == 0)  then
		echo "endpage" >>$psout
		#pageview $psout &
		lpr -s $psout
		@ pscnt = $pscnt + 1
		set psout="psout"$pscnt".ps"
		if  (-e $psout)  \rm $psout
		touch $psout
		head -23 $firstfile >>$psout
	endif
	echo "merge $cfile"
	set lines=`wc -l $cfile`
	set lines=$lines[1]
	@ lines = $lines - 2
	set sedcmd="24,$lines l"
	sed -n "$sedcmd" $cfile >> $psout
	@ test = $cnt % 12
	@ cnt = $cnt + 1
goto loop_start
loop_exit:

echo "endpage" >>$psout
#pageview $psout &
lpr -s -P$SYSPRINT $psout

