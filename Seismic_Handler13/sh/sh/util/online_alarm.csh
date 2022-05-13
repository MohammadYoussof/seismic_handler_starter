#! /bin/csh
#
# file online_alarm.csh
#      ================
#
# version 1, 9-Sep-99
#
# Sends alarm message.  The message is read from a file, the message file
# is deleted after displaying it.
# K. Stammler, 9-Sep-99

# set constants
set outfile=$HOME/online_alarm.txt

if  ("$1" == "" || ! -e $1)  then
	set f_alarm=$HOME/online_alarm_$$.000
	if  (-e $f_alarm)  \rm $f_alarm
	touch $f_alarm
	echo ""  >>$f_alarm
	echo ""  >>$f_alarm
	echo "$0 : input file $1 not found."   >>$f_alarm
else
	set f_alarm=$1
endif

# open window
textedit -En 7 -display ersn12:0 -bg yellow $f_alarm &
#textedit -En 7 -display ersn03:0 -bg yellow $f_alarm &
sleep 7

# write to file
#if  (! -e $outfile)  touch $outfile
#cat $f_alarm >>$outfile
#echo "" >>$outfile


\rm $f_alarm
