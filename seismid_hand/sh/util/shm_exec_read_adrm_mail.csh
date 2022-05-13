#! /bin/csh
#
# file shm_exec_read_adrm_mail.csh
#      ===========================
#
# version 1, 15-Jan-2002
#
# Extracts AutoDRM mail from mail file
# K. Stammler, 15-Jan-2002

if  ("$4" == "")  then
	echo "Usage: $0 <msgid> <mailfile> <waittime> <outfile> [<mailhost>]"
	exit
endif

#set echo

# get parameters
set msgid="$1"
set mailfile=$2
set wait=$3
set outfile=$4
set mailhost=$5

# set constants
set tmpfile=$SH_SCRATCH/mr-$$.000

if  ("$mailhost" == "")  then
	if  (! -e $mailfile)  then
		echo "$0 : mailfile $mailfile not found.  Abort."
		exit
	endif
endif

# wait for 'wait' loops for the message to arrive
set wcnt=0
while  (1 > 0)

	if  (-e $outfile)  \rm $outfile

	@ wcnt = $wcnt + 1
	if  ($wcnt > $wait)  then
		echo "AutoDRM response mail not found.  Automatic read not possible."
		if  (-e $outfile)  \rm $outfile
		exit
	endif

	echo "waiting for 15 seconds ($wcnt) ..."
	sleep 15

	if  ("$mailhost" == "")  then
		set res=`mailx -f $mailfile -H | grep -i autodrm | tail -1`
	else
		set res=`ssh $mailhost mailx -f $mailfile -H | grep -i autodrm | tail -1`
	endif
	if  ($#res < 3)  continue

	set num=$res[2]
	if  (-e $tmpfile)  \rm $tmpfile
	touch $tmpfile
	echo "$num"  >>$tmpfile
	echo "q"     >>$tmpfile
	if  ("$mailhost" == "")  then
		mailx -f $mailfile -N <$tmpfile >$outfile
	else
		ssh $mailhost mailx -f $mailfile -N <$tmpfile >$outfile
	endif
	\rm $tmpfile

	set res=`grep "MSG_ID $msgid SHMREQ" $outfile`
	if  ("$res" == "")  then
		set res=`grep "REF_ID $msgid SHMREQ" $outfile`
	endif
	if  ("$res" == "")  then
		echo "wrong file found, delete output file"
		\rm $outfile
	else
		# output file created and now exit to read
		exit
	endif

end
