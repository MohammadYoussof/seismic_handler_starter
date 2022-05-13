#! /bin/csh
#
# file archevent.csh
#      =============
#
# version 1, 2-Mar-95
#
# extracts event data to archive directory $EVARCH
# K. Stammler, 2-Mar-95

if  ("$1" == "")  then
	echo "Usage: $0 <evt-file>"
	exit
endif

setenv QUIET_STARTUP
if  (-e /usr/local/common_startup)  source /usr/local/common_startup
unsetenv QUIET_STARTUP

if  ("$EVARCH" == "")  then
	echo "event archive path EVARCH not defined"
	exit
endif

# set paths
set evtfile="$1"
set cmdfile="EVARCH_$$"
set inpfile="evarch_inp_$$.000"
set logfile="evarch_log_$$.000"

chdir $EVARCH

if  (-e $inpfile)  \rm $inpfile
if  (-e $logfile)  \rm $logfile

$SH_UTIL/archevent_shc $evtfile $cmdfile".SHC"
touch $inpfile
echo "$cmdfile"  >>$inpfile
echo "quit y"    >>$inpfile
SH <$inpfile >$logfile

\rm $inpfile $logfile $cmdfile".SHC"

cp $evtfile $EVARCH
