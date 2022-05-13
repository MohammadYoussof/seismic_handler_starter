#! /bin/csh
#
# file extract_ah.csh
#      ==============
#
# version 1, 1-mar-95
#
# extracts AH files from SEED volume using rdseed
# K. Stammler, 1-mar-95

if  ("$1" == "" || "$2" == "" || "$3" == "" || "$4" == "")  then
	echo "Usage: $0 <file> <station(s)> <channel(s)> <filelist> [<start> <end>]"
	exit
endif

setenv QUIET_STARTUP
if  (-e /usr/local/common_startup)  source /usr/local/common_startup
unsetenv QUIET_STARTUP

# get parameters
set seedfile="$1"
set station="$2"
set channel="$3"
set filelist="$4"
set starttime="$5"
set endtime="$6"

# set paths
set inpfile=$HOME/extract_ah_$$.000
set logfile=$HOME/extract_ah_$$.log
set d1file=$HOME/extract_ah_$$.d1
set d2file=$HOME/extract_ah_$$.d2

if  (-e $inpfile)  \rm $inpfile
if  (-e $logfile)  \rm $logfile
if  (-e $d1file)  \rm $d1file
if  (-e $d2file)  \rm $d2file
if  (-e $filelist)  \rm $filelist

touch $inpfile
echo "$seedfile"   >>$inpfile
echo ""            >>$inpfile
echo "1"           >>$inpfile
echo "d"           >>$inpfile
echo "$station"    >>$inpfile
echo "$channel"    >>$inpfile
echo "2"           >>$inpfile
echo "0"           >>$inpfile
echo "$starttime"  >>$inpfile
echo "$endtime"    >>$inpfile
echo ""            >>$inpfile
echo "n"           >>$inpfile
echo "quit"        >>$inpfile

ls *.AH >$d1file

rdseed <$inpfile >$logfile
\rm rdseed.err_log.*

ls *.AH >$d2file
diff $d1file $d2file | grep AH | awk '{print $2}' >$filelist

\rm $inpfile $logfile $d1file $d2file

