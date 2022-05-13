#! /bin/csh
#
# file update_today_sfd.csh
#      ====================
#
# version 5, 26-May-97
#
# updates today's sfd file of directory $DACO
# K. Stammler, 2-Aug-94

if  ("$1" == "" || "$2" == "")  then
	echo "Usage: $0 <date> <outfile>"
	echo "     Example '940731'"
	exit
endif

setenv QUIET_STARTUP
if  (-e /usr/local/common_startup)  source /usr/local/common_startup
unsetenv QUIET_STARTUP

#set echo

set exec_dir=$DPROG
set outfile="$2"
set outfile_x="$outfile.x"
set outfile_y="$outfile.y"

# check date
if  ("$1" == "today")  then
	set currdate=`date +%d-%h-%y`
	set date=`$exec_dir/compute_date $currdate 0`
else
	set date="$1"
endif

set curr_hour=`date +%H`

#if  ("$1" == "today" && $curr_hour >= 0 && $curr_hour <= 2)  then
if  ("$1" == "today")  then
	set prevdate=`$exec_dir/compute_date $currdate \-1`
	$SH_UTIL/sfdlist "*_$date*.bh?" $DACO $outfile_x quiet
	$SH_UTIL/sfdlist "*_$prevdate*.bh?" $DACO $outfile_y quiet
	cat $outfile_y >>$outfile_x
	if  (-e $outfile)  \rm $outfile
	sort $outfile_x >$outfile
	\rm $outfile_x $outfile_y
else
	if  (-e $outfile)  \rm $outfile
	$SH_UTIL/sfdlist "*_$date*.bh?" $DACO $outfile quiet
endif

