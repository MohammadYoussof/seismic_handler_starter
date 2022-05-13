#! /bin/csh
#
# file get_latest_emsc_pages.csh
#      =========================
#
# version 1, 9-Apr-2006
#
# Returns latest event pages from EMSC
# <onset> contains the phase in question.  If it is too far back in time
# just create an empty output file and return to save time.
# K. Stammler, 11-May-2004

if  ("$1" == "")  then
	echo "Usage: $0 <outfile> [<onset>]"
	exit
endif

# get parameters
set outfile=$1
set onset=$2

# set constants
#set emsc_page="http://www.emsc-csem.org/cgi-bin/ALERT_all_messages.sh?"
set emsc_page="http://www.emsc-csem.org/index.php?page=current&sub=msg&view="
set tmppre=$SH_SCRATCH/emsc_$$_
set numfiles=15

if  (-e $outfile)  \rm $outfile

if  ("$onset" != "")  then
	set today=`date +%d-%h-%Y`
	set diff=`$SH_UTIL/timename time_daydiff $today $onset`
	if  ($diff > 365)  then
		echo "$0 : event too old.  EMSC not searched."
		touch $outfile
		exit
	endif
endif

set cnt=1
while  ($cnt <= $numfiles)
	if  (-e $tmppre$cnt.000)  \rm $tmppre$cnt.000
	$SH_UTIL/get_html_text.csh "${emsc_page}$cnt" $tmppre$cnt.000
	sleep 1
	@ cnt = $cnt + 1
end

cat $tmppre*.000 | $SH_UTIL/lynx_reformat - \
	| grep "^20" | grep -v "Moment tensors" >$outfile
\rm $tmppre*.000
