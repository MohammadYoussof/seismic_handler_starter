#! /bin/csh
#
# file get_latest_emsc_events.csh
#      ==========================
#
# version 1, 1-Oct-2005
#
# Returns latest event pages from EMSC
# K. Stammler, 1-Oct-2005

if  ("$2" == "")  then
	echo "Usage: $0 <outfile> <numpages>"
	exit
endif

# get parameters
set outfile=$1
set numpag=$2

# set constants
set emsc_page="http://www.emsc-csem.org/cgi-bin/QDM_all.sh"
set tmppre=$SH_SCRATCH/emsc_$$_

set cnt=1
while  ($cnt <= $numpag)
	if  (-e $tmppre$cnt.000)  \rm $tmppre$cnt.000
	$SH_UTIL/get_html_text.csh "$emsc_page?$cnt" $tmppre$cnt.000
	sleep 3
	@ cnt = $cnt + 1
end

if  (-e $outfile)  \rm $outfile
cat $tmppre*.000 | sed 's/^\[..\]//' | sed 's/^\[...\]//' | grep "^..../../" >$outfile
\rm $tmppre*.000
