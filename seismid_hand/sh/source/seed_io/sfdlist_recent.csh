#! /bin/csh
#
# file sfdlist_recent.csh
#      ==================
#
# version 1, 16-Aug-95
#
# creates sfd file on recent files using sfdline
# K. Stammler, 16-Apr-95

set rootdir='.'
set wildcard=\*
set outfile="sfdfile.sfd"

#set echo

if  ("$1" == "help" || "$1" == "-help")  then
	echo "Usage: $0 [<rootdir>] [<outfile>] [<quiet>]"
	exit
endif
if  ("$1" != "")  set rootdir="$1"
if  ("$2" != "")  set outfile="$2"

# get wildcards
set today_str=`date +%d,%m,%y`
set today=`date +%y%m%d`
set yesterday_str=`$SH_UTIL/timename time_subsec $today_str 86400`
set yesterday=`$SH_UTIL/timename time_digit6 $yesterday_str`

set wild1="*$today*.?h?"
set wild2="*$yesterday*.?h?"

set scrfile=$outfile.x

set flist=`find $rootdir \( -name "$wild1" -o -name "$wild2" \) -type f -print`

if  (-e $scrfile)  \rm $scrfile
touch $scrfile
foreach i ($flist)
	if  ("$3" != "quiet")  echo "processing file $i"
	$SEED_PROG/sfdline $i >>$scrfile
end

if  (-e $outfile)  \rm $outfile
sort $scrfile >$outfile
\rm $scrfile
