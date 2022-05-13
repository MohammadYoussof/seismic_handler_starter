#! /bin/csh
#
# file gfdlist.csh
#      ===========
#
# version 1, 20-Oct-2003
#
# creates gfd file using gfdline
# K. Stammler, 20-Oct-2003

set rootdir='.'
set wildcard=\*
set outfile="sfdfile.sfd"
set reclth=""

#set echo

if  ("$1" == "-h")  then
	echo "Usage: $0 [<wild>] [<rootpath>] [<outfile>] [<quiet>] [<reclth>]"
	exit
endif

if  ("$1" != "")  set wildcard="$1"
if  ("$2" != "")  set rootdir="$2"
if  ("$3" != "")  set outfile="$3"
set quiet="$4"
if  ("$5" != "")  set reclth="$5"

set scrfile=$outfile.x.$$

set flist=`find $rootdir -name "$wildcard" -type f -print`

if  ("$reclth" == "")  then
	set q1=""
else
	set q1="-gcfrec=$reclth"
endif

if  (-e $scrfile)  \rm $scrfile
touch $scrfile
foreach i ($flist)
	if  (! -z $i)  then
		if  ("$4" != "quiet")  echo "processing file $i"
		$SH_SOURCE/gcf/gfdline $q1 $i >>$scrfile
	endif
end

if  (-e $outfile)  \rm $outfile
sort $scrfile >$outfile
\rm $scrfile
