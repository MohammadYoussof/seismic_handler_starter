#! /bin/csh
#
# file sfdlist.csh
#      ===========
#
# version 8, 9-Dec-99
#
# creates sfd file using sfdline
# K. Stammler, 8-Apr-94
# version 4, 17-Nov-94, K. Stammler: included '-type f' to find command

set rootdir='.'
set wildcard=\*
set outfile="sfdfile.sfd"
set reclth=""
set byteoff=""

#set echo

if  ("$1" == "-h")  then
	echo "Usage: $0 [<wild>] [<rootpath>] [<outfile>] [<quiet>] [<reclth>] [<byteoff>]"
	exit
endif

if  ("$1" != "")  set wildcard="$1"
if  ("$2" != "")  set rootdir="$2"
if  ("$3" != "")  set outfile="$3"
set quiet="$4"
if  ("$5" != "")  set reclth="$5"
if  ("$6" != "")  set byteoff="$6"

set scrfile=$outfile.x.$$

set flist=`find $rootdir -name "$wildcard" -type f -print`

if  ("$reclth" == "")  then
	set q1=""
else
	set q1="-seedrec=$reclth"
endif
if  ("$byteoff" == "")  then
	set q2=""
else
	set q2="-byteoff=$byteoff"
endif

if  (-e $scrfile)  \rm $scrfile
touch $scrfile
foreach i ($flist)
	if  (! -z $i)  then
		if  ("$4" != "quiet")  echo "processing file $i"
		$SEED_PROG/sfdline $q1 $q2 $i >>$scrfile
	endif
end

if  (-e $outfile)  \rm $outfile
sort $scrfile >$outfile
\rm $scrfile
