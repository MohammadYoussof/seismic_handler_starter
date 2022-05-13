#! /bin/csh
#
# file cgfdlist_l.csh
#      =============
#
# version 1, 20-Oct-2003
#
# creates gfd file using gfdline
# K. Stammler, 20-Oct-2003

#set echo

set rootdir='.'
set wildcard=\*
set outfile="sfdfile.sfd"
set scrfile=~/sfd_scr_$$.000
set reclth=""

if  ("$1" != "")  set wildcard="$1"
if  ("$2" != "")  set rootdir="$2"
if  ("$3" != "")  set outfile="$3"
set quiet="$4"
if  ("$5" != "")  set reclth="$5"

set oscrfile=x.$$.$outfile

if  (-e $scrfile)  \rm $scrfile
find $rootdir -name "$wildcard" -type f -print >$scrfile

if  ("$reclth" == "")  then
	set q1=""
else
	set q1="-seedrec=$reclth"
endif

if  (-e $oscrfile)  \rm $oscrfile
touch $oscrfile
set cnt=1
loop_start:
	set cfile=`sed -n $cnt"p" $scrfile`
	if  ("$cfile" == "")  goto loop_exit
	if  (! -z $cfile)  then
		if  ("$quiet" != "quiet")  echo "processing file $cfile"
		$SH_SOURCE/gcf/gfdline $q1 $cfile >>$oscrfile
	endif
	@ cnt = $cnt + 1
goto loop_start
loop_exit:

if  (-e $outfile)  \rm $outfile
sort $oscrfile >$outfile

\rm $scrfile $oscrfile
