#! /bin/csh
#
# file sfdlist_l.csh
#      =============
#
# version 8, 12-Sep-2005
#
# creates sfd file using sfdline
# K. Stammler, 29-Jul-94
# version 3, 17-Nov-94, K. Stammler: included '-type f' to find command

#set echo

set rootdir='.'
set wildcard=\*
set outfile="sfdfile.sfd"
set scrfile=~/sfd_scr_$$.000
set reclth=""
set byteoff=""

if  ("$1" != "")  set wildcard="$1"
if  ("$2" != "")  set rootdir="$2"
if  ("$3" != "")  set outfile="$3"
set quiet="$4"
if  ("$5" != "")  set reclth="$5"
if  ("$6" != "")  set byteoff="$6"

set oscrfile=x.$$.$outfile

if  (-e $scrfile)  \rm $scrfile
find $rootdir -name "$wildcard" -type f -print >$scrfile

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

if  (-e $oscrfile)  \rm $oscrfile
touch $oscrfile
set cnt=1
loop_start:
	set cfile=`sed -n $cnt"p" $scrfile`
	if  ("$cfile" == "")  goto loop_exit
	if  (! -z $cfile)  then
		if  ("$quiet" != "quiet")  echo "processing file $cfile"
		$SEED_PROG/sfdline $q1 $q2 $cfile >>$oscrfile
	endif
	@ cnt = $cnt + 1
goto loop_start
loop_exit:

if  (-e $outfile)  \rm $outfile
sort $oscrfile >$outfile

\rm $scrfile $oscrfile
