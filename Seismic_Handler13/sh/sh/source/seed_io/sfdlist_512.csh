#! /bin/csh
#
# file sfdlist_512.csh
#      ===============
#
# version 1, 10-Aug-98
#
# creates sfd file for 512-byte-record files using sfdline
# K. Stammler, 10-Jul-98

#set echo

set rootdir='.'
set wildcard=\*
set outfile="sfdfile.sfd"
set scrfile=~/sfd_scr_$$.000

if  ("$1" != "")  set wildcard="$1"
if  ("$2" != "")  set rootdir="$2"
if  ("$3" != "")  set outfile="$3"

set oscrfile=x.$$.$outfile

if  (-e $scrfile)  \rm $scrfile
find $rootdir -name "$wildcard" -type f -print >$scrfile

if  (-e $oscrfile)  \rm $oscrfile
touch $oscrfile
set cnt=1
loop_start:
	set cfile=`sed -n $cnt"p" $scrfile`
	if  ("$cfile" == "")  goto loop_exit
	if  (! -z $cfile)  then
		if  ("$4" != "quiet")  echo "processing file $cfile"
		$SH_UTIL/sfdline -seedrec=512 -byteoff=512 $cfile >>$oscrfile
	endif
	@ cnt = $cnt + 1
goto loop_start
loop_exit:

if  (-e $outfile)  \rm $outfile
sort $oscrfile >$outfile

\rm $scrfile $oscrfile
