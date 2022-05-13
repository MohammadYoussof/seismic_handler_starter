#! /bin/csh
#
# file vrs_tarfiles.csh
#      ================
#
# version 1, 14-Dec-97
#
# Makes tar-archive with new files of local SH-installation.  Writes tar-file
# on HOME-directory named after 'oldlist'.
# K. Stammler, 14-Dec-97

if  ("$2" == "")  then
	echo "$0 <remote-vrslist> <local-vrslist>"
	exit
endif

# get parameters
set oldlist=$1
set newlist=$2

# check parameters
if  (! -e $oldlist)  then
	echo "$0 : Input file $oldlist not found.  Abort."
	exit
endif
if  (! -e $newlist)  then
	echo "$0 : Input file $newlist not found.  Abort."
	exit
endif

# set constants
set tmpfile=$HOME/vrs_tarfiles_$$.000

if  (-e $tmpfile)  \rm $tmpfile
diff $oldlist $newlist >$tmpfile
set tarlist=`$SH_UTIL/vrs_read_diff.csh $tmpfile`
set tarfile=$oldlist:t
set tarfile=$tarfile:r
set tarfile=$HOME/$tarfile.tar
chdir $SH_ROOT
tar cvf $tarfile $tarlist
echo "Created tar-file $tarfile"
chdir $HOME
echo "Compressing ..."
gzip $tarfile

\rm $tmpfile

