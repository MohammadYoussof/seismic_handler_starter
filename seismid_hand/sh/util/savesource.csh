#! /bin/csh
#
# file savesource.csh
#      ==============
#
# version 6, 30-Jan-2007
#
# Saves source files to tar archive
# K. Stammler, 12-Nov-2004

if  ("$1" == "")  then
	echo "Usage: $0 <tarfile>"
	exit
endif

# get parameters
set tarfile=$1

set curd=$PWD

chdir $SH_ROOT

set clist=`find . -name \*.c -print`
set hlist=`find . -name \*.h -print`
set mlist=`find . -name Makefile -print`
set ulist=`find . -name \*.uil -print`
set slist=`find . -name \*.csh -print`
set ylist=`find . -name \*.py -print`
set dlist=`find . -name \*.html -print`
set elist=`find . -name ERR_\*.MSG -print`
set tlist=`find . -name shm-conf\*.txt`
set xlist = ( \
	./doc/gpl.txt ./command/MERGE_ALL.SHC ./command/READGSE_MERGE.SHC \
	./help/SPECTROGRAM.HLP ./help/MAXAMPL.HLP util/sfdb/create_tables.s \
)


tar cvf $tarfile $clist $hlist $mlist $ulist $slist $ylist $elist $dlist $tlist $xlist
echo ""
echo "bzipping ..."
if  (-e $tarfile.bz2)  \rm $tarfile.bz2
bzip2 $tarfile

cd $curd
if  (! -e $tarfile.bz2)  mv $SH_ROOT/$tarfile.bz2 .
