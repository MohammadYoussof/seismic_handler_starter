#! /bin/csh
#
# file makemd5.csh
#      ===========
#
# version 1, 26-Oct-2006
#
# Creates md5 checksum file on current directory
# K. Stammler, 26-Oct-2006

if  (! -w /tmp)  then
	echo "cannot write to /tmp.  Abort."
	exit
else if  (! -w $PWD)  then
	echo "cannot write to $PWD.  Abort"
	exit
endif

set tmpfile=/tmp/mkmd5_$$
find . -type f >$tmpfile

set md5=checksum.md5
if  (-e $md5)  \rm $md5
touch $md5

set cnt=1
while  (1 > 0)
	set f=`sed -n $cnt"p" $tmpfile`
	if  ("$f" == "")  break
	md5sum $f >>$md5
	@ cnt ++
end

\rm $tmpfile
