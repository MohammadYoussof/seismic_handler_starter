#! /bin/csh
#
# file autoftp.csh
#      ===========
#
# version 1, 23-Aug-96
#
# Copies a file in binary mode from a specified ftp-server.  Login and
# password must be specified in the .netrc file
#
# K. Stammler, 23-Aug-96

if  ("$4" == "")  then
	echo "Usage: <get/put> <host> <path> <file>"
	exit
endif

# get parameters
set getput=$1
set rhost=$2
set rpath=$3
set rfile=$4

if  ("$getput" != "get" && "$getput" != "put")  then
	echo "Illegal get/put keyword.  Abort."
	exit
endif

# set constants
set tmpfile=$HOME/autoftp_$$.000

if  (-e $tmpfile)  \rm $tmpfile
touch $tmpfile
echo "cd $rpath"        >>$tmpfile
echo "bin"              >>$tmpfile
echo "$getput $rfile"   >>$tmpfile
echo "bye"              >>$tmpfile

ftp $rhost <$tmpfile

\rm $tmpfile
