#! /bin/csh
#
# file autoftp_dir.csh
#      ===============
#
# version 1, 11-Dec-98
#
# Lists adirectory of a specified ftp-server.  Login and
# password must be specified in the .netrc file
#
# K. Stammler, 11-Dec-98

if  ("$3" == "")  then
	echo "Usage: <dir/ls> <host> <path> [<wild>]"
	exit
endif

#set echo

# get parameters
set dircmd=$1
set rhost=$2
set rpath=$3
set wild="$4"

if  ("$dircmd" != "dir" && "$dircmd" != "ls")  then
	echo "Illegal dir/ls keyword.  Abort."
	exit
endif

# set constants
set tmpfile=$HOME/autoftp_$$.000

if  (-e $tmpfile)  \rm $tmpfile
touch $tmpfile
echo "cd $rpath"        >>$tmpfile
echo "$dircmd $wild" | sed 's/\@/\*/g'   >>$tmpfile
echo "bye"              >>$tmpfile

ftp $rhost <$tmpfile

\rm $tmpfile
