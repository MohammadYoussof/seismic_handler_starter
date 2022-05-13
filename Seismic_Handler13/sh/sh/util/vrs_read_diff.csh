#! /bin/csh
#
# file vrs_read_diff.csh
#      =================
#
# version 1, 14-Dec-97
#
# Reads difference file of version lists ('diff oldlist newlist') and writes
# list of updated files in 'newlist' to stdout.
# K. Stammler, 14-Dec-97

if  ("$1" == "")  then
	echo "Usage: $0 <difflist>"
	exit
endif

#set echo

# get parameters
set difflist=$1

# check parameters
if  (! -e $difflist)  then
	echo "$0 : Input file $difflist not found.  Abort."
	exit
endif

# set constants
set tmpfile=$HOME/vrs_read_diff_$$.000

if  (-e $tmpfile)  \rm $tmpfile

set noglob
grep "^>" $difflist | sed 's/,/ /g' >$tmpfile

# loop all lines
set nlist = ( )
set vlist = ( )
set cnt=1
while  (1 > 0)  # forever
	set line=`sed -n $cnt"p" $tmpfile`
	if  ("$line" == "")  break
	if  ($#line < 5)  continue
	set fname=$line[2]
	set version=$line[5]
	set res=`echo $version | grep '[^0123456789]'`
	if  ("$res" == "")  then
		set nlist = ( $nlist $fname )
		set vlist = ( $vlist $version )
	endif
	@ cnt = $cnt + 1
end

if  ($#nlist != $#vlist)  then
	echo "$0 : unexpected error.  Abort."
	exit
endif

# loop all files
set cnt=1
foreach fname ($nlist)
	set version=$vlist[$cnt]
	set res=`grep "^<" $difflist | grep $fname | sed 's/,/ /g'`
	if  ("$res" == "")  then
		echo $fname
	else
		if  ($#res < 5)  continue
		set ofname=$res[2]
		set oversion=$res[5]
		if  ($oversion < $version)  echo $fname
	endif
	@ cnt = $cnt + 1
end

\rm $tmpfile

