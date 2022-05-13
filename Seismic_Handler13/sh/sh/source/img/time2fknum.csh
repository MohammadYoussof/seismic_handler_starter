#! /bin/csh
#
# file time2fknum.csh
#      ==============
#
# version 1, 8-Oct-2001
#
# Compute time slice number
# K. Stammler, 8-Oct-2001

if  ("$1" == "")  then
	echo "Usage: $0 <time>"
	exit
endif

set timestr=`echo $1 | sed 's/:/ /'`
if  ($#timestr != 2)  then
	echo "Illegal parameter $1.  Abort."
	exit
endif

set hour=$timestr[1]
set min=$timestr[2]

@ num = (($hour * 3600) + ($min * 60)) / 5

echo $num
