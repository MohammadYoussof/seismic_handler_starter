#! /bin/csh
#
# file al_pick_statistics.csh
#      ======================
#
# version 1, 24-Mar-2001
#
# Returns quality describing numbers of a relative onset file
# K. Stammler, 24-Mar-2001

if  ("$2" == "")  then
	echo "Usage: $0 <mode> <pickfile>"
	exit
endif

# get parameters
set mode=$1
set pfile=$2

if  (! -e $pfile)  then
	echo "$0 : pickfile $pfile not found.  Abort."
	exit
endif

if  ("$mode" == "all")  then
	set cmd="grep -v \!"
else
	set cmd="grep -e ^BUG -e ^CLL -e ^FUR -e ^TNS -e ^MOX -e ^WET"
	if  ("`uname`" == "SunOS")  set cmd="/usr/xpg4/bin/$cmd"
endif

set res=`$cmd $pfile | awk '{print $3}' | grep -v 1.000000 | $DPROG/stddev -`
set meancorr=$res[5]
set devcorr=$res[7]

set res=`$cmd $pfile | awk '{print $4}' | $DPROG/stddev -`
set meansnr=$res[5]
set devsnr=$res[7]

set res=`$SH_UTIL/relonset2slo -resid $pfile | grep '^[A-Z]' | awk '{print $2}' | $DPROG/stddev -`
set meanres=$res[5]
set devres=$res[7]

echo $meancorr $devcorr $meansnr $devsnr $meanres $devres $pfile
