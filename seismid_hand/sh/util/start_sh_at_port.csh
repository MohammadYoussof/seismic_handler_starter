#! /bin/csh
#
# file start_sh_at_port.csh
#      ====================
#
# version 1, 9-Jul-2006
#
# Starts SH listening at given port
# K. Stammler, 9-Jul-2006

if  ("$2" == "")  then
	echo "Usage: $0 <port> <hash>"
	exit
endif

# get parameters
set portno=$1
set hash=$2

set logf=$SH_SCRATCH/shc_hash_$hash.log
if  (-e $logf)  \rm $logf

setenv SH_READ_PORT $portno
$SH_ROOT/shc #>& $logf
