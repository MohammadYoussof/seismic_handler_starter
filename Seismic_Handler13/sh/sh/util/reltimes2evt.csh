#! /bin/csh
#
# file reltime2evt.csh
#      ===============
#
# version 1, 6-Feb-2001
#
# Converts relative times to abolute picks in evt-format
# K. Stammler, 6-Feb-2001

if  ("$2" == "")  then
	echo "Usage: $0 <reltimes> <abstime> [<phase>]"
	exit
endif

# get parameters
set reltimes=$1
set abstime=$2
set phase=$3

if  ("$phase" == "")  set phase="P"

if  (! -e $reltimes)  then
	echo "Input file $reltimes not found.  Abort."
	exit
endif

set cnt=1
while  (1 > 0)  # forever
	set line=`sed -n $cnt"p" $reltimes`
	if  ("$line" == "")  break
	@ cnt = $cnt + 1
	if  ($#line < 3)  continue
	if  ("$line[1]" == "\!")  continue
	set rtime=`echo $line[2] | sed 's/-/m/'`
	set res=`$DPROG/floatop $rtime lt 0.0`
	set rtime=`echo $rtime | sed 's/m//'`
	echo "Station code           : $line[1]"
	echo "Component              : Z"
	if  ($res == 1)  then
		set atime=`$SH_UTIL/timename time_subsec $abstime $rtime`
	else
		set atime=`$SH_UTIL/timename time_addsec $abstime $rtime`
	endif
	echo "Onset time             : $atime"
	echo "Phase name             : $phase"
	echo "--- End of Phase ---"
	echo ""
end

