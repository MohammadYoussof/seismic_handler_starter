#! /bin/csh
#
# file sfd_from_sfdb.csh
#      =================
#
# version 2, 23-Feb-7
#
# Create sfdfile (db type) for given time window
# K. Stammler, 6-Feb-7

if  ("$2" == )  then
	echo "Usage: $0 <stime> <etime>"
	exit
endif

# get parameters
set starttime=$1
set endtime=$2

# create sdate & stime for database
set res=`$SH_UTIL/timename time_to_int $starttime`
if  ($#res < 7)  then
	echo "Illegal time format in $starttime."
	exit
endif
@ year = $res[1]
@ month = $res[2]
@ day = $res[3]
@ hour = $res[4]
@ min = $res[5]
@ sec = $res[6]
@ ms = $res[7]
set sdate=`printf "%4d%02d%02d" $year $month $day`
set stime=`printf "%02d%02d%02d.%03d" $hour $min $sec $ms`
set res=`$SH_UTIL/timename time_to_int $endtime`
if  ($#res < 7)  then
	echo "Illegal time format in $endtime."
	exit
endif
@ year = $res[1]
@ month = $res[2]
@ day = $res[3]
@ hour = $res[4]
@ min = $res[5]
@ sec = $res[6]
@ ms = $res[7]
set edate=`printf "%4d%02d%02d" $year $month $day`
set etime=`printf "%02d%02d%02d.%03d" $hour $min $sec $ms`

$SH_UTIL/sol_sql_call.csh $SFDBHOST "select * from sftab where ((sdate = $edate and stime <= $etime ) or sdate < $edate) and ((edate = $sdate and etime >= $stime) or edate > $sdate)"
