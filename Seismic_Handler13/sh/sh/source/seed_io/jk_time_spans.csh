#! /bin/csh
#
# file jk_time_spans.csh
#      =================
#
# version 8, 16-Dec-2004
#
# Returns available time spans of stream sets
# K. Stammler, 12-Aug-96

# set constants

#set echo

# get parameters
if  ("$1" == "")  then
	set descrlist = ( grf-bh grsn-bh grsn-lh )
	set prefixlist = ( grf grsn lp )
else
	set descrlist = $1
	if  ("$descrlist" == "grf-bh")  then
		set prefixlist = ( grf )
	else if  ("$descrlist" == "grsn-bh")  then
		set prefixlist = ( grsn )
	else
		set prefixlist = ( lp )
	endif
endif

set cnt=1
foreach prefix ($prefixlist)
#	set first=`(\ls /juke1pc; \ls /juke2pc; ls /r06p1/datalib) | grep ^$prefix | sort | head -1`
	set first=`(\ls /r06p2/datalib; ls /r06p1/datalib) | grep ^$prefix | sort | head -1`
	if  ("$prefix" == "grf")  then
		# since 2003, GRF data are on GRSN DVDs
		#set last=`(\ls /juke1pc; \ls /juke2pc; ls /r06p1/datalib) | grep ^grsn | sort | tail -1`
		set last=`(\ls /r06p2/datalib; ls /r06p1/datalib) | grep ^grsn | sort | tail -1`
	else
		#set last=`(\ls /juke1pc; \ls /juke2pc; ls /r06p1/datalib) | grep ^$prefix | sort | tail -1`
		set last=`(\ls /r06p2/datalib; ls /r06p1/datalib) | grep ^$prefix | sort | tail -1`
	endif
	set info=`$DPROG/cdarch/cd_info.csh $first`
	if  ("$info" == "")  then
		echo "invalid CD $first in script $0"
		exit
	endif
	set start_time=$info[1]
	set info=`$DPROG/cdarch/cd_info.csh $last`
	if  ("$info" == "")  then
		#set last=`(\ls /juke1pc; \ls /juke2pc; ls /r06p1/datalib) | grep ^$prefix | sort -u | tail -2 | head -1`
		set last=`(\ls /r06p2/datalib; ls /r06p1/datalib) | grep ^$prefix | sort -u | tail -2 | head -1`
		set info=`$DPROG/cdarch/cd_info.csh $last`
		if  ("$info" == "")  then
			echo "invalid CD $last in script $0"
			exit
		endif
	endif
	set end_time=$info[2]
	echo "$descrlist[$cnt] $start_time $end_time"
	@ cnt = $cnt + 1
end

