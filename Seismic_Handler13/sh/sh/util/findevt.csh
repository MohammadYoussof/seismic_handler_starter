#! /bin/csh
#
# file find_evt.csh
#      ============
#
# version 1, 7-Jan-2004
#
# Tries to find evt-file with phases withing given window
# K. Stammler, 7-Jan-2004

# assume parameter file $SH_SCRATCH/findevt.txt (input)
# copy evtfile with phases (if any) to $SH_SCRATCH/findevt.evt

set wfile=$SH_SCRATCH/findevt.txt
set ofile=$SH_SCRATCH/findevt.evt

# set constants
set evtpath=/unsafe/b311sdac/BGRevt

if  (! -e $wfile)  then
	echo "$0 : cannot find input file
endif

if  (-e $ofile)  \rm $ofile

set stime=`head -1 $wfile`
set wlth=`tail -1 $wfile`
set etime=`$SH_UTIL/timename time_addsec $stime $wlth`
set aday=`$SH_UTIL/timename time_digit6 $stime`
set eday=`$SH_UTIL/timename time_digit6 $etime`
set tmp=`echo $aday | cut -c1-1`
if  ("$tmp" == "0")  then
	if  ("$aday" == "$eday")  then
		set dlist = ( 1$aday )
	else
		set dlist = ( 1$aday 1$eday )
	endif
else
	if  ("$aday" == "$eday")  then
		set dlist = ( $aday )
	else
		set dlist = ( $aday $eday )
	endif
endif

set elist = ( )
foreach d ($dlist)
	set elist = ( $elist `ls $evtpath/shm_$d*` )
end

set maxcnt=0
set maxfile=""
foreach e ($elist)
	set pno=`$SH_UTIL/no_of_phases_in.csh $e $stime $etime`
	if  ($pno > $maxcnt)  then
		set maxcnt=$pno
		set maxfile=$e
	endif
end

if  ($maxcnt > 0)  then
	echo "found $maxcnt phases in $maxfile"
	cp $maxfile $ofile
else
	echo "No phases found in this time interval ($stime $etime)."
endif
