#! /bin/csh
#
# file recopy_seed.csh
#      ===============
#
# version 1, 14-Sep-98
#
# Reads and rewrites given stream.  Finds gaps of stream within
# specified time window and writes data for each window
# separately.  Time gaps are enlarged by 0.1 sec.
#
# A problem with this implementation is, that if it writes long time spans
# in one go, the timing errors within this span accumulate.  At the end of
# the time span a visible time difference to the original data may occur.
#
# K. Stammler, 14-Sep-98

if  ("$5" == "")  then
	echo "Usage: $0 <sfdfile> <stream> <from> <to> <minlength>"
	echo "   <sfdfile>    sfdfile with data info"
	echo "   <stream>     stream name like 'bfo-hh-z'"
	echo "   <from>       processing window start time like '1-nov-97_8:35'"
	echo "   <to>         processing window end time"
	echo "   <minlength>  min. length of time window (sec).  Smaller are ignored"
	exit
endif

#set echo

# get parameters
set sfdfile=$1
set stream=$2
set stime=$3
set etime=$4
set minlth=$5

# set constants
set tmpfile=$HOME/seed_rfmt_$$.000
set wdwfile=SEED_RFMTWDW_$$.000
set pathfile=SEEDPATH_$$.STX
set shinp=shinp_$$.000

# parse stream
set tmp=`echo $stream | sed 's/-/ /g'`
if  ($#tmp < 3)  then
	echo "$0 : Syntax error in stream $stream"
	exit
endif
set station=$tmp[1]
set chan=$tmp[2]
set comp=$tmp[3]

# check parameters
if  (! -e $sfdfile)  then
	echo "$0 : sfdfile $sfdfile not found.  Abort."
	exit
endif

if  (-e $tmpfile)  \rm $tmpfile
if  (-e $wdwfile)  \rm $wdwfile
$SEED_PROG/seedgaps -sfdfile=$sfdfile $stream $stime $etime >$tmpfile

# if tmpfile is empty then no gaps :-), otherwise process gaps
if  (-z $tmpfile)  then
	echo $stime $etime >$wdwfile
else
	# loop all gaps and echo time windows
	set dec_s=$stime
	touch $wdwfile
	set cnt=1
	while  (1 > 0)  # forever
		set line=`sed -n $cnt"p" $tmpfile`
		if  ("$line" == "")  break
		if  ($#line < 7)  then
			echo "$0 : illegal syntax in seedgaps output.  Abort."
			exit
		endif
		set gap=$line[3]
		set gap_s=$line[5]
		set gap_e=$line[7]
		set tmp=`echo $gap | sed 's/\./ /'`
		if  ($tmp[1] < 0)  then
			echo "$stream : ignore negative gap $gap secs at $gap_s"
		else
			echo "$stream : found gap $gap s at $gap_s"
			set dec_e=`$SH_UTIL/timename time_subsec $gap_s 0.1`
			# now check whether resulting time window is large enough
			set tdiff=`$SH_UTIL/timename time_intdiff $dec_e $dec_s`
			if  ($tdiff > $minlth)  then
				echo "$dec_s $dec_e" >>$wdwfile
			else
				echo "$stream : ignore small time window, $tdiff sec at $dec_s"
			endif
			set dec_s=$gap_e
		endif
		@ cnt = $cnt + 1
	end
	set dec_e=$etime
	set tdiff=`$SH_UTIL/timename time_intdiff $dec_e $dec_s`
	if  ($tdiff > $minlth)  then
		echo "$dec_s $dec_e" >>$wdwfile
	else
		echo "$stream : ignore small time window, $tdiff sec at $dec_s"
	endif
endif

echo ""
echo "process time windows"
cat $wdwfile
echo ""

if  (-e $pathfile)  \rm $pathfile
echo $sfdfile:h >$pathfile
if  (-e $shinp)  \rm $shinp
touch $shinp
echo "rewrite_seed $pathfile $station $chan $comp $wdwfile" >>$shinp
echo "quit y"                                  >>$shinp
echo ""
echo "execute SH script"
cat $shinp
SH <$shinp
# \rm $pathfile $shinp $wdwfile

$SEED_PROG/sfdlist.csh '*.?h?' $PWD sfdfile.sfd quiet

