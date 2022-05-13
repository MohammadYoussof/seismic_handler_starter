#! /bin/csh
#
# file fir_decimate.csh
#      ================
#
# version 5, 30-Mar-99
#
# Decimates stream using fir_resample.c.  Finds gaps of stream within
# specified time window and calls fir_resample for each window
# separately.  To account for starting process the time gaps are increased
# by a specified time.
# K. Stammler, 5-Dec-97

if  ("$9" == "")  then
	echo "Usage: $0 <sfdfile> <stream> <from> <to> <outchan> <buftime> <minlength> <firfile> <chunklth>"
	echo "   <sfdfile>    sfdfile with data info"
	echo "   <stream>     stream name like 'bfo-hh-z'"
	echo "   <from>       processing window start time like '1-nov-97_8:35'"
	echo "   <to>         processing window end time"
	echo "   <outchan>    output channel name like 'bh'"
	echo "   <buftime>    gaps are enlarged by this amount at start and end"
	echo "   <minlength>  min. length of time window (sec).  Smaller are ignored"
	echo "   <firfile>    name of file with FIR-filter"
	echo "   <chunklth>   data are processed in chunks of this length (sec)"
	exit
endif

#set echo

# get parameters
set sfdfile=$1
set stream=$2
set stime=$3
set etime=$4
set outchan=$5
set skip=$6
set minlth=$7
set firfile=$8
set chunklth=$9

# set constants
set tmpfile=$HOME/fir_decimate_$$.000
set wdwfile=$HOME/fir_decwdw_$$.000

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
			echo "content: $line"
			echo "on $stream $stime $etime"
			\rm $tmpfile $wdwfile
			exit
		endif
		set gap=$line[3]
		set gap_s=$line[5]
		set gap_e=$line[7]
		set tmp=`echo $gap | sed 's/\./ /'`
		if  ($tmp[1] < 0 || "$tmp[1]" == "-0")  then
			echo "$stream : ignore negative gap $gap secs at $gap_s"
		else
			echo "$stream : skip gap $gap at $gap_s"
			set dec_e=$gap_s
			# subtract skiptime also from end of time window
			set dec_e=`$SH_UTIL/timename time_subsec $dec_e $skip`
			# now check whether resulting time window is still large enough
			set tdiff=`$SH_UTIL/timename time_intdiff $dec_e $dec_s`
			if  ($tdiff > $minlth)  then
				echo "$dec_s $dec_e" >>$wdwfile
			else
				echo "$stream : ignore small time window, $tdiff sec at $dec_s"
			endif
			set dec_s=`$SH_UTIL/timename time_addsec $gap_e $skip`
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

# remove old fir scratch files
\rm fir_*.seed

# loop over windows
set cnt=1
while  (1 > 0)  # forever
	set line=`sed -n $cnt"p" $wdwfile`
	if  ($#line < 2)  break
	set dec_s=$line[1]
	set dec_e=$line[2]
	# care for even number of samples, round window to seconds
	set tdiff=`$SH_UTIL/timename time_intdiff $dec_e $dec_s`
	set dec_e=`$SH_UTIL/timename time_addsec $dec_s $tdiff`
	$SEED_PROG/fir_resample -nowarn -chan=$outchan $sfdfile $stream \
		$dec_s $dec_e $firfile $chunklth
	mv fir_resample.seed fir_$cnt.seed
	@ cnt = $cnt + 1
end

# call seed_tidy
$SEED_PROG/sfdlist.csh 'fir_*.seed' $PWD fir.sfd quiet
$SEED_PROG/seed_tidy -complete -onefile fir.sfd

\rm $tmpfile $wdwfile fir.sfd


