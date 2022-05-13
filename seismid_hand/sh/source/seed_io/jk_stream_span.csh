#! /bin/csh
#
# file jk_stream_span.csh
#      ==================
#
# version 3, 21-Aug-2000
#
# returns available time span of specified stream stored in Jukebox
# K. Stammler, 13-Aug-96

if  ("$1" == "")  then
	echo "Usage: $0 <stream>"
	exit
endif

#set echo

# get parameters
set stream=$1

# parse stream string
set b_stream=`echo $stream | sed 's/-/ /g'`

# get start and end time of jukebox data
set res=`grep '^'$b_stream[1]'$' $SEED_INPUTS/grf_stations.list`
if  ("$b_stream[1]" == "grf")  set res="grf"
if  ("$res" == "")  then
	set genstream=grsn-$b_stream[2]
	if  ("$genstream" == "grsn-hh")  set genstream="grsn-bh"
else
	set genstream=grf-bh
endif
set res=`$SEED_PROG/jk_time_spans.csh $genstream`
if  ($#res < 3)  exit
set jukestart=$res[2]
set jukeend=$res[3]

# get individual start and end times of stream (if specified)
if  (-e $SEED_INPUTS/$stream.ontime)  then
	set streamstart=`cat $SEED_INPUTS/$stream.ontime`
else
	set streamstart=$jukestart
endif
if  (-e $SEED_INPUTS/$stream.offtime)  then
	set streamend=`cat $SEED_INPUTS/$stream.offtime`
else
	set streamend=$jukeend
endif

# take later start time and earlier end time
set tdiff=`$DPROG/timename time_intdiff $jukestart $streamstart`
if  ($tdiff < 0)  then
	set start=$streamstart
else
	set start=$jukestart
endif
set tdiff=`$DPROG/timename time_intdiff $streamend $jukeend`
if  ($tdiff < 0)  then
	set end=$streamend
else
	set end=$jukeend
endif

echo $start $end



