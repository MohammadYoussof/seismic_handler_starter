#! /bin/csh
#
# file inquire_stream_on_jk.csh
#      ========================
#
# version 1, 12-Aug-96
#
# Checks whether stream & time is available on Jukebox
# K. Stammler, 12-Aug-96


if  ("$2" == "")  then
	echo "Usage: $0 <stream> <time>"
	exit
endif

# set constants
set jukebox=/juke1pc

# get parameters
set stream=$1
set itime=$2

# get CD name
set cdname=`$SEED_PROG/lookup_cd $stream $itime`
if  ($#cdname < 1)  then
	echo "no"
	exit
endif
set cdname=$cdname[1]
if  ("$cdname" == "unavailable" || "$cdname" == "online")  then
	echo "no"
	exit
endif

# now look whether time is between ontime and offtime and in jukebox
set wdw=`$SEED_PROG/jk_stream_span.csh $stream $itime`
if  ($#wdw < 2)  then
	echo "no"
	exit
endif
set tdiff=`$DPROG/timename time_intdiff $itime $wdw[1]`
if  ($tdiff < 0)  then
	echo "no"
	exit
endif
set tdiff=`$DPROG/timename time_intdiff $wdw[2] $itime`
if  ($tdiff < 0)  then
	echo "no"
	exit
endif

# look whether CD is really there
set res=`ls $jukebox | grep $cdname`
if  ("$res" == "")  then
	echo "no"
else
	echo "yes"
endif
