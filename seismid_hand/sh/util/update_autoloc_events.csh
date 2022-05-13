#! /bin/csh
#
# file update_autoloc_events.csh
#      =========================
#
# version 1, 11-Nov-2004
#
# Updates Autoloc eventfile 'eventlist' on home directory
# K. Stammler, 11-Nov-2004

set evfile=$HOME/eventlist

set flist=`ls /autoloc/ev*.alert*`

# reset file
if  (-e $evfile)  \rm $evfile
touch $evfile

if  ($#flist == 0)  then
	exit
endif

foreach f ($flist)
	$SH_UTIL/autoloc2evt $f auto P >>$evfile
end
