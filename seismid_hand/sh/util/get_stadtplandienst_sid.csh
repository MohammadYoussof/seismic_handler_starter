#! /bin/csh
#
# file get_stadtplandienst_sid.csh
#      ===========================
#
# version 1, 15-Dec-2005
#
# Returns sid value for German stadtplandienst service
# K. Stammler, 15-Dec-2005

#set echo

# set constants
set sidfile=$SH_SCRATCH/last_stadtplandienst_sid.000
set tmpfile=$SH_SCRATCH/stadtplan_$$.000

if  (-e $tmpfile) \rm $tmpfile

set noglob

$SH_UTIL/get_html_text.csh http://www.stadtplandienst.de $tmpfile
set line=`grep 'http://www.stadtplandienst.de/map.asp?search=coords&sid=' $tmpfile | head -1`
if  ($#line != 2)  then
	echo $#line
	echo "$line"
	echo "$0 : cannot get stadtplandienst sid.  Abort."
	\rm $tmpfile
	exit
endif

set adr="$line[2]"
$SH_UTIL/get_html_text.csh "$adr" $tmpfile

set sid=`echo "$line[2]" | sed 's/http:\/\/www.stadtplandienst.de\/map.asp?search=coords\&sid=//'`
if  (-e $sidfile)  \rm $sidfile
echo "$sid" >$sidfile

\rm $tmpfile
