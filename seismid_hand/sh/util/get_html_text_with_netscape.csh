#! /bin/csh
#
# file get_html_text.csh
#      =================
#
# version 2, 2-Jun-2004
#
# Gets text web page and saves it to disk.  Needs running
# netscape process or at least access to X-Server.
# K. Stammler, 19-Dec-2002

if  ("$2" == "")  then
	echo "Usage: $0 <pageadr> <outfile>"
	exit
endif

# get parameters
set pageadr="$1"
set outfile=$2

#set echo

# set constants
set user=`whoami`
set display=":0.0"
if  ("`hostname`" == "ersn13")  set display=":0.1"
if  ("`whoami`" == "seismo" && "`hostname`" == "ersn16")  then
	set display="ersn13:0.1"
endif
#if  (-e /opt/SUNWns/netscape)  then
#	set ns=/opt/SUNWns/netscape
#else
	set ns=/programs/sol2/netscape-4.5/netscape
#endif

if  (-e $outfile)  \rm $outfile

setenv DISPLAY "$display"

# check for netscape
set prc="`ps -ef | grep netscape | tail -1 | grep $user | grep -v grep`"
if  ("$prc" == "")  then
	$ns &
	sleep 15
endif

$ns -remote "openURL($pageadr)"
sleep 2
$ns -remote "saveAs($outfile,Text)"
