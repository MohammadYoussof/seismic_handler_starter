#! /bin/csh
#
# file get_html_text.csh
#      =================
#
# version 4, 9-Apr-2006
#
# Gets text web page and saves it to disk.  Uses lynx
# K. Stammler, 19-Dec-2002

if  ("$2" == "")  then
	echo "Usage: $0 <pageadr> <outfile> [<width>]"
	exit
endif

# get parameters
set pageadr="$1"
set outfile=$2
set width=$3

#set echo

# set constants

if  (-e $outfile)  \rm $outfile

set w=""
if  ("$width" != "")  set w="-width=$width"

lynx -dump $w "$pageadr" >$outfile
