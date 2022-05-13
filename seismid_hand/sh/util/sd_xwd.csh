#! /bin/csh
#
# file sd_xwd.csh
#      ==========
#
# version 3, 25-Nov-2003
#
# makes hardcopy of screen to PostScript printer
# K. Stammler, 20-Oct-93
#

if  ("$1" == "")  then
	echo "Usage: $0 <tmpname>"
	exit
endif

# get parameters
set tmpname=$1

# execute private hardcopy file if existing
if  (-x $SH_USERROOT/private/sd_xwd.csh)  then
	$SH_USERROOT/private/sd_xwd.csh $tmpname
	exit
endif

#set echo
echo ""
echo "begin screendump to file $tmpname.rs ..."

set pnmdir=$SH_UTIL
set name="$SH_SCRATCH/$tmpname"

# remove possibly existing scratch files
\rm $name.ps  >&/dev/null
\rm $name.rs  >&/dev/null

# execute and print out screen dump
xwd >$name.rs
xpr -device ps $name.rs >$name.ps
echo "printing scratch file $name.ps"
#lpr -Lf28_ps -h -s -r $name.ps
lpr -h -s $name.ps

# remove intermediate files
\rm $name.rs
