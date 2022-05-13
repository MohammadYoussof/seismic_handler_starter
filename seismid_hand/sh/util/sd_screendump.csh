#! /bin/csh
#
# file screendump.csh
#      ==============
#
# version 2, 30-Oct-95
#
# makes hardcopy of screen to PostScript printer
# K. Stammler, 20-Oct-93
#

echo ""
echo "begin screendump to file $1.rs ..."

set pnmdir="$SH_UTIL"
set name="$SH_SCRATCH/$1"

# remove possibly existing scratch files
\rm $name.ps  >&/dev/null
\rm $name.pnm >&/dev/null
\rm $name.rs  >&/dev/null

# execute and print out screen dump
screendump $name.rs
$pnmdir/rasttopnm $name.rs >$name.pnm
$pnmdir/pnmtops -d 600 -scale 0.65 $name.pnm > $name.ps
lpr -h -s -r $name.ps

# remove intermediate files
\rm $name.pnm
\rm $name.rs

# print termination message
echo "screendump finished, printer file $1.ps queued."

# show printer status
echo ""
echo "printer status"
lpc status

# show scratch directory
echo ""
echo "scratch directory"
ls -s $SH_SCRATCH/*.ps
