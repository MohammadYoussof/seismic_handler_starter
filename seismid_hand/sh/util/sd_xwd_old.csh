#! /bin/csh
#
# file screendump.csh
#      ==============
#
# version 1, 21-Oct-93
#
# makes hardcopy of screen to PostScript printer
# K. Stammler, 20-Oct-93
#

echo ""
echo "begin screendump to file $1.rs ..."

set pnmdir=$SH_UTIL
set name="$SH_SCRATCH/$1"

# remove possibly existing scratch files
\rm $name.ps  >&/dev/null
\rm $name.rs  >&/dev/null

# execute and print out screen dump
xwd >$name.rs
xpr -device ps $name.rs >$name.ps
echo "printing scratch file $name.ps"
#lpr -Lf28_ps -h -s -r $name.ps
lp -onb $name.ps

# remove intermediate files
\rm $name.rs

## print termination message
#echo "screendump finished, printer file $1.ps queued."
#
## show printer status
#echo ""
#echo "printer status"
#lpc status
#
## show scratch directory
#echo ""
#echo "scratch directory"
#ls -s $SH_SCRATCH/*.ps
