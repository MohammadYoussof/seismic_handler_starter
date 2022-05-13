#! /bin/csh
#
# file al_apply_resid_corr.csh
#      =======================
#
# version 2, 27-Feb-2001
#
# Applies corrections to residuals in a relonset file
# K. Stammler, 8-Feb-2001

if  ("$5" == "")  then
	echo "Usage: $0 <relonset> <outfile> <phase> <slow> <baz>"
	exit
endif

#set echo

# get parameters
set relonset=$1
set outfile=$2
set phase=$3
set slow=$4
set baz=$5

# check files
if  (! -e $relonset)  then
	echo "$0 : Input file $relonset not found.  Abort."
	exit
endif
if  (-e $outfile)  \rm $outfile
touch $outfile

set xphase=$phase
set cnt=1
while  (1 > 0)
	set line=`sed -n $cnt"p" $relonset`
	if  ("$line" == "")  break
	if  ($#line < 2)  then
		echo "$line"  >>$outfile
	else if  ("$line[1]" == "\!")  then
		echo "$line"  >>$outfile
	else
		set station=$line[1]
		set reltime=`printf "%5.2f" $line[2]`
		if  ("$phase" == "col3")  then
			set xphase=$line[3]
		endif
		set corr=`$SH_UTIL/getresid $station $xphase $slow $baz`
		set line[2]=`echo "scale=2; $reltime - $corr" | bc`
		echo "$line"  >>$outfile
	endif
	@ cnt = $cnt + 1
end
