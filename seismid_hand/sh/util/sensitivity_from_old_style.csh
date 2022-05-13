#! /bin/csh
#
# file sensitivity_from_old_style.csh
#      ==============================
#
# version 1, 20-May-2006
#
# Create sensitivity file from old-style seedcalib_... files
# K. Stammler, 20-Mar-2006

set tmpfile=$HOME/sc_$$.000


if  (-e $tmpfile)  \rm $tmpfile
cd $SEED_INPUTS
ls seedcalib_*-??-? | sed 's/seedcalib_//' >$tmpfile

set cnt=1
while  (1 > 0)
	set stream=`sed -n $cnt"p" $tmpfile`
	if  ("$stream" == "")  break
	set c=1
	while  (1 > 0)
		set line=`sed -n $c"p" seedcalib_$stream`
		if  ("$line" == "")  break
		echo "$stream  $line"
		@ c ++
	end
	@ cnt ++
end

\rm $tmpfile
