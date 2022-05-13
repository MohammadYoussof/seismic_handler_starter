#! /bin/csh
#
# file swap_vax_files.csh
#      ==================
#
# version 1, 20-feb-95
#
# swaps all files matching wildcard $1 with 'swap_vax_recs'
# keeps old filenames
# K. Stammler, 20-feb-95


if  ("$1" == "")  then
	echo "Usage: $0 <wild>"
	exit
endif

set flist=`ls $1`
foreach i ($flist)
	mv $i $i.tmp
	$SEED_PROG/swap_vax_recs $i.tmp $i
	\rm $i.tmp
end
