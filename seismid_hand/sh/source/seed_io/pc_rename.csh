#! /bin/csh
#
# file pc_rename.csh
#      =============
#
# version 2, 30-Sep-98
#
# renames all file matching wildcard passed
# K. Stammler, 15-Nov-94

if  ("$1" == "")  then
	echo "Usage: $0 <wildcard>"
	exit
endif

set flist=`ls $1`
foreach i ($flist)
	if  (-d $i)  continue
	set name=`$SEED_PROG/pcname $i`
	if  ("$name" == "")  continue
	if  (! -e $name[1])  mkdir -p $name[1]
	\mv $i $name[1]/$name[2]
end
