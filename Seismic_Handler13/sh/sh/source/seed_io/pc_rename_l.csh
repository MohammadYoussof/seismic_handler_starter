#! /bin/csh
#
# file pc_rename_l.csh
#      ===============
#
# version 1, 28-Aug-97
#
# renames all file matching wildcard passed, (does the same as 'pc_rename.csh'
# but for many files.
# K. Stammler, 28-Nov-97

if  ("$1" == "")  then
	echo "Usage: $0 <wildcard>"
	exit
endif

# get parameters
set wildcard="$1"

# set constants
set tmpfile=$HOME/pc_rename_$$.000

if  (-e $tmpfile)  \rm $tmpfile
find . -name "$wildcard" -type f -print >$tmpfile

set cnt=1
while  (1 > 0)  # forever
	set i=`sed -n $cnt"p" $tmpfile`
	if  ("$i" == "")  break
	@ cnt = $cnt + 1
	if  (-d $i)  continue
	set name=`$SEED_PROG/pcname $i`
	if  ("$name" == "")  continue
	if  (! -e $name[1])  mkdir -p $name[1]
	mv $i $name[1]/$name[2]
end

\rm $tmpfile
