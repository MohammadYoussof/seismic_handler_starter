#! /bin/csh
#
# file string_replace.csh
#      ==================
#
# version 1, 21-May-2006
#
# Replace strings in a set of files
# K. Stammler, 21-May-2006

if  ("$3" == "")  then
	echo "Usage: $0 <wildcard> <search> <replace>"
	exit
endif

set wild="$1"
set search="$2"
set repl="$3"

set flist=`ls $wild`
foreach f ($flist)
	if  (-e $f.tmp)  \rm $f.tmp
	sed "s/$search/$repl/" $f >$f.tmp
	\rm $f
	mv $f.tmp $f
end
