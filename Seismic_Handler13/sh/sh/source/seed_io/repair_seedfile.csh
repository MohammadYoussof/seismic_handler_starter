#! /bin/csh
#
# file repair_seedfile.csh
#      ===================
#
# version 1, 10-Dec-98
#
# Throws away all SEED records with unreadable header from a given SEED file.
# Uses 'split' to split up the file into pieces of single records (gives
# many files !), runs 'sfdlist_l' and recollects all recognized records
# with 'seed_tidy'.  This is not the most effective way to do it, but
# simple to implement.
# K. Stammler, 10-Dec-98

if  ("$3" == "")  then
	echo "$0 <reclth> <inpfile> <outfile> [<tmppath>]"
	exit
endif

# get parameters
set reclth=$1
set inpfile=$2
set outfile=$3
set tmpdir=$4

if  (! -e $inpfile)  then
	echo "Cannot find $inpfile.  Abort."
	exit
endif

# set constants
if  ("$tmpdir" == "")  set tmpdir=$HOME/repair_$$

# make tmp directory
if  (-d $tmpdir)  then
	\rm $tmpdir/*
	rmdir $tmpdir
endif
set curdir=$PWD
mkdir $tmpdir
cp $inpfile $tmpdir
chdir $tmpdir

split -a 3 -b $reclth $inpfile:t split_
\rm $inpfile:t
$SEED_PROG/sfdlist_l.csh 'split_*' $PWD sfdfile.sfd quiet
$SEED_PROG/seed_tidy -complete sfdfile.sfd
\rm sfdfile.sfd split_*
set resfile=`ls`
if  ($#resfile > 1)  then
	echo "something went wrong.   Please clean $tmpdir"
	exit
else if  ($#resfile < 1)  then
	echo "no output created.  please check."
	chdir $curdir
	rmdir $tmpdir
	exit
endif
if  ("$resfile" != "$outfile")  then
	mv $resfile $outfile
endif

chdir $curdir
\mv $tmpdir/$outfile .
chmod a-x $outfile
rmdir $tmpdir
