#! /bin/csh
#
# file seed_fill_gaps.csh
#      ==================
#
# version 3, 14-Mar-96
#
# fills data gaps if data found on alternate directory
# K. Stammler, 4-Mar-96

if  ("$5" == "")  then
	echo "Usage: $0 <main-dir> <stream> <t-start> <t-end> <alt-dir>"
	exit
endif

#set echo

# get parameters
set main_dir=$1
set stream=$2
set t_start=$3
set t_end=$4
set alt_dir=$5

# set constants
set mainsfd=$HOME/fillgaps_main_$$.000
set altsfd=$HOME/fillgaps_alt_$$.000
set gapfile=$HOME/fillgaps_gap_$$.000
set tmpdir=$HOME/fillgaps_dir_$$

# check directories
if  (! -w $main_dir)  then
	echo "$0 : cannot write to data path $main_dir.  Abort"
	exit
endif
if  (! -e $alt_dir)  then
	echo "$0 : cannot read from alt data path $alt_dir. Abort"
	exit
endif

# make own sfdfiles
#set xdir=$PWD
#echo "chat: create own sfd files"
#chdir $main_dir
#$SEED_PROG/sfdlist.csh '*.?h?' $main_dir $mainsfd quiet
#chdir $alt_dir
#$SEED_PROG/sfdlist.csh '*.?h?' $alt_dir $altsfd quiet
#chdir $xdir
cp $main_dir/sfdfile.sfd $mainsfd
cp $alt_dir/sfdfile.sfd $altsfd

# create gap file and count gaps
echo "chat: create gap file for main directory"
if  (-e $gapfile)  \rm $gapfile
$SEED_PROG/seedgaps -sfdfile=$mainsfd $stream $t_start $t_end >$gapfile
set gap_no=`wc -l $gapfile`
set gap_no = $gap_no[1]
echo "chat: $gap_no gaps found"
# nothing to do on zero gaps
if  ($gap_no == 0)  then
	\rm $mainsfd $altsfd $gapfile
	exit
endif

# loop all gaps
# -------------
#
# cd to temporary working directory
mkdir $tmpdir
chdir $tmpdir
set gapcnt=1
gap_loop:
	# get next line from gap-file and parse it (empty line exits)
	set gapline=`sed -n $gapcnt"p" $gapfile`
	if  ("$gapline" == "")  goto gap_loop_exit
	set gap_start=$gapline[5]
	set gap_end=$gapline[7]
	set datfile_l=$gapline[9]
	set datfile=$datfile_l:t
	echo -n "chat: gap no $gapcnt $gap_start; "
	# get length of gap, negative gaps are ignored
	set tdiff=`$DPROG/timename time_intdiff $gap_end $gap_start`
	if  ($tdiff < 0)  then
		echo "negative gap, ignore"
		@ gapcnt = $gapcnt + 1
		goto gap_loop
	endif
	# try to copy gap data from alternate directory
	set res=`$SEED_PROG/copy_recs -quiet -exact $altsfd $stream $gap_start $gap_end gap_$stream`
	# if no data found nothing can be done, switch to next gap
	if  ("$res" == "")  then
		echo "error in copy_recs, see err-file"
		\rm gap_$stream
		@ gapcnt = $gapcnt + 1
		goto gap_loop
	else if  ($res[1] == 0)  then
		echo "no data available, no change"
		\rm gap_$stream
		@ gapcnt = $gapcnt + 1
		goto gap_loop
	endif
	# there is some data, let's put it into the main directory
	echo -n "fill $datfile; "
	# put the file with the gap to the working directory
	mv $datfile_l .
	# fill gap, output is created on main directory
	set res=`$SEED_PROG/seedmerge $datfile gap_$stream $datfile_l`
	if  ("$res" == "")  then
		# Something went wrong, output is in stderr
		echo -n "error (see err-file); "
	else if  ("$res[1]" == "done")  then
		# Now data file with filled gap is on main directory.
		# The local data files gap_$stream and $datfile are now obsolete
		echo -n "$res; "
#	else if  ("$res[1]" == "append")  then
#		# Here only the file with earlier data was copied to the main directory
#		# by seedmerge.  The other file must be copied 'by hand'.
#		set appfile=$res[2]
#		echo -n "copy-append; "
#		cp $appfile $main_dir
#		# if the local gap file is copied, care for appropriate name
#		if  ("$appfile" == "gap_$stream")  then
#			chdir $main_dir
#			$DPROG/rename_to_std.csh gap_$stream >& /dev/null
#			chdir $tmpdir
#		endif
	else
		# some strange return string from seedmerge
		echo -n "failure; "
	endif
	echo ""
	# clean up working directory and increment counter
	\rm gap_$stream $datfile
	@ gapcnt = $gapcnt + 1
goto gap_loop
gap_loop_exit:

chdir ..
rmdir $tmpdir

# remove scratch files
\rm $mainsfd $altsfd $gapfile
