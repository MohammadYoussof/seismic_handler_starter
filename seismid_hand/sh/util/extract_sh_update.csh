#! /bin/csh
#
# file extract_sh_update.csh
#      =====================
#
# version 1, 21-Aug-96
#
# Creates update file of SH/SHM.  It is assumed that the source version
# list is up to date.
# first input is version list of remote installation
# second input is name of output file (if .Z compressed, if .gz gzip'ed)
# K. Stammler, 21-Aug-96

if  ("$2" == "")  then
	echo "Usage: $0 <version-list> <output-name>"
	exit
endif

setenv QUIET_STARTUP
if  (-e /usr/local/common_startup)  source /usr/local/common_startup
unsetenv QUIET_STARTUP

# get parameters
set remotevrs=$1
set outname=$2

# set paths
set sourcepath=$SH_ROOT
set sourcevrs=$FTP/software/sh_vrslist.txt
set diff_file=$HOME/vrs_diff_$$.txt
set copy_list=$HOME/vrs_copy_$$.lis

# check files
if  (! -e $sourcevrs)  then
	echo "$0 : source version list $sourcevrs not found.  Abort"
	exit
	# should check for last update time
else if  (! -e $remotevrs)  then
	echo "$0 : remote version $remotevrs not found.  Abort"
	exit
endif

# create difference file and copy list
if  (-e $diff_file)  \rm $diff_file
diff $sourcevrs $remotevrs >$diff_file
if  (-e $copy_list)  \rm $copy_list
$SH_UTIL/vrs_read_diff $diff_file >$copy_list
\rm $diff_file

# check for writa access to output file and absolute path name
echo "create tar-archive"
if  (-e $outname)  \rm $outname
if  (-e $outname)  then
	echo "$0 : no write access to $outname.  Abort"
	\rm $copy_list
	exit
endif
touch $outname
if  (! -e $outname)  then
	echo "$0 : no write access to $outname.  Abort"
	\rm $copy_list
	exit
endif
set oldpath=$PWD
chdir $sourcepath
if  (-e $oldpath/$outname)  then
	set fulloutname=$oldpath/$outname
else if  (-e $outname)  then
	set fulloutname=$outname
else
	echo "$0 : Illegal output name $outname.  Abort"
	\rm $copy_list
	exit
endif
\rm $fulloutname
if  ("$fulloutname:e" == "Z" || "$fulloutname:e" == "gz")  then
	set fulloutname=$fulloutname:r
endif

# write tar file
set cnt=1
loop_start:
	set fname=`sed -n $cnt"p" $copy_list`
	if  ("$fname" == "")  goto loop_exit
	echo process $fname
	if  ($cnt == 1)  then
		tar cf $fulloutname $fname
	else
		tar uf $fulloutname $fname
	endif
	@ cnt = $cnt + 1
goto loop_start
loop_exit:
\rm $copy_list

# compress if requested
if  ("$outname:e" == "Z")  then
	compress $fulloutname
else if  ("$outname:e" == "gz")  then
	gzip $fulloutname
endif


