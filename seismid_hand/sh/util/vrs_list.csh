#! /bin/csh
#
# version 3, 22-Dec-96
#
# v 2, 19-Dec-94, K. Stammler: piped grep output to head -1
# v 3, 22-Dec-96, K. Stammler: changed 'cd' to '\cd'
#
# extracts version numbers of all files of a tree
# K. Stammler, 10-Aug-94

if  ("$1" == "")  then
	echo "Usage: $0 <root>"
	exit
endif

# set paths
set listfile_x=,vrs_list_x_$$.000
set listfile=,vrs_list_$$.000
set scrfile=,vrs_grep_$$.000

# set constants
set rootpath="$1"
cd $rootpath

if  (-e $listfile_x)  \rm $listfile_x
if  (-e $listfile)  \rm $listfile
if  (-e $scrfile)  \rm $scrfile

find . -type f ! -name ',*' ! -name '[ps].*' ! -name '*%' \
	! -name '*.orig' ! -name '*.old' ! -name 'fal[0-9][0-9]*' \
	! -name '*.000' -print >$listfile_x
sort $listfile_x >$listfile
\rm $listfile_x

# loop list
set cnt=1
#set echo
loop_start:
	set cfile=`sed -n $cnt"p" $listfile`
	if  ("$cfile" == "")  goto loop_exit
	touch $scrfile
	grep "* version" $cfile | head -1 >>$scrfile
	if  (-z $scrfile)  grep "! version" $cfile | head -1 >>$scrfile
	if  (-z $scrfile)  grep "# version" $cfile | head -1 >>$scrfile
	if  (! -z $scrfile)  then
		echo -n "$cfile,"
		cat $scrfile
	endif
	\rm $scrfile
	@ cnt = $cnt + 1
goto loop_start
loop_exit:

\rm $listfile
