#! /bin/csh
#
# file make_telex_mox.csh
#      ==================
#
# version 1, 14-Oct-96
#
# creates MOX telex message from evt-files
# K. Stammler, 14-Oct-96

if  ("$3" == "")  then
	echo "Usage: $0 <from> <to> <telexheader>"
	exit
endif

# get parameters
set fromtime=$1
set totime=$2
set telexheader="$3"

# set constants
set detecprog=$DPROG
set evtdir=$EVT/evt_archive
set scrdir=$HOME/make_telex_$$
set listfile=evtlist.lis
set listfile_sorted=evtlist_sorted.lis
set evtall=evtall.txt
set telexfile=telex.txt
set grflist = ( mox )

# get 6-digit string
set s6from=`$detecprog/timename time_digit6 $fromtime`
# increment totime
set totime=`$detecprog/timename time_addsec $totime 86400`
set s6to=`$detecprog/timename time_digit6 $totime`

# loop over days and get list of all files
chdir $evtdir
set s6cur=$s6from
set curtime=$fromtime
set evtlist = ( )
while  ("$s6cur" != "$s6to")
	set evtlist = ( $evtlist `ls shm_$s6cur*.evt` )
	set curtime=`$detecprog/timename time_addsec $curtime 86400`
	set s6cur=`$detecprog/timename time_digit6 $curtime`
end

if  (-e $scrdir)  then
	echo "$0 scratch dir $scrdir already exists. abort."
	exit
endif
mkdir $scrdir

#echo $evtlist
foreach evt ($evtlist)
	$SH_UTIL/evt_copy_and_split $evt $evtdir $scrdir
end

# now go to scratch dir and use the created files
chdir $scrdir

# loop all grf stations
foreach station ($grflist)
	if  (-e $listfile)  \rm $listfile
	if  (-e $listfile_sorted)  \rm $listfile_sorted
	if  (-e $evtall)  \rm $evtall
	if  (-e $telexfile)  \rm $telexfile
	touch $evtall
	set flist=`find . -name $station\* -print`
	if  ($#flist == 0)  continue
	ls $station* >& $listfile
	$SH_UTIL/evt_files_sort.csh $listfile >$listfile_sorted
	set evtlist=`cat $listfile_sorted`
	foreach evt ($evtlist)
		$SH_UTIL/evt_time_sort $evt >>$evtall
		\rm $evt
	end
	$SH_UTIL/ev2telex -ignoreflag $evtall $telexfile "$telexheader" $station
	set telexheader=""
	cat $telexfile
end
echo "stop+++"

echo $scrdir
# delete scratch directory
cd ..
\rm -rf $scrdir
