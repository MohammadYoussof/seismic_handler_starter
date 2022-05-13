#! /bin/csh
#
# file evt_extract_ms.csh
#      ==================
#
# version 1, 6-Jul-95
#
# extract MS values from evt-files
# K. Stammler, 6-Jul-95

if  ("$2" == "")  then
	echo "Usage: $0 <from> <to>"
	exit
endif

# set echo

# get parameters
set fromtime=$1
set totime=$2

# set constants
set detecprog=$DPROG
set evtdir=/home/b3sn10/seismo/evt_archive
set scrdir=$HOME/evt_extract_ms_$$
set listfile=evtlist.lis
set listfile_sorted=evtlist_sorted.lis
set evtall=evtall.txt
set msfile=evt_ms.txt
set grflist = ( gra1 gra2 gra3 gra4 grb1 grb2 grb3 grb4 grb5 grc1 grc2 grc3 grc4 )

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

# loop all files
if  (-e $listfile)  \rm $listfile
if  (-e $listfile_sorted)  \rm $listfile_sorted
if  (-e $evtall)  \rm $evtall
if  (-e $msfile)  \rm $msfile
touch $evtall
set flist=`ls *.evt`
if  ($#flist == 0)  echo "no evt-files found"
ls *.evt > $listfile
$SH_UTIL/evt_files_sort.csh $listfile >$listfile_sorted
set evtlist=`cat $listfile_sorted`
foreach evt ($evtlist)
	$SH_UTIL/evt_time_sort $evt >>$evtall
	\rm $evt
end

$SH_UTIL/evt_extract_ms $evtall

# delete scratch directory
\rm $scrdir/*
rmdir $scrdir
