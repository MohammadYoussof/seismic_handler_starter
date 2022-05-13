#! /bin/csh
#
# file make_telex.csh
#      ==============
#
# version 6, 12-Sep-97
#
# creates telex message from evt-files
# K. Stammler, 30-Jun-95

if  ("$3" == "")  then
	echo "Usage: $0 <from> <to> <telexheader> [<stations>] [<allphases>]"
	exit
endif

# get parameters
set fromtime=$1
set totime=$2
set telexheader="$3"
set stations=$4
set allphases=$5

if  ("$stations" == "")  set stations=grf
set ignflag=""
if  ("$allphases" == "allphases" \
	|| "$allphases" == "yes")  set ignflag="-ignoreflag"

# set constants
set detecprog=$SH_UTIL
if  ($?EVTPATH == 1)  then
	set evtdir=$EVTPATH
else
	set evtdir=$EVT/evt_archive
endif
set scrdir=$HOME/make_telex_$$
set listfile=evtlist.lis
set listfile_sorted=evtlist_sorted.lis
set evtall=evtall.txt
set telexfile=telex.txt
if  ("$stations" == "grf")  then
	set statlist = ( gra1 gra2 gra3 gra4 grb1 grb2 grb3 grb4 grb5 \
		grc1 grc2 grc3 grc4 )
else if  ("$stations" == "grsn")  then
	set statlist = ( bfo brg bug bseg cll clz fur mox tns wet )
else
	set statlist=`echo $stations | sed 's/,/ /g'`
endif

# get 6-digit string
set year=`$SH_UTIL/timename time_to_int $fromtime`
set year=$year[1]
set yp=""
if  ($year >= 2000)  set yp=1
set s6from=`$SH_UTIL/timename time_digit6 $fromtime`
# increment totime
set totime=`$SH_UTIL/timename time_addsec $totime 86400`
set s6to=`$SH_UTIL/timename time_digit6 $totime`

# loop over days and get list of all files
chdir $evtdir
set s6cur=$s6from
set curtime=$fromtime
set evtlist = ( )
while  ("$s6cur" != "$s6to")
	set evtlist = ( $evtlist `ls shm_${yp}${s6cur}*.evt` )
	set curtime=`$SH_UTIL/timename time_addsec $curtime 86400`
	set s6cur=`$SH_UTIL/timename time_digit6 $curtime`
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
foreach station ($statlist)
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
	$SH_UTIL/ev2telex $ignflag $evtall $telexfile "$telexheader" $station
	set telexheader=""
	cat $telexfile
end
echo "stop+++"

chdir ..
# delete scratch directory
\rm $scrdir/*
rmdir $scrdir
