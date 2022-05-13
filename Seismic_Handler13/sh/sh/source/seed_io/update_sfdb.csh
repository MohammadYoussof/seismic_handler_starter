#! /bin/csh
#
# file update_sfdb.csh
#      ===============
#
# version 1, 12-Nov-2006
#
# Updates sfdb database every few minutes or every day.
# Should run on the database host.
#
# K. Stammler, 12-Nov-2006

if  ("$2" == "")  then
	echo "Usage: $0 <backtime> <unit>"
	exit
endif

#set echo

# get parameters
set backtime="$1"
set unit="$2"

# set constants
set tmpfile=/tmp/sfdb_upd_$$.000
set tmpfile2=/tmp/sfdb_upd2_$$.000

if  ("$unit" != "m" && "$unit" != "d")  then
	echo "$0 : <unit> must be m or d"
	exit
endif

if  ("$unit" == "m")  then
	set min=0
	set max=100
else
	set min=0
	set max=1000
endif

set res=`mysql sfdb -B -e "select id,rootpath from pathtab where id > $min and id < $max" | grep -v rootpath`

set cnt=1
while  ($cnt <= $#res)

	# get new path
	set pathid=$res[$cnt]
	@ cnt ++
	if  ($cnt > $#res)  then
		echo "$0 : this cannot happen.  Uneven number of elements."
		exit
	endif
	set rpath=$res[$cnt]
	@ cnt ++
	#echo $pathid $rpath

	# get new files command
	if  ("$unit" == "m")  then
		set cmd=`mysql sfdb -B -e "select m_cmd from newfilescmd where id = $pathid" | grep -v m_cmd | sed "s/__TIME__/$backtime/"`
	else
		set cmd=`mysql sfdb -B -e "select d_cmd from newfilescmd where id = $pathid" | grep -v d_cmd | sed "s/__TIME__/$backtime/"`
	endif
	if  ("$cmd" == "")  then
		echo "No update command for $rpath.  Ignore path"
		continue
	endif

	# get list of new files
	if  (-e $tmpfile)  \rm $tmpfile
	`eval echo $cmd` >$tmpfile

	cd $rpath

	# read through list
	if  (-e $tmpfile2)  \rm $tmpfile2
	touch $tmpfile2
	set line=1
	while  (1 > 0)
		set f=`sed -n $line"p" $tmpfile | sed 's/^\.\///'`
		if  ("$f" == "")  break
		@ line ++
		if  ("$f:e" == "sfd")  continue
		if  ("`echo $f | grep '/ACE\.T/'`" != "")  continue
		if  ("`echo $f | grep '/LOG\.L/'`" != "")  continue
		if  ("`echo $f | grep '/...\.E/'`" != "")  continue
		$SEED_PROG/sfdline $f | $SEED_PROG/sfd2db -u -invhdr TT 3 $pathid >>$tmpfile2
	end

	mysql sfdb <$tmpfile2

end

\rm $tmpfile $tmpfile2
