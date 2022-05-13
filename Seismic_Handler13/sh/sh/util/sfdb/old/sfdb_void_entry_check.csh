#! /bin/csh
#
# file sfdb_void_entry_check.csh
#      =========================
#
# version 1, 22-Dec-2006
#
# Checks and removes void entries of pathid's smaller than 1000 in sftab.
# K. Stammler, 22-Dec-2006

# get DB command
if  ($?SH_USER_PARAM == 0)  then
	echo "$0 : no SH_USER_PARAM variable.  Abort."
	exit
endif
set cmd=`grep sfdb_command $SH_USER_PARAM | sed 's/sfdb_command//'`
if  ("$cmd" == "")  then
	echo "$0 : no sfdb command found in $SH_USER_PARAM.  Abort."
	exit
endif
set cmdqual=`grep sfdb_exec_qual $SH_USER_PARAM | sed 's/sfdb_exec_qual//'`
if  ("$cmdqual" == "")  then
	echo "$0 : no sfdb exec_qual found in $SH_USER_PARAM.  Abort."
	exit
endif

set idlist=`$cmd $cmdqual "select id from pathtab where id>0 and id<1000" | grep -v id`

foreach id ($idlist)
	$SH_UTIL/sfdb/sfdb_check_outdated_entries.csh $id correct
end

