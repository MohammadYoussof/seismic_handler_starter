#! /bin/csh
#
# file sol_sql_call.csh
#      ================
#
# version 1, 7-Nov-2006
#
# Calls mysql on db host
# K. Stammler, 7-Nov-2006

if  ("$2" == "")  then
	echo "Usage; $0 <host> <sqlcmd>"
	exit
endif

#set echo

set dbhost="$1"
set cmd="$2"

set noglob
/usr/local/bin/ssh -n $dbhost '/usr/bin/mysql sfdb -e '\"$cmd\"
