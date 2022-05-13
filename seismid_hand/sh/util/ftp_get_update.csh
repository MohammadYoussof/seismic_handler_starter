#! /bin/csh
#
# file ftp_get_update.csh
#      ==================
#
# version 1, 27-Dec-96
#
# Transfers requested SH-update (with ftp_request_update.csh) to $SH_ROOT
# K. Stammler, 27-Dec-96

if  ("$1" == "")  then
	echo "Usage: $0 <update-file>"
	exit
endif

set updfile=$1

chdir $SH_ROOT
$SH_UTIL/autoftp.csh get ftp.szgrf.uni-erlangen.de /pub/sh_updates $updfile
