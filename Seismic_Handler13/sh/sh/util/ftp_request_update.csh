#! /bin/csh
#
# file ftp_request_update.csh
#      ======================
#
# version 2, 27-Dec-96
#
# Requests update of SH/SHM source via ftp.  Creates version list
# and ftp's it to to SZGRF ftp server.  To make this script work you
# need to add the following lines into your $HOME/.netrc file:
#
#   machine ersn06.szgrf.uni-erlangen.de
#   login ftp
#   password <your-email-addr>
#
# If the file $HOME/.netrc does not exist, create it AND set the mode
# with 'chmod 600 $HOME/.netrc'.
#
# The first parameter <sh-root> is the absolute path of your sh root,
# the second parameter is the version filename WITHOUT path.  It will always
# be created on $SH_ROOT.
#
# K. Stammler, 23-Aug-96

if  ("$2" == "")  then
	echo "Usage: $0 <sh-root> <vrs-file>"
	exit
endif

# set constants
set szgrf=ftp.szgrf.uni-erlangen.de
set szgrf_path=/pub/sh_updates

# get parameters from command line
set shroot=$1
set vrsfile=$2
set shstartup=$shroot/setup/shsetup

source $shstartup

chdir $SH_ROOT

# create version list
if  (-e $vrsfile)  \rm $vrsfile
$SH_UTIL/vrs_list.csh $shroot >$vrsfile

# export it to the SZGRF
$SH_UTIL/autoftp.csh put $szgrf $szgrf_path $vrsfile
