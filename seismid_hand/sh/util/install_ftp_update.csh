#! /bin/csh
#
# file install_ftp_update.csh
#      ======================
#
# version 3, 22-Dec-2000
#
# Installs requested (ftp_request_update.csh) and transferred
# (ftp_get_update.csh) SH-update.  The script assumes that the setup file
# $SH_ROOT/setup/shsetup is properly set and the selected compiler
# $SH_COMPILER is available.

if  ("$1" == "")  then
	echo "Usage: $0 <update-file(-without-path)> [<noshm>]"
	exit
endif

# get parameters
set updfile=$1
set noshm=$2

# find decompression program gzip
if  (-e /usr/bin/gzip)  then
	set unzip="/usr/bin/gzip -d"
else if  (-e /usr/bin/gunzip)  then
	set unzip="/usr/bin/gunzip"
else
	echo "No gzip found in standard locations.  Decompression might not work."
	set unzip="gzip -d"
endif

# decompress and extract source files
chdir $SH_ROOT
if  (! -e $updfile)  then
	echo "$0 : update tarfile $updfile not found.  Abort."
	exit
endif
$unzip $updfile
set updfile=$updfile:r
tar xvf $updfile

if  ("$noshm" == "noshm")  then
	make depend
	make
else
	make depend_shm
	make shm
endif
