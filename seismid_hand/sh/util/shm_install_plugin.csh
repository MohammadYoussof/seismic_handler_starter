#! /bin/csh
#
# file shm_install_plugin.csh
#      ======================
#
# version 1, 23-May-2006
#
# Retrieves plugin tarball and installs it
# K. Stammler, 23-May-2006

if  ("$3" == "")  then
	echo "Usage: $0 <ftpadr> <plugin-name> <plugin-idx>"
	exit
endif

# get parameters
set ftpadr=$1
set pname=$2
set pidx=$3

cd $SH_USERROOT/private

set pfile=""
if  (-e plugins.txt)  set pfile=$PWD/plugins.txt
if  ("$pfile" == "")  set pfile=$PWD/external_processes.txt

if  (! -e $pfile)  then
	set pfile=$PWD/plugins.txt
	touch $pfile
endif

wget $ftpadr/$pname.tar.gz

if  (! -e $pname.tar.gz)  then
	echo "$0 : cannot retrieve $pname from $ftpadr.  Abort."
	exit
endif

gzip -d $pname.tar
tar xvf $pname.tar
\rm $pname.tar
if  (! -e $pname.plg)  then
	echo "$0 : no plugin definition file $pname.plg found.  Abort."
	exit
endif
sed "s/##NUM##/$pidx/" $pname.plg >>$pfile
\rm $pname.plg
