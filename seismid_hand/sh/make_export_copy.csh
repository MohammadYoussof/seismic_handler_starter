#! /bin/csh
#
# file make_export_copy.csh
#      ====================
#
# version 11, 4-Aug-2000
#
# creates copy of SH for export
# K. Stammler, 31-Aug-93

if  ("$1" == "")  then
	echo "Usage $0 <export-path> [<ftp-file>]"
	exit
endif

# get parameters from command line
set expath=$1
set ftpfile=$2

if  (! -w $expath)  then
	echo "cannot write to $expath.  Abort"
	exit
endif

# set constants
set shcroot=/programs/sol2/sh
set tmpfile=$HOME/make_export_copy_$$.000

# copy whole directory tree to tree $expath
chdir $shcroot
chdir ..
echo "create SH tree on $expath"
tar cf - sh | (chdir $expath; tar xf -)
chdir $expath

# enable write access to files
echo "enable write access to all files"
chmod -R +w $expath/sh

# remove all directories not needed
echo "delete all directories not needed"
\rm -rf $expath/sh/command/BMP
\rm -rf $expath/sh/source/SCCS
\rm -rf $expath/sh/source/foreign/SCCS
\rm -rf $expath/sh/source/foreign/readdrm
\rm -rf $expath/sh/source/newgraph/SCCS

#! delete all files not needed
echo "delete all files not needed"
set flist = `find $expath/sh -name \*% -print`
\rm -f $flist
set flist = `find $expath/sh -name ,\* -print`
\rm -f $flist
set flist = `find $expath/sh -name \*.HP -print`
\rm -f $flist
set flist = `find $expath/sh -name \*.ps -print`
\rm -f $flist
set flist = `find $expath/sh -name \*.old -print`
\rm -f $flist
set flist = `find $expath/sh -name \*.bak -print`
\rm -f $flist
set flist = `find $expath/sh -name \*_old -print`
\rm -f $flist
set flist = `find $expath/sh -name \*.co -print`
\rm -f $flist
set flist = `find $expath/sh -name core -print`
\rm -f $flist
set flist = `find $expath/sh -name \*.000\* -print`
\rm -f $flist
set flist = `find $expath/sh -name fal\[0-9\]\* -print`
\rm -f $flist
\rm $expath/sh/shscratch/*
\rm $expath/sh/source/seed_io/seedcalib_*
\rm $expath/sh/source/seed_io/scan_streams

# put back header.ps and HEADER.HP file
cp $shcroot/inputs/header.ps $expath/sh/inputs
cp $shcroot/inputs/HEADER.HP $expath/sh/inputs

## copy version file from FTP-directory to doc-path
#cp $FTP/software/SH.versions $expath/sh/doc

# edit setup file
if  (-e $expath/sh/setup/shsetup.x)  \rm $expath/sh/setup/shsetup.x
if  (-e $tmpfile)  \rm $tmpfile
touch $tmpfile
echo -n 's/shcroot=\$PUBSOFT\/sh/shcroot=' >>$tmpfile
echo "`$DPROG/escape_string $expath/sh`/" >>$tmpfile
sed -f $tmpfile $expath/sh/setup/shsetup >$expath/sh/setup/shsetup.x
\rm $tmpfile
\rm $expath/sh/setup/shsetup
mv $expath/sh/setup/shsetup.x $expath/sh/setup/shsetup

# run setup file
source $expath/sh/setup/shsetup

echo "create tar file with executables"
tar cf sh_exe.tar sh

echo "clean up directories"
chdir $SH_ROOT
make clean_shm
chdir $expath
\rm -rf sh/source/seed_io/notimpl
\rm -rf sh/util/locsat
\rm -rf sh/util/hypoellipse
\rm -rf sh/util/notimpl
\rm sh/util/fk_src/*.o
\rm sh/util/fk_src/fk
\rm sh/util/rasttopnm
\rm sh/util/pnmtops

\rm -rf sh/util/fk_src
\rm -rf sh/util/locsat
\rm sh/util/fk
\rm sh/util/LocSAT

echo "create tar file with sources"
tar cf sh_src.tar sh

# copy this to ftp directory
if  ("$ftpfile" != "")  then
	echo "put this file to ftp-directory"
	cp sh_src.tar $ftpfile.tar
	gzip $ftpfile.tar
	\mv $ftpfile.tar.gz $FTP/software
	chdir $FTP/software
	if  (-e SH-SHM.tar.gz)  \rm SH-SHM.tar.gz
	ln -s $ftpfile.tar.gz SH-SHM.tar.gz
endif

