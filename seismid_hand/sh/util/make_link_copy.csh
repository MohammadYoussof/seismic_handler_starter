#! /bin/csh
#
# file make_link_copy.csh
#      ==================
#
# version 2, 12-Mar-2007
#
# Makes a link copy of the SH tree on the current directory
# K. Stammler, 3-Dec-2004

if  ("$1" == "")  then
	echo "Usage: $0 <sh-root>"
	exit
endif

set echo

# get parameters
set shroot=$1

set dst=$PWD

cd $shroot

set flist=`find . -name \*.c -print`
set flist = ( $flist `find . -name \*.h -print` )
set flist = ( $flist `find . -name \*.uil -print` )
set flist = ( $flist `find . -name Makefile -print` )
set flist = ( $flist `find . -name \*.dat -print` )
set flist = ( $flist `find . -name \*.csh -print` )
set flist = ( $flist `find . -name \*.py -print` )
set flist = ( $flist `find . -name \*.gse -print` )
set flist = ( $flist `find . -name \*.DAT -print` )
set flist = ( $flist `find . -name \*.txt -print` )
set flist = ( $flist `find . -name \*.TXT -print` )

echo "creating $#flist links"

cd $dst
foreach f ($flist)
	set d=$f:h
	if  (! -d $d)  mkdir -p $d
	ln -s $shroot/$f $f
end

if  (! -d lib)  mkdir lib

set dlist = ( source/seed_io/inputs command globals filter help \
	errors inputs doc plugins )
foreach d ($dlist)
	if  (-e $d)  \rm -rf $d
	ln -s $shroot/$d $d
end

# copy setup file, this must be different
if (! -d setup)  mkdir setup
cp -i $shroot/setup/shsetup setup

