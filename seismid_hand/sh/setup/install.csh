#! /bin/csh
#
# file install.csh
#      ===========
#
# version 4, 18-Jun-2006
#
# Installation script for SH/SHM package
# K. Stammler, 13-Nov-2003

set ext="undefined"
if  ("`uname`" == "Linux")  then
	set ext="linux"
else if  ("`uname`" == "SunOS")  then
	set ext="sol2.5.1"
endif

if  ("$1" == "")  then
	echo "Usage: $0 <installpath> [<sourcepath>]"
	echo ""
	echo "   Steps to install are:"
	echo "   (i)   create installation directory (e.g. 'mkdir $PWD/shprog')"
	echo "   (ii)  create tar file directory (e.g. 'mkdir $PWD/shtar')"
	echo "   (iii) cd to tar file directory and get files:"
   echo "         ftp://ftp.szgrf.bgr.de/pub/software/SH-SHM.tar.gz"
   echo "         ftp://ftp.szgrf.bgr.de/pub/software/fk-$ext.tar.gz"
   echo "         ftp://ftp.szgrf.bgr.de/pub/software/locsat-$ext.tar.gz"
   echo "         ftp://ftp.szgrf.bgr.de/pub/software/SHM-install.csh"
	echo "   (iv)  make SHM-install.csh executable 'chmod a+x ./SHM-install.csh'"
	echo "   (v)   call e.g. './SHM-install.csh $PWD/shprog'"
	echo "         Note that the parameter must be the absolute path to the"
	echo "         installation directory"
	echo ""
	echo -n "Do you want run run the installation now ? [y/n] "
	set reply="$<"
	if  ("$reply" == "y" || "$reply" == "Y")  then
		set res=`which wget`
		if  (! -e $res)  then
			echo "need 'wget' to continue.  Please install"
			exit
		endif
		echo -n "Where should SH/SHM be installed? Enter full pathname: "
		set dstpath="$<"
		if  (! -d $dstpath)  mkdir $dstpath
		if  (! -w $dstpath)  then
			echo "Cannot write to $dstpath.  Abort."
			exit
		endif
		set srcpath=$HOME/shpackage
		if  (-d $srcpath)  then
			echo "SH package directory $srcpath already exists."
			echo "Please rename or delete it and restart the installation program."
			exit
		endif
		echo -n "Create SH package directory $srcpath ? [y/n] "
		set reply="$<"
		if  ("$reply" == "y" || "$reply" == "Y")  then
			mkdir $srcpath
		else
			exit
		endif
		echo "download SH/SHM package"
		(cd $srcpath; wget ftp://ftp.szgrf.bgr.de/pub/software/SH-SHM.tar.gz)
		echo "download FK package"
		(cd $srcpath; wget ftp://ftp.szgrf.bgr.de/pub/software/fk-$ext.tar.gz)
		echo "download LocSAT package"
		(cd $srcpath; wget ftp://ftp.szgrf.bgr.de/pub/software/locsat-$ext.tar.gz)
	else
		exit
	endif

else

	# get parameters
	set dstpath=$1
	set srcpath=$2
	if  ("$srcpath" == "")  set srcpath=$PWD

endif

# check parameters
if  (! -d $srcpath)  then
	echo "$0 : source path $srcpath not found.  Abort."
	exit
endif
if  (! -w $dstpath)  then
	echo "$0 : cannot write to installation path $dstpath.  Abort."
	exit
endif
if  ("$srcpath" == "$dstpath")  then
	echo "$0 : source path and installation path must not be equal.  Sorry."
	exit
endif

# find input files
set pack_shm=""
set pack_fk=""
set pack_locsat=""

if  (-e $srcpath/SH-SHM.tar.gz)  then
	set pack_shm=$srcpath/SH-SHM.tar.gz
else
	set pack_shm=`ls $srcpath/SH-* | tail -1`
endif
if  ("`uname`" == "Linux")  then
	if  (-e $srcpath/fk-linux.tar.gz)  set pack_fk=$srcpath/fk-linux.tar.gz
	if  (-e $srcpath/locsat-linux.tar.gz)  set pack_locsat=$srcpath/locsat-linux.tar.gz
else if  ("`uname`" == "SunOS")  then
	if  (-e $srcpath/fk-sol2.5.1.tar.gz)  set pack_fk=$srcpath/fk-sol2.5.1.tar.gz
	if  (-e $srcpath/locsat-sol2.5.1.tar.gz)  set pack_locsat=$srcpath/locsat-sol2.5.1.tar.gz
else
endif

if  (-e $srcpath/libahio.a.$ext)  then
	set use_ah=1
else
	set use_ah=0
endif


# default values
set compile_shm="yes"
set compile_fk="no"
set compile_locsat="no"
set displ_res="undef"
set userroot=$HOME/shfiles
if  ("`uname`" == "Linux")  then
	set linux_colors="yes"
else
	set linux_colors="no"
endif

# find text editor
set editor=""
foreach  ed  (textedit nedit vuepad xedit)
	set res=`which $ed`
	if  (-e "$res")  then
		set editor=$ed
		break
	endif
end

# check for some programs in search path
set plist = ( gzip make makedepend gcc uil ar ranlib xterm )
foreach p ($plist)
	set res=`which $p`
	if  (! -e "$res")  then
		echo "Program $p not found in search path.  Please add."
		exit
	endif
end


# enter setup menu

set cmd=""
while  ("$cmd" != "i")

	echo ""
	echo "(p)  SHM-package:              $pack_shm"
	echo "(f)  FK-package:               $pack_fk"
	echo "(l)  LocSAT-package:           $pack_locsat"
	echo "(u)  user root path:           $userroot"
	echo "(e)  window based texteditor:  $editor"
	echo "(cp) compile SHM:              $compile_shm"
	echo "(cf) compile FK:               $compile_fk"
	echo "(cl) compile LocSAT:           $compile_locsat"
#	echo "(r)  resolution:               $displ_res"
	echo "(c)  linux colors:             $linux_colors"
	echo "(i)  Start Installation"
	echo "(q)  Quit Installation"
	echo ""
	echo "if all settings are ok, enter 'i'"
	echo ""
	echo -n "cmd: "
	set cmd="$<"

	if  ("$cmd" == "q")  exit
	if  ("$cmd" == "p")  then
		echo -n "SHM-package: "
		set pack_shm="$<"
	endif
	if  ("$cmd" == "f")  then
		echo -n "FK-package: "
		set pack_fk="$<"
	endif
	if  ("$cmd" == "l")  then
		echo -n "LocSAT-package: "
		set pack_locsat="$<"
	endif
	if  ("$cmd" == "cp")  then
		if  ("$compile_shm" == "yes")  then
			set compile_shm="no"
		else
			set compile_shm="yes"
		endif
	endif
	if  ("$cmd" == "cf")  then
		if  ("$compile_fk" == "yes")  then
			set compile_fk="no"
		else
			set compile_fk="yes"
		endif
	endif
	if  ("$cmd" == "cl")  then
		if  ("$compile_locsat" == "yes")  then
			set compile_locsat="no"
		else
			set compile_locsat="yes"
		endif
	endif
	if  ("$cmd" == "r")  then
		if  ("$displ_res" == "xga")  then
			set displ_res="wxga"
		else if  ("$displ_res" == "wxga")  then
			set displ_res="sxga"
		else
			set displ_res="xga"
		endif
	endif
	if  ("$cmd" == "c")  then
		if  ("$linux_colors" == "yes")  then
			set linux_colors="no"
		else
			set linux_colors="yes"
		endif
	endif
	if  ("$cmd" == "e")  then
		echo -n "window based texteditor: "
		set editor="$<"
		if  (! -e $editor)  then
			echo "$editor does not exist."
			echo -n "Enter <Return> ..."
			set x="$<"
			set editor=""
		endif
	endif
	if  ("$cmd" == "u")  then
		echo -n "user root path: "
		set userroot="$<"
		if  (! -e $userroot)  then
			mkdir $userroot
			if  (! -e $userroot)  then
				echo "cannot create $userroot. Reset to default."
				echo -n "Enter <Return> ..."
				set x="$<"
				set userroot=$HOME/shfiles
			endif
		endif
	endif
	if  ("$cmd" == "i")  then
		if  ("$editor" == "")   then
			echo "Please specify window based texteditor executable."
			echo -n "Enter <Return> ..."
			set x="$<"
			set cmd="."
		endif
	endif

end


# check paths again
if  (! -e $pack_shm)  then
	echo "$0 : echo SHM-package $pack_shm not found.  Abort."
	exit
endif
if  (! -e $pack_fk)  then
	echo "$0 : FK-package not found.  Abort."
	exit
endif
if  (! -e $pack_locsat)  then
	echo "$0 : LocSAT-package not found.  Abort."
	exit
endif


# copy and extract SHM
echo "extracting SHM-package"
cd $dstpath
cp $pack_shm .
set pack_shm=$pack_shm:t
gzip -d $pack_shm
set pack_shm=$pack_shm:r
if  (! -e $pack_shm)  then
	set pack_shm=`ls SH-?.??_SHM-?.??.tar | tail -1`
	if  ("$pack_shm" == "")  then
		echo "SHM package extraction error.  Abort."
		exit
	else if  (! -e $pack_shm)  then
		echo "SHM package extraction error.  Abort."
		exit
	endif
endif
chmod +w $pack_shm
tar xf $pack_shm
\rm $pack_shm

## this is for testing the installation program
#\cp /home/klaus/new-shm-files/shsetup.template $dstpath/sh/setup
#\cp /home/klaus/new-shm-files/shm.uil $dstpath/sh/source/motif

# create setup file from template
echo "prepare setup file"
cd sh/setup
if  ("`uname`" == "Linux")  then
	set remstr="#OS-linux "
else if  ("`uname`" == "SunOS")  then
	set remstr="#OS-sol2 "
else
	echo "$0 : sorry, this operating system is not supported"
	exit
endif
set locsatpath=$dstpath/sh/util/locsat
set plth=`echo $locsatpath | wc -c`
if  ($plth > 25)  then
	if  (-e $HOME/lcs)  \rm $HOME/lcs
	ln -s $locsatpath $HOME/lcs
	set locsatpath=$HOME/lcs
	set plth=`echo $locsatpath | wc -c`
	if  ($plth > 25)  then
		echo "$0 : cannot find short locsat path.  Abort."
		exit
	endif
endif
if  (-e shsetup)  \rm shsetup
sed "s/$remstr//" shsetup.template | sed "s@##ROOTPATH##@$dstpath/sh@" | \
	sed "s@##LOCSATPATH##@$locsatpath@" | sed "s@##EDITOR##@$editor@" | \
	sed 's/-DSH_SETUP_SZGRF//' | sed "s@##USERROOT##@$userroot@" >shsetup
if  ($use_ah == 0)  then
	if  (-e shsetup.x)  \rm shsetup.x
	sed 's/-DSH_SETUP_AH//' shsetup | sed 's/-lahio//' >shsetup.x
	\rm shsetup
	mv shsetup.x shsetup
endif
if  ($?MACHTYPE == 1)  then
	if  ("`echo $MACHTYPE | grep 64`" != "")  then
		if  (-d /usr/X11R6/lib64)  then
			if  (-e shsetup.x)  \rm shsetup.x
			sed 's/\/usr\/X11R6\/lib/\/usr\/X11R6\/lib64/' shsetup >shsetup.x
			\rm shsetup
			mv shsetup.x shsetup
		endif
	endif
endif
source shsetup

# create directories for evt files
mkdir $SH_USERROOT/evt
mkdir $SH_USERROOT/evt/evid
mkdir $SH_USERROOT/evt/evtout

# find screen resolution
set displ_res=`$dstpath/sh/util/get_screen_resolution.csh`
if  ($#displ_res != 3)  then
	echo "cannot find screen resolution, please set manually"
	set displ_res="sxga"
else
	echo "found screen resolution $displ_res"
	set displ_res=$displ_res[3]
endif

set origsetup=$dstpath/sh/inputs/shm-config-default.txt
if  (! -e $origsetup)  then
	echo "Installation program error.  Sorry."
	exit
endif
set thissetup=$dstpath/sh/inputs/shm-config.txt
if  (-e $thissetup)  \rm $thissetup
cp $origsetup $thissetup

# settings for small displays (laptops)
if  ("$displ_res" != "" && "$displ_res" != "sxga")  then
	if  (-e $thissetup.x)  \rm $thissetup.x
	sed "s/shm-cof-sxga-geometry/shm-conf-$displ_res-geometry/" $thissetup >$thissetup.x
	mv $thissetup.x $thissetup
endif

# linux colors
if  ("$linux_colors" == "yes")  then
	# do nothing, this is default
else
	if  (-e $thissetup.x)  \rm $thissetup.x
	sed 's/reverse_xors                 true/reverse_xors                 false/' $thissetup>$thisetup.x
	mv $thissetup.x $thissetup
endif

# compile SHM
if  ("$compile_shm" == "yes")  then
	echo "compiling SHM"
	cd $SH_ROOT
	# remove all o's and a's
	set olist=`find . -name \*.o -print`
	\rm $olist
	cd $SH_LIB
	if  (-e libahio.a)  mv libahio.a libahio.ax
	\rm *.a
	if  (-e libahio.ax)  mv libahio.ax libahio.a
	cd $SH_ROOT
	make depend_shm
	make shm
	make world
endif

# install FK
echo "extract FK package"
cd $SH_UTIL
if  (-e fk_src)  \rm -rf fk_src
cp $pack_fk .
set pack_fk=$pack_fk:t
gzip -d $pack_fk
set pack_fk=$pack_fk:r
chmod +w $pack_fk
tar xf $pack_fk
\rm $pack_fk
if  (-e fk)  \rm fk
ln -s fk_src/fk fk

# compile FK
if  ("$compile_fk" == "yes")  then
	echo "compile FK package"
	cd fk_src
	\rm *.o
	make
endif

# install LocSAT
echo "extract LocSAT package"
cd $SH_UTIL
if  (-e locsat)  \rm -rf locsat
mkdir locsat
cp $pack_locsat .
set pack_locsat=$pack_locsat:t
gzip -d $pack_locsat
set pack_locsat=$pack_locsat:r
chmod +w $pack_locsat
tar xf $pack_locsat
\rm $pack_locsat
if  (-e LocSAT)  \rm LocSAT
ln -s locsat/bin/LocSAT LocSAT

# compile LocSAT
if  ("$compile_locsat" == "yes")  then
	echo "compile LocSAT package"
	cd locsat
	set olist=`find . -name \*.o -print`
	\rm $olist
	set olist=`find . -name \*.a -print`
	\rm $olist
	cd $SH_UTIL/locsat/libsrc
	make
	cd $SH_UTIL/locsat/bin/LocSAT
	make
endif

# add SHM resources to .Xdefaults
set resrc=""
if  (-e ~/.Xdefaults)  set resrc=~/.Xdefaults
if  ("$resrc" == "")  then
	if  (-e ~/.Xresources)  set resrc=~/.Xresources
endif
if  ("$resrc" == "")  then
	set resrc="~/.Xdefaults"
	touch $resrc
endif
set res=`grep "SHM*background" $resrc`
if  ("$res" == "")  then
	echo "SHM*locsat_prefix_text.value:       tab"  >>$resrc
	echo "SHM*fk_frqlo_text.value:            0.4"  >>$resrc
	echo "SHM*fk_frqhi_text.value:            3.0"  >>$resrc
	echo "SHM*fk_slowness_text.value:         15"   >>$resrc
	echo "SHM*fk_resol_text.value:            51"   >>$resrc
	echo "SHM*fk_colnum_text.value:           10"   >>$resrc
	echo "SHM*filter_butpar_hi_text.value:    1Hz"  >>$resrc
	echo "SHM*filter_butpar_lo_text.value:    100s" >>$resrc
	echo "SHM*filter_butpar_order_text.value: 4"    >>$resrc
	echo "SHM*opick_thresh_text.value:        10.0" >>$resrc
	echo "SHM*opick_duration_text.value:      1.0"  >>$resrc
	echo "SHM*opick_break_text.value:         0.00" >>$resrc
endif
xrdb $resrc


echo ""
echo ""
echo "Please call"
echo ""
echo "source $dstpath/sh/setup/shsetup"
echo ""
echo "before using SH or SHM.  Put this line into your .cshrc file"
echo ""

