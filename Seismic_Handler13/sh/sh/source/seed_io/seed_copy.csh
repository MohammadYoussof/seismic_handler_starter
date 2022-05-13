#! /bin/csh
#
# file seed_copy.csh
#      =============
#
# version 8, 9-May-2007
#
# copies Mini-SEED files to output directory
# K. Stammler, 5-Jul-95

if  ("$1" == "help" || "$1" == "-help")  then
	echo "Usage: $0 <sfdfile> <streamlist> <from> <to> <workdir> <outfile> [<exact>]"
	exit
endif

#setenv QUIET_STARTUP
#source /usr/local/common_startup
#unsetenv QUIET_STARTUP

#set echo

# set constants
set scrdir=tmp_dir_$$
set seedspec="/programs/sol2/seed/own/src"
setenv HEADER_INPUT $seedspec/inputs
set jkpath="JK:"

# get parameters from command line
set sfdfile="$1"
set streamwild="$2"
set fromtime="$3"
set totime="$4"
set outdir="$5"
set outfile="$6"
set exact="$7"

# store original directory
set startdir=$PWD

# inquire empty parameters
while  ("$sfdfile" == "")
	echo -n "sfd-file directory [/cdrom]: "
	set sfdfile="$<"
	if  ("$sfdfile" == "")  set sfdfile="/cdrom"
	set sfdfile="`eval echo $sfdfile`"/sfdfile.sfd
	#if  ("$sfdfile" == '$ARCH')  set sfdfile="$ARCH/sfdfile.sfd"
	if  (! -e $sfdfile && "$sfdfile" != "$jkpath" && "$sfdfile" != "DB:")  then
		if  (-e $sfdfile/sfdfile.sfd)  then
			set sfdfile="$sfdfile/sfdfile.sfd"
		else
			echo "file $sfdfile does not exist"
			set sfdfile=""
		endif
	endif
end
if  ("$streamwild" == "")  then
	echo -n "streamlist [grf]: "
	set streamwild="$<"
	if  ("$streamwild" == "")  set streamwild="grf"
endif
while  ("$fromtime" == "")
	echo -n "from time [23-jul-95_8:23 or 23,7,95,8,23]: "
	set fromtime="$<"
end
while  ("$totime" == "")
	echo -n "to time   [23-jul-95_8:23 or 23,7,95,8,23]: "
	set totime="$<"
end
while  ("$outdir" == "")
	echo -n "working directory: "
	set outdir="$<"
	if  (! -d $outdir)  then
		echo "$outdir is an illegal directory"
		set outdir=""
		continue
	endif
	if  (! -w $outdir)  then
		echo "cannot write to $outdir"
		set outdir=""
	endif
end
if  ("$outfile" == "")  then
	echo -n "output file [seedout.sed]: "
	set outfile="$<"
	if  ("$outfile" == "")  set outfile="seedout.sed"
endif

## get list of streams
#if  ("$streamwild" == "grf")  set streamwild='gr[a-c]?-bh-?'
#if  ("$streamwild" == "grf-bh-?")  set streamwild='gr[a-c]?-bh-?'
#if  ("$streamwild" == "grf-bh-z")  set streamwild='gr[a-c]?-bh-z'
#if  ("$streamwild" == "grsn")  set streamwild='[a-f,h-z]*-bh-? grfo-bh-? gsh-bh-?'
#if  ("$streamwild" == "grsn-hh-?")  set streamwild='[a-f,h-z]*-hh-? gsh-hh-?'
#if  ("$streamwild" == "grsn-hh-z")  set streamwild='[a-f,h-z]*-hh-z gsh-hh-z'
#if  ("$streamwild" == "grsn-bh-?")  set streamwild='[a-f,h-z]*-bh-? grfo-bh-? gsh-bh-?'
#if  ("$streamwild" == "grsn-bh-z")  set streamwild='[a-f,h-z]*-bh-z grfo-bh-z gsh-bh-z'
#if  ("$streamwild" == "grsn-lh-?")  set streamwild='[a-f,h-z]*-lh-? grfo-lh-? gsh-lh-?'
#if  ("$streamwild" == "grsn-lh-z")  set streamwild='[a-f,h-z]*-lh-z grfo-lh-z gsh-lh-z'
#
#set streamlist=`(chdir $SEED_PROG/streams; ls $streamwild)`

set streamlist=""
if  ("$streamwild" == "grf")        set streamlist=`$DPROG/list_streams.csh grf-bh`
if  ("$streamwild" == "grf-bh-?")   set streamlist=`$DPROG/list_streams.csh grf-bh`
if  ("$streamwild" == "grf-bh-z")   set streamlist=`$DPROG/list_streams.csh grf-bh | grep z$`
if  ("$streamwild" == "grsn")       set streamlist=`$DPROG/list_streams.csh grsn-bh-all`
if  ("$streamwild" == "grsn-hh-?")  set streamlist=`$DPROG/list_streams.csh grsn-hh-all`
if  ("$streamwild" == "grsn-hh-z")  set streamlist=`$DPROG/list_streams.csh grsn-hh-all | grep z$`
if  ("$streamwild" == "grsn-bh-?")  set streamlist=`$DPROG/list_streams.csh grsn-bh-all`
if  ("$streamwild" == "grsn-bh-z")  set streamlist=`$DPROG/list_streams.csh grsn-bh-all | grep z$`
if  ("$streamwild" == "grsn-lh-?")  set streamlist=`$DPROG/list_streams.csh grsn-lh-all`
if  ("$streamwild" == "grsn-lh-z")  set streamlist=`$DPROG/list_streams.csh grsn-lh-all | grep z$`
if  ("$streamwild" == "sxnet")      set streamlist=`$DPROG/list_streams.csh sxnet-bh-all`
if  ("$streamwild" == "sxnet-bh-?") set streamlist=`$DPROG/list_streams.csh sxnet-bh-all`
if  ("$streamwild" == "sxnet-bh-z") set streamlist=`$DPROG/list_streams.csh sxnet-bh-all | grep z$`

if  ("$streamlist" == "")  then
	set streamlist=`(chdir $SEED_PROG/streams; ls $streamwild)`
endif

# copy data to temporary directory
chdir $outdir
mkdir $scrdir
chdir $scrdir
if  ("$sfdfile" == "$jkpath")  then
	set loc_sfdfile="$sfdfile"
	set pathcache=""
	set f_pathcache=""
else if  ("$sfdfile" == "DB:")  then
	set loc_sfdfile=/tmp/seed_copy_$$.sfd
	if  (-e $loc_sfdfile)  \rm $loc_sfdfile
	if  ("`uname`" == "Linux")  then
		echo "station chan    comp    pathid  relpath sdate   stime   edate   etime   recnum hswap    recsize offset  dataflags       priority        dataformat" >$loc_sfdfile
		$SH_UTIL/sfdb/sfd_from_sfdb.py $fromtime $totime >>$loc_sfdfile
		set pathcache=""
		set f_pathcache=""
	else
		$SEED_PROG/sfd_from_sfdb.csh $fromtime $totime >$loc_sfdfile
		set f_pathcache=/tmp/seed_pathcache_$$.000
		if  (-e $f_pathcache)  \rm $f_pathcache
		$SH_UTIL/sol_sql_call.csh $SFDBHOST "select * from pathtab order by id" >$f_pathcache
		set pathcache="-pathcache=$f_pathcache"
	endif
else
	set pathcache=""
	set f_pathcache=""
	set loc_sfdfile=$startdir/$sfdfile
	if  (! -e $loc_sfdfile)  set loc_sfdfile=$sfdfile
endif
if  ("$exact" == "exact")  then
	set exact="-exact"
else
	set exact=""
endif
foreach stream ($streamlist)
	echo "copying stream $stream"
	$SEED_PROG/copy_recs $pathcache -defaultnet=GR $exact $loc_sfdfile $stream \
		$fromtime $totime standard
end

if  ("$sfdfile" == "DB:")  \rm $loc_sfdfile
if  ("$f_pathcache" != "")  \rm $f_pathcache

if  ("$outfile" == "mini-seed")  then
	mv * ..
	chdir ..
	rmdir $scrdir
	exit
endif

# create seed-volume
$seedspec/seed_volume.csh '*' $outfile

# remove scratch dir
if  (-f ./$outfile)  mv $outfile ..
chdir ..
\rm -rf $scrdir
