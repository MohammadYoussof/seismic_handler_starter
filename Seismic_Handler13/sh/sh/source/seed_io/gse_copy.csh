#! /bin/csh
#
# file gse_copy.csh
#      ============
#
# version 9, 05-Feb-2003
#
# copies Mini-SEED files to GSE files
# K. Stammler, 7-Jul-95

if  ("$1" == "help" || "$1" == "-help")  then
	echo "Usage: $0 <sfddir> <streamlist> <from> <to> <outfile> [<autodrm>]"
	exit
endif

setenv QUIET_STARTUP
source /usr/local/common_startup
unsetenv QUIET_STARTUP

#set echo

# get parameters from command line
set sfddir="$1"
set streamwild="$2"
set fromtime="$3"
set totime="$4"
set outfile="$5"
set autodrm=$6

# set home path
if  ("$autodrm" == "www")  then
	setenv HOME /home/wwwuser
	source $SH_ROOT/setup/shsetup
endif

# set constants
set sh_inpfile=$HOME/sh_gse_copy_$$.000
set sh_outpath=$HOME/shlog
set sh_outfile=$sh_outpath/gse_copy_$$.log
set cmail=/home/autodrm/work/current.mail
if  (! -e $sh_outpath)  mkdir $sh_outpath

# inquire empty parameters
while  ("$sfddir" == "")
	echo -n "sfd-file directory [/cdrom]: "
	set sfddir="$<"
	if  ("$sfddir" == "")  set sfddir="/cdrom"
	set sfddir=`eval echo $sfddir`
	if  (! -e $sfddir/sfdfile.sfd && $sfddir != "JK:")  then
		echo "file $sfddir/sfdfile.sfd does not exist"
		set sfddir=""
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
if  ("$outfile" == "")  then
	echo -n "output file [gseout.gse]: "
	set outfile="$<"
	if  ("$outfile" == "")  set outfile="gseout.gse"
endif

# get list of streams
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

if  ($#streamlist == 0)  then
	set streamlist=`echo "$streamwild" | sed "s/*/ /g" | sed "s/?/ /"`
	if  ($#streamlist != 1)  then
		echo "$0 : illegal stream list $streamwild.  Abort.
		exit
	endif
endif

# write input file for SH
set readlth=`$SH_UTIL/timename time_intdiff $totime $fromtime`
if  ("$sfddir" == "/cdrom")  then
	set sh_sfddir='%cdrom'
else if  ("$sfddir" == "/cdrom/cdrom0")  then
	set sh_sfddir='%cdrom2'
else if  ("$sfddir" == "JK:")  then
	set sh_sfddir='JK:'
else if  ("$sfddir" == "DB:" || "$sfddir" == "db:")  then
	set sh_sfddir='DB:'
else
	setenv SH_READDIR "$sfddir"'/'
	set sh_sfddir='SH_READDIR:'
endif
if  (-e $sh_inpfile)  \rm $sh_inpfile
touch $sh_inpfile
echo "nr"   >>$sh_inpfile
foreach stream ($streamlist)
	set b_stream=`echo $stream | sed 's/-/ /g'`
	if  ($#b_stream == 3)  then
		set b_stat=$b_stream[1]
		set b_chan=$b_stream[2]
		set b_comp=$b_stream[3]
	else if  ($#b_stream == 2)  then
		set b_stat=$b_stream[1]
		set b_chan=""
		set b_comp=$b_stream[2]
	else
		set b_stat=$b_stream[1]
		set b_chan=""
		set b_comp=z
	endif
	echo "reads $sh_sfddir $fromtime $readlth $b_stat $b_comp $b_chan" >>$sh_inpfile
end
echo "@WRITEGSE $outfile" >>$sh_inpfile
echo "quit y"             >>$sh_inpfile

# execute SH
if  (-e $sh_outfile)  \rm $sh_outfile
SH <$sh_inpfile >& $sh_outfile

if  (! -e $outfile || -z $outfile)  then
	echo ""     >>$sh_outfile
	echo ""     >>$sh_outfile
	echo "------------------- input was -------------------" >> $sh_outfile
	echo ""     >>$sh_outfile
	echo ""     >>$sh_outfile
	cat $sh_inpfile >>$sh_outfile
	if  ("$autodrm" == "autodrm" && -e $cmail)  then
		echo ""     >>$sh_outfile
		echo ""     >>$sh_outfile
		echo "------------------- mail request was --------------" >> $sh_outfile
		echo ""     >>$sh_outfile
		echo ""     >>$sh_outfile
		cat $cmail >>$sh_outfile
	endif
	if  ("$autodrm" == "autodrm")  then
		echo "`date +%d-%h-%y_%H:%M` SH $fromtime reason 06 $sh_outfile" \
		>>$AUTODRMFAILLOG
	endif
	if  (-z $outfile)  \rm $outfile
else
	\rm $sh_outfile
endif

# remove scratch files
\rm $sh_inpfile

# remove old shlog-files
chdir $sh_outpath

