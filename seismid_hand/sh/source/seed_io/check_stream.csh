#! /bin/csh
#
# file check_stream.csh
#      ================
#
# version 4, 10-Jul-95
#
# displays available data and checks for gaps
# K. Stammler, 15-Mar-95

if  ("$1" == "")  then
	echo "Usage: $0 <stream>"
	exit
endif

# get parameters
set stream="$1"

# get station, channel and component
set b_stream=`echo $stream | sed 's/-/ /g'`
if  ($#b_stream != 3)  then
	echo "$0 : illegal stream syntax in $stream"
	exit
endif
set station="$b_stream[1]"
set channel="$b_stream[2]"
set comp="$b_stream[3]"
#set stream="$station-$channel-$comp"

# set paths
set sfdfile="$HOME/chkstr_$$.sfd"

if  (-e $sfdfile)  \rm $sfdfile

set datapathlist=( $ARCH $ARCHF $DACO $DATA )
set datanamelist=( "ARCH" "ARCHF" "DACO" "DATA" )

set datacnt = 0
echo ""
foreach datapath ($datapathlist)
	@ datacnt = $datacnt + 1
	if  (! -e $datapath)  then
		echo "no data on $datanamelist[$datacnt] ($datapath)"
		echo "--------------------"
		echo ""
		echo ""
		continue
	endif
	# create sfd-file
	if  ("$datapath" == $ARCHF)  then
		set result=`$SEED_PROG/inquire_avd $ARCHF/sfdfile.sfd $stream`
		\cp $ARCHF/sfdfile.sfd $sfdfile
	else
		set wildcard="$station*.$channel$comp"
		$SEED_PROG/sfdlist.csh "$wildcard" $datapath $sfdfile quiet
		# get available time window
		set result=`$SEED_PROG/inquire_avd $sfdfile $stream`
	endif
	set start_time="$result[1]"
	set end_time="$result[2]"
	echo "stream $stream on directory $datanamelist[$datacnt] ($datapath)"
	echo "-------------------------------------"
	echo ""
	echo "available from $start_time to $end_time"
	echo "data gaps:"
	$SEED_PROG/seedgaps -sfdfile=$sfdfile $stream $start_time $end_time
	echo ""
	echo ""
end

\rm $sfdfile
