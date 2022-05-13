#! /bin/csh
#
# file szgrf_cd_mount.csh
#      ==================
#
# version 2, 16-Aug-95
# v 2: also included Solaris 2 systems
#
# mounts CD automatically for specified time and stream
# K. Stammler, 8-May-95

# get parameters
if  ("$1" == "" || "$2" == "")  then
	echo "Usage: $0 <stream> <time>"
	exit
endif
set stream="$1"
set reqtime="$2"

#set echo

# determine operating system
set os_version=`uname -r`
set os_version=`$DPROG/stringop extract $os_version 0 1`

# constants
if  ($os_version > 4)  then
	set cdrom="/cdrom/cdrom0"
	set mntcmd="/usr/local/cd_wait_for_mount.csh"
	set umntcmd="/usr/local/ignore"
else
	set cdrom="/cdrom"
	set mntcmd="/usr/local/cd_mount_iso.csh"
	set umntcmd="/usr/local/cd_umount_iso.csh"
endif

# which CD is needed ?
set res=`$SEED_PROG/lookup_cd $stream $reqtime`
if  ("$res" == "")  then
	echo "time $reqtime for stream $stream not available on CD"
	exit
endif
set label="$res[1]"
set magic="$res[2]"

again:

# check whether already someting mounted
set flist=`ls $cdrom`
if  ("$flist" == "")  then
	# no disk in drive
	echo ""
	echo "-----------------------------"
	echo "| `hostname`: need disk $label |"
	echo "-----------------------------"
	echo ""
	# wait for user to put it in
	set answer="$<"
	if  ($os_version < 5)  then
		# mount it
		$mntcmd `hostname`
	endif
else
	if  (-e $cdrom/label)  then
		set disklabel=`cat $cdrom/label`
		set str=""
		if  ("$disklabel" == "$label")  set str=$label
	else
		set sfd1=`head -1 $cdrom/sfdfile.sfd`
		set str=`echo "$sfd1" | grep "$magic"`
	endif
	if  ("$str" == "")  then
		# wrong disk in drive
		echo ""
		echo "-----------------------------------"
		echo "| `hostname`: unmounting current disk |"
		echo "-----------------------------------"
		$umntcmd `hostname`
		eject /dev/sr0
		echo "-----------------------------"
		echo "| `hostname`: need disk $label |"
		echo "-----------------------------"
		echo ""
		# wait for user to put it in
		set answer="$<"
		# mount it
		$mntcmd `hostname`
	else
		# correct disk already mounted
		echo "nothing to do, disk $label already mounted"
		exit
	endif
endif

# now check for correct disk
if  (-e $cdrom/label)  then
	set disklabel=`cat $cdrom/label`
	set str=""
	if  ("$disklabel" == "$label")  set str=$label
else
	set sfd1=`head -1 $cdrom/sfdfile.sfd`
	set str=`echo "$sfd1" | grep "$magic"`
endif
if  ("$str" == "")  then
	echo "wrong disk in drive, need $label"
	echo "go back again"
	goto again
endif
