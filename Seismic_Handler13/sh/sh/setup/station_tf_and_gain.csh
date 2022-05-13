#! /bin/csh
#
# file station_tf_and_gain.csh
#      =======================
#
# version 1, 15-Nov-2003
#
# Assign transfer function and gain factor to station
# K. Stammler, 15-Nov-2003

set xprog=$SH_ROOT/setup
set defstime="21-Jul-1972_00:00"
set defetime="21-Jul-2015_00:00"

set lkfile=$SH_INPUTS/filter_lookup.txt

echo ""
echo -n "Station name: "
set station="$<"
echo -n "Channel list at station $station, separated by blanks [default:HH BH LH]: "
set chanlist="$<"
if  ("$chanlist" == "")  set chanlist = ( HH BH LH )
echo -n "Component list, separated by blanks [default:Z N E]: "
set complist="$<"
if  ("$complist" == "")  set complist = ( Z N E )

set station=`echo $station | sed y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/`
set upstat=$station

# transfer function entries

set avtf=`(cd $SH_FILTER; ls *_S+G_WWSSN_SP.FLF) | sed 's/_S+G_WWSSN_SP.FLF//' | grep -v TF_`

set res=`grep "^$station-" $lkfile`
if  ("$res" == "")  then

	echo "List of transfers function classes:"
	echo "   $avtf"
	echo ""
	echo -n "Enter transfer function class for station ${station}: "
	set tf="$<"
	if  ("$tf" == "")  then
		echo "Empty input, operation aborted."
		exit
	endif
	set tf=`echo $tf | sed y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/`
	set found=0
	foreach t ($avtf)
		if  ("$t" == "$tf")  then
			set found=1
			break
		endif
	end
	if  ($found == 0)  then
		echo "Illegal transfer function class.  Operation aborted."
		exit
	endif
	foreach chan ($chanlist)
		foreach comp ($complist)
			echo "$station-$chan-$comp $tf" >>$lkfile
		end
	end

else

	echo ""
	echo ""
	echo "Transfer function entries found:"
	echo ""
	grep "^$station-" $lkfile
	echo ""
	echo -n "Change [y/n]: "
	set inp="$<"
	echo ""

	if  ("$inp" == "y")  then
		echo "List of transfers function classes:"
		echo "   $avtf"
		echo ""
		echo -n "Enter transfer function class for station ${station}: "
		set tf="$<"
		if  ("$tf" == "")  then
			echo "Empty input, operation aborted."
			exit
		endif
		set tf=`echo $tf | sed y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/`
		set found=0
		foreach t ($avtf)
			if  ("$t" == "$tf")  then
				set found=1
				break
			endif
		end
		if  ($found == 0)  then
			echo "Illegal transfer function class.  Operation aborted."
			exit
		endif
		if  (-e $lkfile.x)  \rm $lkfile.x
		grep -v "^$station-" $lkfile > $lkfile.x
		\rm $lkfile
		mv $lkfile.x $lkfile
		foreach chan ($chanlist)
			foreach comp ($complist)
				echo "$station-$chan-$comp $tf" >>$lkfile
			end
		end
	else
		echo "No changes made to transfer function assignment of station $upstat."
	endif

endif


# gain factors

echo ""
echo ""
echo "Gain factors of station $station, unit (nm/s)/count"
echo ""

cd $SEED_INPUTS

# convert stations, channels and components to lowercase
set station=`echo $station | sed y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/`
set chanlist=`echo $chanlist | sed y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/`
set complist=`echo $complist | sed y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/`

# first show current status
set found=0
set auto=1
foreach chan ($chanlist)
	foreach comp ($complist)
		set res=`ls -l seedcalib_$station-$chan-$comp | cut -c1-1`
		if  ("$res" == "")  then
			# do nothing
		else if  ("$res" == "l")  then
			set found=1
			printf "%9s: " $station-$chan-$comp
			set res=`ls -l seedcalib_$station-$chan-$comp`
			set link=$res[$#res]
			set res=`tail -1 seedcalib_$station-$chan-$comp`
			echo "$res (link to $link)"
		else
			set found=1
			printf "%9s: " $station-$chan-$comp
			tail -1 seedcalib_$station-$chan-$comp
			set lnum=`wc -l seedcalib_$station-$chan-$comp`
			set lnum=$lnum[1]
			set stime=`tail -1 seedcalib_$station-$chan-$comp | awk '{print $1}'`
			if  ($lnum > 1 || "$stime" != "$defstime")  set auto=0
		endif
	end
end

# if something found, ask for change
if  ($found == 1)  then
	echo ""
	echo -n "Change [y/n]: "
	set inp="$<"
	if  ("$inp" != "y")  then
		echo "No changes to gain factors of station $upstat."
		exit
	endif
endif

# if file found are all created by this program delete them
if  ($found == 1 && $auto == 1)  then
	echo "delete calibration files of station $upstat."
	\rm seedcalib_$station-*
	set found=0
endif

if  ($found == 0)  then

	# create new gain files, same for all components
	foreach chan ($chanlist)
		echo -n "Enter gain factor for station $station channel $chan [(nm/s)/count]: "
		set gain="$<"
		foreach comp ($complist)
			echo "$defstime $defetime $gain" >seedcalib_$station-$chan-$comp
		end
	end

else

	# the files found are not created by this program, edit manually
	foreach chan ($chanlist)
		foreach comp ($complist)
			set res=`ls -l seedcalib_$station-$chan-$comp | cut -c1-1`
			if  ("$res" == "l")  then
				echo "seedcalib_$station-$chan-$comp is link (not changed)"
			else
				echo "Edit gain factor file of $station-$chan-$comp ..."
				$SH_TEXTEDIT seedcalib_$station-$chan-$comp
			endif
		end
	end

endif


