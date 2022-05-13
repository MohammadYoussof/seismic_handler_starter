#! /bin/csh
#
# file prep_simfilters.csh
#      ===================
#
# version 3, 3-Dec-2005
#
# Prepare simulation filters for a broadband recording instrument.
# K. Stammler, 15-May-2003

if  ("$1" == "")  then
	echo "Usage: $0 <bbname>"
	echo "   Example $0 CMG40T"
	exit
endif

# get parameters
set bbname=$1

# set constants
set filist = ( G_WWSSN_SP WWSSN_LP WOODAND LRSM_SP LRSM_LP SRO_LP KIRNOS )
#set filist = ( G_WWSSN_SP WWSSN_LP )
set addfil="ADDFIL"

if  ($?PSVIEW == 0)  then
	echo "$0 : Environment variable PSVIEW (PostScript display program) not set."
	exit
endif
set tmp=`$SH_UTIL/logseries 2 2 2 | cut -c1-3`
if  ("$tmp" != "2.0")  then
	echo "$0 : logseries not usable (in SH_UTIL)"
	echo '   try'
	echo '   cd $SH_UTIL'
	echo '   make logseries'
	exit
endif

# look for the transfer function of the input instrument
set tf_inp=$SH_FILTER/TF_VEL_S+$bbname.FLF
if  (-e $tf_inp)  then
	echo ""
	echo Using the following transfer function:
	echo ""
	cat $tf_inp
	echo ""
else
	echo ""
	echo "Need the following file: $tf_inp"
	echo ""
	exit
endif

# create new GMT defaults file
echo "creating new .gmtdefaults file"
if  (-e .gmtdefaults)  \rm .gmtdefaults
gmtdefaults -Ds >.gmtdefaults

echo ""
foreach f ($filist)

	set simfil=${bbname}_S+$f
	echo "processing file $f (creating $simfil.FLF)"

	set filtype=""
	set frq=""
	set frq2=""
	set cmd=""
	# loop until user accepts filter
	while  ("$cmd" != "a")

		if  (-e $simfil.FLF)  \rm $simfil.FLF
		$SH_UTIL/catflf TF_VEL_S+$f div TF_VEL_S+$bbname >$simfil.FLF
		if  ("$filtype" != "")  then
			# create additional HP or LP filter and apply it
			if  (-e ${simfil}_X.FLF)  \rm ${simfil}_X.FLF
			mv $simfil.FLF ${simfil}_X.FLF
			if  (-e $addfil.FLF)  \rm $addfil.FLF
			$SH_UTIL/butfreq $filtype $inptype $frq $frq2 -f=$addfil -o=$order
			$SH_UTIL/catflf ${simfil}_X mul $addfil >$simfil.FLF
			\rm ${simfil}_X.FLF 
		endif

		# show current filter
		$SH_UTIL/filtgraph.csh TF_VEL_S+$bbname,TF_VEL_S+$f,${bbname}_S+$f \
			1e-4 1e4 1e-7 100
		echo ""
		echo ""
		echo "current filter $simfil is:"
		echo ""
		cat ${bbname}_S+$f.FLF
		echo ""
		echo -n "apply Highpass, Lowpass, Bandpass, Accept or Delete (hp,lp,bp,a,d): "
		set filtype=$<
		if  ("$filtype" == "a")  break
		if  ("$filtype" == "d")  then
			echo "deleting filter $simfil and continue"
			\rm $simfil.FLF
			break
		endif
		if  ("$filtype" != "lp" && "$filtype" != "hp" && "$filtype" != "bp")  then
			set filtype=""
			set frq=""
			set order=""
			echo "illegal input.  Repeat"
			continue
		endif
		if  ("$filtype" == "hp")  then
			echo -n "enter Highpass frequency in s: "
			set inptype=t
			set frq=$<
			set frq2=""
		else if  ("$filtype" == "lp")  then
			echo -n "enter Lowpass period in Hz: "
			set inptype=f
			set frq=$<
			set frq2=""
		else
			echo -n "enter lower boundary in Hz: "
			set inptype=f
			set frq=$<
			echo -n "enter upper boundary in Hz: "
			set frq2=$<
		endif
		echo -n "enter order of filter (default: 2): "
		set order=$<
		if  ("$order" == "")  set order=2
	end

end

if  (-e fg.ps)  \rm fg.ps
if  (-e ADDFIL.FLF)  \rm ADDFIL.FLF

