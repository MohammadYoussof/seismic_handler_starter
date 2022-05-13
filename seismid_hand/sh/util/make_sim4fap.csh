#! /bin/csh
#
#  file make_inv_flt.csh
#       ================
#
# version 1, 23-May-2005
#
# Reads FAP file and computes simulation filter
# K. Stammler, 23-May-2005

if  ("$2" == "")  then
	echo "Usage: $0 <fap-input> <simfilter>"
	exit
endif

# get parameters
set fap=$1
set simfil=$2

if  (! -e $fap)  then
	echo "input file $fap not found.  Abort."
	exit
endif

# header of flt filter file
echo "! created by $0 $*"
echo "1357913578"
echo "2"
set linenum=`wc -l $fap`
set linenum=$linenum[1]
echo "$linenum"

set cnt=1
while  (1 > 0)

	# read and parse next line of fap file
	set line=`sed -n $cnt"p" $fap`
	if  ("$line" == "")  break
	if  ($#line != 3)  then
		echo "Illegal syntax in $fap, line $cnt"
		exit
	endif
	set frq=$line[1]
	set amp=$line[2]
	set phase=`echo $line[3] | sed 's/^-/m/'`
	#set phase=`$DPROG/floatop $phase div 57.2958 | sed 's/^-/m/'`

	# compute transfer function of sim. instrument at given frequency
	set res=`$SH_UTIL/evalflf $simfil $frq`
	if  ($#res != 2)  then
		echo "Illegal output of evalflf, line $cnt"
		exit
	endif
	set simamp=$res[1]
	set simphase=`echo $res[2] | sed 's/^-/m/'`
	set simphase=`$DPROG/floatop $simphase mul 57.2958 | sed 's/^-/m/'`

	# divide sim. instrument by recording instrument
	set outamp=`$DPROG/floatop $simamp div $amp`
	set outphase=`$DPROG/floatop $simphase minus $phase`

	echo "$frq $outamp $outphase"

	@ cnt ++
end

