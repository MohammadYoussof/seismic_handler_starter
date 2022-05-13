#! /bin/csh
#
# file compute_beams.csh
#      =================
#
# version 1, 12-Mar-2005
#
# Computes beam shifts
# K. Stammler, 12-Mar-2005

if  ("$1" == "")  then
	echo "Usage: $0 <beamlocfile>"
	exit
endif

# get parameters
set f_beam=$1

#set echo

# set constants
set statlist = ( arsa bfo brg bsd bseg bud bug cll cop crvs dava fur gkp grfo \
	gunz hlg ibbn kba khc ksp kwp moa morc mox mud nkc nott nrdl obka ojc okc \
	pksm pru psz pvcc rgn rue sop stu suw tirr tns vyhs war werd wet wlf )
#set phaselist = ( Pg Pn Sg Sn )
set phaselist = ( P PP S SS PKPdf PKPab PKPbc SKSac ScS PS SP )
set depth=10
set dt=2.0

if  (! -e $f_beam)  then
	echo "$0 : input file $f_beam not found."
	exit
endif

# clear output files
foreach s ($statlist)
	foreach p ($phaselist)
		if  (-e beam_${s}_$p.dat)  \rm beam_${s}_$p.dat
		touch beam_${s}_$p.dat
	end
end

set cnt=1
while  (1 > 0)  # forever
	set line=`sed -n "$cnt"p $f_beam`
	if  ("$line" == "")  break
	if  ($#line != 2)  then
		echo "$0 : illegal line $line in $f_beam.  Abort."
		exit
	endif
	set lat=$line[1]
	set lon=$line[2]
	set xlat=`echo $lat | sed 's/^-/s/'`
	set xlon=`echo $lon | sed 's/^-/w/'`
	echo "$line"
	foreach s ($statlist)
		set dist=`$SH_UTIL/locdiff $xlat $xlon $s | grep distance | awk '{print $2}'`
		foreach p ($phaselist)
			set trav=`$SH_UTIL/traveltime -q $p $dist $depth`
			set bm=`echo "scale=2; $trav / $dt" | bc`
			set ibm=`printf "%4.0f" $bm`
			#echo $s $p $trav $bm $ibm
			echo "$ibm" >>beam_${s}_$p.dat
		end
	end
	@ cnt ++
end


