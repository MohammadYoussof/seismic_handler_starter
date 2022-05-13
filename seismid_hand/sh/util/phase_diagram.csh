#! /bin/csh
#
# file phase_diagram.csh
#      =================
#
# version 4, 16-Sep-2005
#
# Plots phase diagram using GMT
# K. Stammler, 28-Mar-2003

if  ("$1" == "")  then
	echo "Usage: $0 <evtfile>"
	exit
endif

# get parameters
set evtfile=$1

#set echo

# set constants
set relfile=pdiag.000
set psfile=pdiag.ps

set collist = ( 255/0/0 0/0/255 0/255/0 255/0/255 \
	255/150/0 0/255/255 0/100/100 )

chdir $SH_SCRATCH

if  (! -e $evtfile)  then
	echo "$0 : input file $evtfile not found.  Abort."
	exit
endif

if  (-e $relfile)  \rm $relfile
$SH_UTIL/evt2phasetimes $evtfile >$relfile

set phaselist=`awk '{print $1}' $relfile | sort -u`

if  ($#phaselist > $#collist)  then
	echo "Cannot display all phases.  Abort."
	exit
endif

if  (-z $relfile)  then
	echo "$0 : $evtfile cannot be displayed.  Abort."
	\rm $relfile
	exit
endif
set res=`wc -l $relfile`
set res=$res[1]
if  ($res < 3)  then
	echo "$0 : Need at least 3 phases.  Abort."
	\rm $relfile
	exit
endif

# find bounds
set mintime=`awk '{print $3}' $relfile | sort -n | head -1`
set maxtime=`awk '{print $3}' $relfile | sort -n | tail -1`
set mindist=`awk '{print $4}' $relfile | sort -n | head -1`
set maxdist=`awk '{print $4}' $relfile | sort -n | tail -1`

set addtime=`echo "scale=2; ($maxtime - $mintime) / 10.0" | bc`
set adddist=`echo "scale=2; ($maxdist - $mindist) / 10.0" | bc`

set mintime=`echo "scale=2; $mintime - $addtime" | bc`
set maxtime=`echo "scale=2; $maxtime + $addtime" | bc`
set mindist=`echo "scale=2; $mindist - $adddist" | bc`
set maxdist=`echo "scale=2; $maxdist + $adddist" | bc`

set fmt=-JX5/5

if  (-e $psfile)  \rm $psfile
touch $psfile

set cnt = 0
set cont=""
foreach phase ($phaselist)

	@ cnt = $cnt + 1

	# fit line to data points
	set res=`$SH_UTIL/evt2phasetimes $evtfile | grep "^$phase " | awk '{print $3,$4}' | $SH_UTIL/fit_to_line`
	if  ($#res == 4 && "$res" != "NaN NaN NaN NaN")  then
		set lot=`$SH_UTIL/evt2phasetimes $evtfile | grep "^$phase" | awk '{print $3}' | sort -n | head -1`
		set hit=`$SH_UTIL/evt2phasetimes $evtfile | grep "^$phase" | awk '{print $3}' | sort -n | tail -1`
		set a=$res[1]
		set m=$res[2]
		set siga=$res[3]
		set sigm=$res[4]
		set minl=`echo "scale=5; $a + $m * $lot" | bc`
		set maxl=`echo "scale=5; $a + $m * $hit" | bc`
		printf "%f %f\n %f %f\n" $lot $minl $hit $maxl | \
			psxy $fmt -K $cont -R$mintime/$maxtime/$mindist/$maxdist >>$psfile
		set cont="-O"
	endif

	grep ^$phase $relfile | awk '{print $3,$4}' | \
		psxy $fmt -K $cont -B20:"Time since Origin (s)":/100:"Epi. Distance (km)"::."Phase Diagram": \
			-R$mintime/$maxtime/$mindist/$maxdist -Sc0.1 -G$collist[$cnt] >>$psfile
	grep ^$phase $relfile | awk '{print $3,$4,"8 0 0 5 \ ",$2}' | \
		pstext $fmt -O -K -R >>$psfile
	set cont="-O"
end

# loop all phases
set cnt = 0
set ypos=0.2
foreach phase ($phaselist)

	@ cnt = $cnt + 1

	# fit line to data points
	set res=`$SH_UTIL/evt2phasetimes $evtfile | grep "^$phase " | awk '{print $3,$4}' | $SH_UTIL/fit_to_line`
	if  ($#res < 4)  then
		set inf=""
	else if  ("$res" == "NaN NaN NaN NaN")  then
		set inf=""
	else
		set inf=`printf "(v=%5.2f +- %5.2f \\  d=%6.1f +- %6.1f)" $res[2] $res[4] $res[1] $res[3]`
	endif

	echo 1.3 $ypos | psxy -N $fmt -O -K -R0/1/0/1 -Sc0.1 -G$collist[$cnt] >>$psfile
	echo 1.35 $ypos 11 0 0 5 "$phase $inf" | pstext -N $fmt -O -K -R >>$psfile
	set ypos = `echo "scale=2; $ypos - 0.05" | bc`

end

# print epicenter info if available
set origin=`grep 'Origin time' $evtfile`
if  ($#origin > 3)  then
	set origin=$origin[4]
else
	set origin=""
endif
set lat=`grep '^Latitude' $evtfile`
if  ($#lat > 2)  then
	set lat=$lat[3]
else
	set lat=""
endif
set lon=`grep '^Longitude' $evtfile`
if  ($#lon > 2)  then
	set lon=$lon[3]
else
	set lon=""
endif
set srcreg="`grep '^Source region' $evtfile`"
if  ("$srcreg" != "")  then
	set srcreg=`echo "$srcreg" | cut -c26-80`
else
	set srcreg=""
endif
set ypos = `echo "scale=2; $ypos - 0.05" | bc`
echo 1.35 $ypos 11 0 0 5 "$origin" | pstext -N $fmt -O -K -R >>$psfile
set ypos = `echo "scale=2; $ypos - 0.05" | bc`
echo 1.35 $ypos 11 0 0 5 "epi: $lat, $lon" | pstext -N $fmt -O -K -R >>$psfile
set ypos = `echo "scale=2; $ypos - 0.05" | bc`
echo 1.35 $ypos 11 0 0 5 "$srcreg" | pstext -N $fmt -O -K -R >>$psfile

# dummy to close
echo 1.5 0.5 | psxy $fmt -O -R0/1/0/1 -Sc0.1 >>$psfile

if  ($?PSVIEW == 0)  then
	set pos=""
	if  ("`hostname`" == "ersn24")  set pos="-geometry 934x780+1+1"
	pageview -right $pos $psfile &
else
	$PSVIEW $psfile &
endif

\rm $relfile
