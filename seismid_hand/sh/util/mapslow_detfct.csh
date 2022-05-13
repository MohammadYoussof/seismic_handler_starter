#! /bin/csh
#
# file mapslow_detfct.csh
#      ==================
#
# version 1, 12-Dec-2003
#
# Shows detection functions from fk detection files using GMT.
# K. Stammler, 12-Dec-2003

if  ("$5" == "")  then	
	echo "Usage: $0 <detfile> <timewidth> <baz> <mineval> <detpar>"
	exit
endif

# get parameters
set detfile=$1
set timewidth=$2
set baz=$3
set mineval=$4
set detpar=$5

# set constants
set sprog=/home/klaus/runfk/source
set tmpfile=$HOME/detfct_$$.000
set psfile=$HOME/detfct_$$.ps
set proj=-JX15/3

set path = ( /programs/sol2/gmt-3.3.4/bin $path )

if  (! -e $detfile)  then
	echo "$0 : file $detfile not found.  Abort."
	exit
endif

echo $detfile $timewidth $baz $detpar

if  (-e $tmpfile)  \rm $tmpfile
touch $tmpfile

# call evalfct to get evaluation function, put result to tmpfile
$sprog/evalfct $detfile 0.0 $baz $detpar >>$tmpfile
set tmp=`wc -l $tmpfile`
set tmp=$tmp[1]
set x=`wc -l $detfile`
set x=$x[1]
if  ($tmp != $x)  then
	echo "$0 : evalfct produced too less lines ($tmp < $x).  Abort"
	nedit $detfile &
	nedit $tmpfile
	\rm $tmpfile
	exit
endif

# add original qual, relpow and abspow entries to tmpfile
if  (-e $tmpfile.x)  \rm $tmpfile.x
awk '{print $3}' $detfile | paste $tmpfile - >$tmpfile.x
\rm $tmpfile
mv $tmpfile.x $tmpfile
awk '{print $6}' $detfile | paste $tmpfile - >$tmpfile.x
\rm $tmpfile
mv $tmpfile.x $tmpfile
awk '{print $7}' $detfile | paste $tmpfile - >$tmpfile.x
\rm $tmpfile
mv $tmpfile.x $tmpfile

# now we have a file with columns:
# 1. abolute time
# 2. relative time
# 3. azimuth deviation
# 4. slowness
# 5. evaluation
# 6. qual
# 7. relpow
# 8. abspow

if (-e $psfile)  \rm $psfile
touch $psfile

set O=""

set maxdsp=`echo "scale=2; $mineval * 2" | bc`
set vba=5
set vbf=1
set tmp=`echo $maxdsp | sed 's/\./ /'`
set tmp=$tmp[1]
if  ($tmp > 40)  then
	set vba=20
	set vbf=5
else if  ($tmp < 10)  then
	set vba=0.5
	set vbf=0.1
endif

set cmd_list =   ( '{print $2,$6}' '{print $2,$7}' '{print $2,$8}' '{print $2,$3}' '{print $2,$4}' '{print $2,$5}' )
set ba_list =    ( 1000            0.25            20              30              10              $vba )
set bf_list =    ( 500             0.05            5               5               2               $vbf )
set title_list = ( "qual"          "relpow"        "abspow"        "bazdif"        "slow"          "eval" )
set gx_list =    ( 4               0               0               0               0               0 )
set gy_list =    ( 2.5             4.3             4.3             4.3             4.3             4.3 )
set min_list =   ( 0               0               10              0              -10              0 )
set max_list =   ( 3000            1               80              90              25              $maxdsp )

set cnt=1
set xtitle="time in s"
foreach title ($title_list)

	set cmd="$cmd_list[$cnt]"
	set ba=$ba_list[$cnt]
	set bf=$bf_list[$cnt]
	set gx=$gx_list[$cnt]
	set gy=$gy_list[$cnt]
	set min=$min_list[$cnt]
	set max=$max_list[$cnt]
	awk "$cmd" $tmpfile | psxy -K $O $proj -P -R0.0/$timewidth/$min/$max \
		-Ba500f100:"$xtitle":/a${ba}f${bf}:"$title":WSne -X$gx -Y$gy \
		| grep -v setpagedevice >>$psfile
	set O="-O"
	set xtitle=" "
	@ cnt = $cnt + 1

end

echo "showpage" >>$psfile
pageview $psfile &

sleep 3
\rm $tmpfile $psfile

#awk '{print $3}' $detfile
