#! /bin/csh
#
# file epicenter_map.csh
#      =================
#
# version 1, 18-Jan-2002
#
# Plot epicenter map
# K. Stammler, 18-Jan-2002

if  ("$1" == "")  then
	echo "Usage: $0 <evtfile>"
	exit
endif

#set echo

# get parameters
set evt=$1

# set constants
set lathw=1.5    # latitude half width
set lonhw=2.5    # longitude half width
set psfile=epimap.ps
set grd=map.grd
set magn="1:2500000"
set citycol=200/0/200
set bordercol=50/255/50
set epicol=255/0/0
set black=0/0/0
set gmtp=/programs/sol2/gmt-3.3.4/bin

set topo=0
set tmp=$0
if  ("$tmp:t" == "topo_map.csh")  set topo=1

if  (! -e $evt)  then
	echo "$0 : Input evtfile $evt not found.  Abort."
	exit
endif

set lat=`grep ^Latitude $evt`
set lon=`grep ^Longitude $evt`

if  ($#lat < 3  ||  $#lon < 3)  then
	echo "$0 : No location found in Evt-file.  Abort."
	exit
endif
set lat=`echo $lat[3] | sed 's/^+//'`
set lon=`echo $lon[3] | sed 's/^+//'`

set mappar="-Jt$lon/$magn"
set title="EpiMap ($lat,$lon)"

set latmin=`echo "scale=3; $lat - ($lathw)" | bc`
set latmax=`echo "scale=3; $lat + ($lathw)" | bc`
set lonmin=`echo "scale=3; $lon - ($lonhw)" | bc`
set lonmax=`echo "scale=3; $lon + ($lonhw)" | bc`
set area=$lonmin/$lonmax/$latmin/$latmax

# create postscript output
if  (-e $psfile)  \rm $psfile
touch $psfile

set iflag=""
set fillcol=-G255/255/239

# topographic map
if  ($topo == 1)  then
	set iflag=-O
	set fillcol=""
	if  (-e $grd)  \rm $grd
	$gmtp/grdraster 16 -G$grd -R$area -I0.5m $mappar
	$gmtp/grdimage $grd $mappar -R$area -C$SH_UTIL/col.cpt -P -K  | \
		grep -v setpagedevice >>$psfile
#	-Y5.5 -X4.0
endif

# plot coastlines and borders
pscoast $mappar -R$area -A10 -B1.0g0.5f0:."$title": \
	-Df $fillcol -S230/248/255 -N1/8/$bordercol -W0.1/255/255/255/solid \
	-P -Ir/0.1/0/0/255 -K $iflag >>$psfile

# plot cities
awk '{print $1,$2}' $SH_UTIL/cities.dat \
	| psxy -: -O -K -R $mappar -W5/$citycol -G$citycol -Sc0.1 >>$psfile
awk '{print $1,$2,"9","0","4","1","\ ",$3}' $SH_UTIL/cities.dat \
	| pstext  -: -O -K -R $mappar -G$black >>$psfile

echo "$lat $lon" \
	| psxy -: -O -R $mappar -W5/$epicol -G$epicol -Sa0.1 >>$psfile

pageview $psfile &

echo $0:t
