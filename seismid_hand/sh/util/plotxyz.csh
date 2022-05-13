#! /bin/csh
#
# file plotxyz.csh
#      ===========
#
# version 1, 11-Mar-2005
#
# Plots xyz data
# K. Stammler, 11-Mar-2005

if  ("$1" == "")  then
	echo "Usage: $0 <xyzfile>"
	exit
endif

#set echo

# get parameters
set xyzfile=$1

if  (! -e $xyzfile)  then
	echo "Input file $xyzfile not found.  Abort."
	exit
endif

# set constants
set psfile=xyz.ps

# find extrema
set zmin=`awk '{print $3+0}' $xyzfile | sort -n | head -1`
set zmax=`awk '{print $3+0}' $xyzfile | sort -n | tail -1`
set xmin=`awk '{print $1+0}' $xyzfile | sort -n | head -1`
set xmax=`awk '{print $1+0}' $xyzfile | sort -n | tail -1`
set ymin=`awk '{print $2+0}' $xyzfile | sort -n | head -1`
set ymax=`awk '{print $2+0}' $xyzfile | sort -n | tail -1`

# if zmax-zmin too small add some space
set res=`echo "scale=1; ($zmax-$zmin)*10" | bc`
set res=`printf "%3.0f" $res`
if  ($res < 3)  then
	set zmax=`echo "scale=2; $zmin+0.3" | bc`
endif

# position of maximum
set res=`awk '{print $1+0,$2+0,$3+0}' $xyzfile | sort -k 3 -n | tail -1`
set maxxpos=$res[1]
set maxypos=$res[2]
set size=10.0
set xmin2=`echo "scale=1; $maxxpos - $size" | bc`
set xmax2=`echo "scale=1; $maxxpos + $size" | bc`
set ymin2=`echo "scale=1; $maxypos - $size" | bc`
set ymax2=`echo "scale=1; $maxypos + $size" | bc`

if  (-e pxyz.cpt)  \rm pxyz.cpt
sed "s/ZMIN/$zmin/g" $SH_UTIL/xyz.cpt | sed "s/ZMAX/$zmax/g" >pxyz.cpt

if  (-e $psfile)  \rm $psfile
touch $psfile
awk '{print $1+0,$2+0,$3+0,$3+0}' $xyzfile | \
	psxyz -JX3 -JZ2.5 -R$xmin/$xmax/$ymin/$ymax/$zmin/$zmax \
	-Cpxyz.cpt -Ss0.02 \
	-E110/25 -Ba30f10/a60f10/a0.2 -K >>$psfile
awk '{print $1+0,$2+0,$3+0,$3+0}' $xyzfile | \
	psxyz -X5 -JX3 -JZ2.5 -R$xmin/$xmax/$ymin/$ymax/$zmin/$zmax \
	-Cpxyz.cpt -Ss0.02 \
	-E290/25 -Ba30f10/a60f10/a0.2 -O -K >>$psfile
awk '{print $1+0,$2+0,$3+0,$3+0}' $xyzfile | \
	psxyz -X-5 -Y3.8 -JX3 -JZ2 -R$xmin2/$xmax2/$ymin2/$ymax2/$zmin/$zmax \
	-Cpxyz.cpt -Ss0.05 \
	-E110/25 -Ba5/a5/a0.2 -O -K >>$psfile
awk '{print $1+0,$2+0,$3+0,$3+0}' $xyzfile | \
	psxyz -X5 -JX3 -JZ2 -R$xmin2/$xmax2/$ymin2/$ymax2/$zmin/$zmax \
	-Cpxyz.cpt -Ss0.05 \
	-E290/25 -Ba5/a5/a0.2 -O -K >>$psfile
echo "$xmin2 $ymax2 15 0 4 7 $xyzfile\ $maxxpos\ $maxypos" | \
	pstext -JX3 -R$xmin2/$xmax2/$ymin2/$ymax2 -Y0.3 -O -N >>$psfile

kghostview $psfile &

