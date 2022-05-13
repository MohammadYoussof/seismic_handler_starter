#! /bin/csh
#
# file mapslow_fk.csh
#      ==============
#
# version 1, 12-Dec-2003
#
# Show FK Diagrams from mapslow window
# K. Stammler, 12-Dec-2003

if  ("$3" == "")  then
	echo "Usage: $0 <modenum> <time> <baz>"
	exit
endif

# get parameters
set modenum=$1
set dtime=$2
set baz=$3

# set constants
set shcmd=MAPSLOW_FK_$$.SHC

chdir $HOME

if  ($modenum == 1)  then
	set slist=grf
	set comp=z
	set chan=bh
	set wdw=20
	set filter=grf_s+g_wwssn_sp
	set filoff=10
	set lofrq=0.4
	set hifrq=3
	set maxslo=15.0
else if  ($modenum == 2)  then
	set slist=grsn
	set comp=z
	set chan=lh
	set wdw=140
	set filter=grsn_s+kirnos
	set filoff=60
	set lofrq=0.02
	set hifrq=0.5
	set maxslo=25.0
else if  ($modenum == 3)  then
	set slist=grsn
	set comp=t
	set wdw=140
	set chan=lh
	set filter=grsn_s+kirnos
	set filoff=60
	set lofrq=0.02
	set hifrq=0.5
	set maxslo=25.0
else if  ($modenum == 4)  then
	set slist=grsn
	set comp=r
	set chan=lh
	set wdw=140
	set filter=grsn_s+kirnos
	set filoff=60
	set lofrq=0.02
	set hifrq=0.5
	set maxslo=25.0
else if  ($modenum == 5)  then
	set slist=grsn
	set comp=z
	set chan=lh
	set wdw=100
	set filter=grsn_s+sro_lp
	set filoff=600
	set lofrq=0.01
	set hifrq=0.2
	set maxslo=35.0
else if  ($modenum == 6)  then
	set slist=grsn
	set comp=t
	set chan=lh
	set wdw=100
	set filter=grsn_s+sro_lp
	set filoff=600
	set lofrq=0.01
	set hifrq=0.2
	set maxslo=35.0
else if  ($modenum == 7)  then
	set slist=grsn
	set comp=r
	set chan=lh
	set wdw=100
	set filter=grsn_s+sro_lp
	set filoff=600
	set lofrq=0.01
	set hifrq=0.2
	set maxslo=35.0
else
	echo "Illegal modenum $modenum. Abort."
	exit
endif

set rtime=`$SH_UTIL/timename time_subsec $dtime $wdw`
set rtime=`$SH_UTIL/timename time_subsec $dtime $filoff`
set rlth=`echo "scale=2; $wdw + $filoff" | bc`
set today=`$DPROG/get_yesterday.csh -1`
set daysold=`$SH_UTIL/timename time_daydiff $today $rtime`
if  ($daysold < 40)  then
	set rpath="sfd:"
else
	set rpath="jk:"
endif

if  (-e $shcmd)  \rm $shcmd
touch $shcmd

echo "xopen;;"  >>$shcmd
echo "sdef trcnum" >>$shcmd
if  ("$comp" == "z")  then
	echo "reads $rpath $rtime $rlth $slist z $chan" >>$shcmd
else
	echo "readhoriz $rpath $rtime $rlth $slist $comp $chan $baz" >>$shcmd
endif
echo 'title 1 |please|$blank|wait|$blank|...|'  >>$shcmd
echo 'calc i &trcnum = $dsptrcs'  >>$shcmd
echo "fili f $filter" >>$shcmd
echo 'filter f all' >>$shcmd
echo 'del |1-|"trcnum|' >>$shcmd
echo "cut all $filoff $rlth" >>$shcmd
echo 'rd'        >>$shcmd
#echo "time &g1"  >>$shcmd
echo "fk $lofrq $hifrq $maxslo 101" >>$shcmd
echo "quit y"    >>$shcmd

cat $shcmd

SH <$shcmd
\rm $shcmd

setenv UIDPATH $SH_SOURCE/img/mapmatrix.uid
$SH_SOURCE/img/mapmatrix fk_signal.out 10 $maxslo &
sleep 3

\rm fk_signal.*


