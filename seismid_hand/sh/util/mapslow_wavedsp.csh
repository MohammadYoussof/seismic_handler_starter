#! /bin/csh
#
# file mapslow_wavedsp.csh
#      ===================
#
# version 3, 6-Jul-2005
#
# Display beam traces on screen.
# K. Stammler, 13-Mar-2003

if  ("$4" == "")  then
	echo "Usage: $0 <mode> <time> <slow> <baz> [<otime> <dist> <depth>]"
	exit
endif

# get parameters
set mode=$1
set dtime=$2
set slow=$3
set baz=$4
set otime=$5
set dist=$6
set depth=$7

#set rlist=bfo,tns,wet,mox,bug,brg,cll,fur
set rlist=bfo,tns,wet,mox,bug,brg,cll,fur,stu,bseg,rue

set slow=`printf "%4.1f" $slow`
set baz=`printf "%5.1f" $baz`

if  ("$mode" == 1)  then
	# show grf traces
	set slist=grf
	set seclth=120
	set cutlth=5
	set filter=grf_s+g_wwssn_sp
	set comp=z
else if  ("$mode" == 2)  then
	set slist=$rlist
	set seclth=300
	set cutlth=180
	set filter=grsn_s+kirnos
	set comp=z
else if ("$mode" == 3)  then
	set slist=$rlist
	set seclth=300
	set cutlth=180
	set filter=grsn_s+kirnos
	set comp=t
else if ("$mode" == 4)  then
	set slist=$rlist
	set seclth=300
	set cutlth=180
	set filter=grsn_s+kirnos
	set comp=r
else if ("$mode" == 5)  then
	set slist=$rlist
	set seclth=900
	set cutlth=600
	set filter=grsn_s+sro_lp
	set comp=z
else if ("$mode" == 6)  then
	set slist=$rlist
	set seclth=900
	set cutlth=600
	set filter=grsn_s+sro_lp
	set comp=t
else if ("$mode" == 7)  then
	set slist=$rlist
	set seclth=900
	set cutlth=600
	set filter=grsn_s+sro_lp
	set comp=r
else
	echo "$0 : illegal mode"
	exit
endif

if  ("`uname`" == "Linux")  then

$SH_ROOT/shc_world <<EOF
xopen;;
mapslow_wavedsp_lnx $slist $dtime $seclth $cutlth $slow $baz $filter $otime $dist $depth $comp
quit y
EOF

else

#SH <<EOF
$SH_ROOT/shc_world <<EOF
xopen;;
mapslow_wavedsp $slist $dtime $seclth $cutlth $slow $baz $filter $otime $dist $depth $comp
quit y
EOF

endif
