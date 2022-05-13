#! /bin/csh
#
# file filtgraph.csh
#      =============
#
# version 3, 28-May-2002
#
# graphic display of filter amplitudes
# K. Stammler, 4-Nov-98

if  ("$1" == "")  then
	echo "Usage: $0 <filter> [<lofrq>] [<hifrq>] [<loamp>] [<hiamp>] [<title>]"
	exit
endif

#set echo

# get parameters
set filter=$1
set lofrq=$2
set hifrq=$3
set loamp=$4
set hiamp=$5
set title="$6"

# set constants
set progpath=$SH_UTIL
set tmpfile=frqtmp.000
set psfile=fg.ps
#set fs=-JX2.5l/2.5l
set fs=-JX10l/10l
set pw=3
set xoff=3
set yoff=3
set collist = ( 255/0/0 0/100/255 0/255/0 \
	255/0/255 125/125/0 30/45/180 0/255/255 )
#set collist = ( 0/0/0 0/0/0 0/0/0 0/0/0 0/0/0 0/0/0 0/0/0 0/0/0 0/0/0 )

if  ("$lofrq" == "")  set lofrq = 1.0e-2
if  ("$hifrq" == "")  set hifrq = 1.0e3
if  ("$loamp" == "")  set loamp = 1.0e-5
if  ("$hiamp" == "")  set hiamp = 1.0e5

set frqlist=`$progpath/logseries $lofrq $hifrq 1.1`
set fillist=`echo $filter | sed 's/,/ /g'`
if  (-e $psfile)  \rm $psfile
touch $psfile
set first=1
set col=1

foreach fil ($fillist)

	if  (-e $tmpfile)  \rm $tmpfile
	touch $tmpfile
	foreach frq ($frqlist)
		set res=`$SH_UTIL/evalflf $fil $frq`
		echo $frq $res[1] | grep -v 0.000000e+00 >>$tmpfile
	end

	if  ($col > $#collist)  then
		echo "too less colors defined.  Abort."
		exit
	endif

	if  ($first == 1)  then
		psxy $tmpfile $fs -R$lofrq/$hifrq/$loamp/$hiamp -X$xoff -Y$yoff \
			-W$pw/$collist[$col] -P -K \
			-Ba100f50:"Frequency in Hz":/a100f50:".$title":WeSn >>$psfile
	else
		psxy $tmpfile $fs -R$lofrq/$hifrq/$loamp/$hiamp \
			-W$pw/$collist[$col] -P -O -K >>$psfile
	endif

	set first=0
	@ col = $col + 1

end

# print filter names in color
set col=1
set xtext=1.05
set ytext=1
foreach fil ($fillist)
	#set filname=`echo $fil | sed 's/TF_DSP_//'`
	set filname=$fil
	pstext -JX10/10 -G$collist[$col] -N -R0/1/0/1 -P -O -K <<EOF >>$psfile
      $xtext $ytext 9 0.0 0 0 $filname
EOF
	set ytext=`echo "scale=3; $ytext - 0.07" | bc`
	@ col = $col + 1
end

echo "showpage" >>$psfile

\rm $tmpfile

$PSVIEW $psfile &

