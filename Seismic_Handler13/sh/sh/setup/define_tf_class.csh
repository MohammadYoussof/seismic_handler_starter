#! /bin/csh
#
# file define_tf_class.csh
#      ===================
#
# version 1, 15-Nov-2003
#
# Defines new transfer function class for SHM
# K. Stammler, 15-Nov-2003

set xprog=$SH_ROOT/setup

echo -n "Enter new class name: "
set class="$<"

set class=`echo $class | sed y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/`

set outname=$SH_FILTER/TF_VEL_$class.FLF

set zero_re = ( )
set zero_im = ( )
set pole_re = ( )
set pole_im = ( )

# get zeros
set cnt=1
while  (1 > 0)
	echo -n "zero $cnt real part (empty input stops): "
	set inp="$<"
	if  ("$inp" == "")  break
	set zero_re = ( $zero_re $inp )
	echo -n "zero $cnt imaginary part               : "
	set inp="$<"
	set zero_im = ( $zero_im $inp )
	@ cnt = $cnt + 1
end

echo ""

# get poles
set cnt=1
while  (1 > 0)
	echo -n "pole $cnt real part (empty input stops): "
	set inp="$<"
	if  ("$inp" == "")  break
	set pole_re = ( $pole_re $inp )
	echo -n "pole $cnt imaginary part               : "
	set inp="$<"
	set pole_im = ( $pole_im $inp )
	@ cnt = $cnt + 1
end

# normalization
echo ""
echo -n "Enter normalization factor: "
set norm="$<"

if (-e $outname)  \rm $outname
touch $outname

echo "! transfer function class $class, velocity input"  >>$outname
echo "1357913578"                                        >>$outname
echo "1"                                                 >>$outname
echo "$norm"                                             >>$outname
echo $#zero_re                                           >>$outname
set cnt=1
foreach zero ($zero_re)
	echo "($zero_re[$cnt],$zero_im[$cnt])"                >>$outname
	@ cnt = $cnt + 1
end
echo $#pole_re                                           >>$outname
set cnt=1
foreach pole ($pole_re)
	echo "($pole_re[$cnt],$pole_im[$cnt])"                >>$outname
	@ cnt = $cnt + 1
end

echo ""
echo ""
echo "created file $outname"
echo ""
cat $outname
echo ""

$SH_UTIL/prep_simfilters.csh $class
