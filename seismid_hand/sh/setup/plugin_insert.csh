#! /bin/csh
#
# file plugin_insert.csh
#      =================
#
# version 1, 25-Nov-2003
#
# Inserts plugin package into menu
# K. Stammler, 25-Nov-2003

set plist = ( `(cd $SH_ROOT/plugins; ls *.tar)` )
set setup=$SH_USERROOT/private/external_processes.txt

echo ""
echo "Available plugins are:"
echo ""
echo "Number    Name"
echo ""
set cnt=1
foreach p ($plist)
	printf "%2d        %s\n"  $cnt $p:r
	@ cnt = $cnt + 1
end
echo ""
echo -n "Enter plugin number to install: "
set plgnum="$<"
if  ($plgnum < 1 || $plgnum > $#plist)  then
	echo "Illegal plugin number $plgnum. Abort."
	exit
endif

echo ""
echo ""
echo "Currently the following plugin menu entries are used:"
$SH_ROOT/setup/plugin_list.csh

echo -n "Enter menu position number to use for new plugin [1-10]: "
set posnum="$<"

if  ($posnum < 1 || $posnum > 10)  then
	echo "Illegal menu position number $posnum.  Abort."
	exit
endif

$SH_ROOT/setup/plugin_delete.csh $posnum

if  (! -e $SH_USERROOT/private/external_processes.txt)  then
	cp $SH_ROOT/plugins/header_text.txt $setup
endif

cd $SH_USERROOT/private
set plgname=$plist[$plgnum]
tar xf $SH_ROOT/plugins/$plgname
set plgname=$plgname:r

sed "s/##NUM##/$posnum/" $plgname.plg >>$setup
\rm $plgname.plg
