#!/bin/csh

## argument given?
if ("$1" == "") then
	echo "Usage: md5check checksum.md5 [<chatty>]"
	exit
endif

#set echo

set checksum=$1
set chatty="$2"

if  ("`uname`" == "Linux")  then
	set md5prog=md5sum
	set md5col=1
else
	set md5prog=md5
	set md5col=4
endif

## checksum there?
if ( ! -f $checksum ) then
	echo "Checksum file not found\!"
	exit 1
endif

set lines=`wc -l $checksum | awk '{print $1}'`

@ fail=0
@ i=1
while ( $i <= $lines )
	set data = `awk NR==$i'{printf ("%s %s\n", substr($2,2,length($2)-2), $4)}' $checksum`
	if  ($#data < 2)  then
		set data = `awk NR==$i'{printf ("%s %s\n", $2, $1)}' $checksum`
	endif
	@ i=$i + 1

	## if error in input format skip line
	if ( $#data != 2 ) then
		continue
	endif

	set file = $data[1]
	set md5 = $data[2]

	if  ("$file:t" == "checksum.md5")  continue

	## test if file exists
	if ( ! -f $file ) then
		@ fail=$fail + 1
		echo "$file == NOT FOUND =="
		continue
	endif

	## make md5sum of file
	set datanew = `$md5prog $file`
	set md5new = $datanew[$md5col]

	if ( "$md5" != "$md5new" ) then
		@ fail=$fail + 1
		echo "$file == FAILED =="
		continue
	endif

	if  ("$chatty" != "")  echo "$file == OK =="
end

if ( $fail > 0 ) then
	echo "$fail file(s) failed"
	exit 1
endif
