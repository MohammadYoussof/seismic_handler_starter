#! /bin/csh -f
#
# file get_screen_resolution.csh
#      =========================
#
# version 1, 14-Oct-2005
#
# Returns resolution of default display and reolution name
# K. Stammler, 14-Oct-2005

#set res=`xprop -root | grep DESKTOP_GEOMETRY\( | sed 's/,//'`
set w=`xwininfo -root | grep Width:`
set h=`xwininfo -root | grep Height:`
if  ($#w < 2 || $#h < 2)  then
	echo "0 0 unknown"
	exit
endif

set w=$w[2]
set h=$h[2]

set name=vga
if  ($w >= 800 && $h >= 600) set name=svga
if  ($w >= 1024 && $h >= 768) set name=xga
if  ($w >= 1270 && $h >= 768) set name=wxga2
if  ($w >= 1270 && $h >= 800) set name=wxga
if  ($w >= 1280 && $h >= 1024) set name=sxga
if  ($w >= 1400 && $h >= 1050) set name=sxga+
if  ($w >= 1600 && $h >= 1200) set name=uxga
if  ($w >= 2048 && $h >= 1536) set name=qxga

echo "$w $h $name"
