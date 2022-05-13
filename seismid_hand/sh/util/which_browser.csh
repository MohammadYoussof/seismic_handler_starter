#! /bin/csh
#
# file which_browser.csh
#      =================
#
# version 1, 18-Dec-2005
#
# finds which browser is available
# K. Stammler, 19-Dec-2005

set browser=""

if  ("`uname`" == "SunOS")  then
	if  (-e /programs/sol2/netscape-7.0/SUNWns/netscape) \
		set browser="/programs/sol2/netscape-7.0/SUNWns/netscape"
	if  ("$browser" == "")  then
		set tmp=`which netscape`
		if  (-e "$tmp")  set browser="netscape"
	endif
else
	set tmp=`which firefox`
	if  (-e "$tmp")  set browser="firefox"
	if  ("$browser" == "")  then
		set tmp=`which mozilla`
		if  (-e "$tmp")  set browser="mozilla"
	endif
	if  ("$browser" == "")  then
		set tmp=`which konqueror`
		if  (-e "$tmp")  set browser="konqueror"
	endif
endif

if  ("$browser" == "")  then
	set browser="no-browser-found"
endif

echo $browser
