# to be sourced
#
# file socket_setup.s
#      ==============
#
# version 1, 20-Jul-2006
#
# Sets environment for socket use of SH
# K. Stammler, 20-Jul-2006

if  ("`uname`" == "SunOS")  then
	setenv SH_CCOPTIONS "-g -DSH_SETUP_AH -DSH_SETUP_SZGRF -DSH_SOCKET -I/usr/dt/include -I/usr/openwin/share/include"
	setenv SH_LINKLIBS "-lahio -lsocket -lnsl"
else
	# actually this was tested for Suse Linux only
	setenv SH_CCOPTIONS "-g -DSH_SETUP_LINUX -DSH_SOCKET -I/usr/X11R6/include"
endif
