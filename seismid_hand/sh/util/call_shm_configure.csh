#! /bin/csh
#
# file call_shm_configure.csh
#      ======================
#
# version 1, 28-Dec-96
#
# Calls shm_configure menu in a separate xterm window.
# K. Stammler, 28-Dec-96

#xterm -fn 6x10 -geometry 80x34x40x200 +aw -sl 132 -sb -e $SH_UTIL/shm_configure.csh &
xterm +aw -sl 132 -sb -e $SH_UTIL/shm_configure.csh
#cmdtool -geometry 132x128x0x0 $SH_UTIL/shm_configure.csh &
