# file Makefile
#      ========
#
# version 11, 4-Aug-2000
#
# Makefile of directory $SH_SOURCE/newgraph
# K. Stammler, 7-Mar-95

#CFLAGS      = $(SH_CCOPTIONS) -I$(SH_SOURCE) -I$(SH_UTIL) -I$(SH_SOURCE)/newgraph -I$(SH_SOURCE)/foreign -DBASECNST=\"basecnst.h\"
CFLAGS      = $(SH_CCOPTIONS) -I$(SH_SOURCE) -I$(SH_SOURCE)/newgraph -I$(SH_SOURCE)/foreign -DBASECNST=\"basecnst.h\"
CC          = $(SH_COMPILER)
LD          = $(SH_COMPILER)
RANLIB		= $(SH_RANLIB)
LDFLAGS     = -L/usr/local/X11/lib
LIBS        = -lsource -lm -lX
LIB_GRAPH	=	$(SH_LIB)libgraph.a

LOCAL_SRCS	=	graphch.c hpch.c memch.c memfile.c psch.c xwch.c
#LOCAL_OBJS	=	$(LOCAL_SRCS:%.c=%.o)
LOCAL_OBJS	=	$(LOCAL_SRCS:.c=.o)

all:
	make $(LIB_GRAPH)

$(LIB_GRAPH):	$(LOCAL_OBJS)
	ar r $@ $(LOCAL_OBJS); $(RANLIB) $@


clean:
	@rm $(LOCAL_OBJS) *%


depend:
	makedepend -s "# DO NOT DELETE" -- $(CFLAGS) -- $(LOCAL_SRCS)


# DO NOT DELETE

graphch.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
graphch.o: /usr/include/sys/_symbol_aliasing.h
graphch.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
graphch.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
graphch.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
graphch.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
graphch.o: /usr/include/secure/_common.h /usr/local/sh/sh/source/basecnst.h
graphch.o: /usr/local/sh/sh/source/sysbase.h
graphch.o: /usr/local/sh/sh/source/fousrdef.h graphbas.h gmusrdef.h
graphch.o: mmusrdef.h psusrdef.h ccusrdef.h tkusrdef.h hpusrdef.h djusrdef.h
graphch.o: bgusrdef.h xwusrdef.h vwusrdef.h gcusrdef.h
hpch.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
hpch.o: /usr/include/sys/_symbol_aliasing.h
hpch.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
hpch.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
hpch.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
hpch.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
hpch.o: /usr/include/secure/_common.h /usr/include/string.h
hpch.o: /usr/include/strings.h /usr/include/secure/_string.h
hpch.o: /usr/local/sh/sh/source/basecnst.h /usr/local/sh/sh/source/sysbase.h
hpch.o: /usr/local/sh/sh/source/fousrdef.h /usr/local/sh/sh/source/ascii.h
hpch.o: graphbas.h hpusrdef.h hperrors.h
memch.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
memch.o: /usr/include/sys/_symbol_aliasing.h
memch.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
memch.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
memch.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
memch.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
memch.o: /usr/include/secure/_common.h /usr/include/string.h
memch.o: /usr/include/strings.h /usr/include/secure/_string.h
memch.o: /usr/local/sh/sh/source/basecnst.h /usr/local/sh/sh/source/sysbase.h
memch.o: /usr/local/sh/sh/source/fousrdef.h mfusrdef.h graphbas.h mmusrdef.h
memch.o: gcerrors.h
memfile.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
memfile.o: /usr/include/sys/_symbol_aliasing.h
memfile.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
memfile.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
memfile.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
memfile.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
memfile.o: /usr/include/secure/_common.h /usr/include/string.h
memfile.o: /usr/include/strings.h /usr/include/secure/_string.h
memfile.o: /usr/local/sh/sh/source/basecnst.h
memfile.o: /usr/local/sh/sh/source/sysbase.h
memfile.o: /usr/local/sh/sh/source/fousrdef.h
memfile.o: /usr/local/sh/sh/source/shconst.h
memfile.o: /usr/local/sh/sh/source/erusrdef.h mfusrdef.h
psch.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
psch.o: /usr/include/sys/_symbol_aliasing.h
psch.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
psch.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
psch.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
psch.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
psch.o: /usr/include/secure/_common.h /usr/include/string.h
psch.o: /usr/include/strings.h /usr/include/secure/_string.h
psch.o: /usr/local/sh/sh/source/basecnst.h /usr/local/sh/sh/source/sysbase.h
psch.o: /usr/local/sh/sh/source/fousrdef.h graphbas.h psusrdef.h pserrors.h
xwch.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
xwch.o: /usr/include/sys/_symbol_aliasing.h
xwch.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
xwch.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
xwch.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
xwch.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
xwch.o: /usr/include/secure/_common.h /usr/include/string.h
xwch.o: /usr/include/strings.h /usr/include/secure/_string.h
xwch.o: /usr/include/ctype.h /usr/include/runetype.h
xwch.o: /usr/local/sh/sh/source/basecnst.h /usr/X11R6/include/X11/Xlib.h
xwch.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
xwch.o: /usr/include/machine/types.h /usr/include/i386/types.h
xwch.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
xwch.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
xwch.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/sys/_structs.h
xwch.o: /usr/X11R6/include/X11/X.h /usr/X11R6/include/X11/Xfuncproto.h
xwch.o: /usr/X11R6/include/X11/Xosdefs.h /usr/include/stddef.h
xwch.o: /usr/X11R6/include/X11/Xatom.h /usr/X11R6/include/X11/cursorfont.h
xwch.o: /usr/X11R6/include/X11/Xutil.h /usr/X11R6/include/X11/keysym.h
xwch.o: /usr/X11R6/include/X11/keysymdef.h /usr/local/sh/sh/source/sysbase.h
xwch.o: /usr/local/sh/sh/source/fousrdef.h graphbas.h xwusrdef.h xwerrors.h
xwch.o: ../globalparams.h
