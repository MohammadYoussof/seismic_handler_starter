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

graphch.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
graphch.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
graphch.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
graphch.o: /usr/include/stdio_impl.h /programs/sol2/sh/source/basecnst.h
graphch.o: /programs/sol2/sh/source/sysbase.h
graphch.o: /programs/sol2/sh/source/fousrdef.h graphbas.h gmusrdef.h
graphch.o: mmusrdef.h psusrdef.h ccusrdef.h tkusrdef.h hpusrdef.h djusrdef.h
graphch.o: bgusrdef.h xwusrdef.h vwusrdef.h gcusrdef.h
hpch.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
hpch.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
hpch.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
hpch.o: /usr/include/stdio_impl.h /usr/include/string.h
hpch.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
hpch.o: /programs/sol2/sh/source/sysbase.h
hpch.o: /programs/sol2/sh/source/fousrdef.h /programs/sol2/sh/source/ascii.h
hpch.o: graphbas.h hpusrdef.h hperrors.h
memch.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
memch.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
memch.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
memch.o: /usr/include/stdio_impl.h /usr/include/string.h
memch.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
memch.o: /programs/sol2/sh/source/sysbase.h
memch.o: /programs/sol2/sh/source/fousrdef.h mfusrdef.h graphbas.h mmusrdef.h
memch.o: gcerrors.h
memfile.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
memfile.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
memfile.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
memfile.o: /usr/include/stdio_impl.h /usr/include/string.h
memfile.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
memfile.o: /programs/sol2/sh/source/sysbase.h
memfile.o: /programs/sol2/sh/source/fousrdef.h
memfile.o: /programs/sol2/sh/source/shconst.h
memfile.o: /programs/sol2/sh/source/erusrdef.h mfusrdef.h
psch.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
psch.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
psch.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
psch.o: /usr/include/stdio_impl.h /usr/include/string.h
psch.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
psch.o: /programs/sol2/sh/source/sysbase.h
psch.o: /programs/sol2/sh/source/fousrdef.h graphbas.h psusrdef.h pserrors.h
xwch.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
xwch.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
xwch.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
xwch.o: /usr/include/stdio_impl.h /usr/include/string.h
xwch.o: /usr/include/iso/string_iso.h /usr/include/ctype.h
xwch.o: /usr/include/iso/ctype_iso.h /programs/sol2/sh/source/basecnst.h
xwch.o: /usr/openwin/share/include/X11/Xlib.h /usr/include/sys/types.h
xwch.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
xwch.o: /usr/include/sys/select.h /usr/include/sys/time.h
xwch.o: /usr/include/sys/time.h /usr/openwin/share/include/X11/X.h
xwch.o: /usr/openwin/share/include/X11/Xfuncproto.h
xwch.o: /usr/openwin/share/include/X11/Xosdefs.h /usr/include/stddef.h
xwch.o: /usr/include/iso/stddef_iso.h /usr/openwin/share/include/X11/Xatom.h
xwch.o: /usr/openwin/share/include/X11/cursorfont.h
xwch.o: /usr/openwin/share/include/X11/Xutil.h
xwch.o: /programs/sol2/sh/source/sysbase.h
xwch.o: /programs/sol2/sh/source/fousrdef.h graphbas.h xwusrdef.h xwerrors.h
xwch.o: ../globalparams.h
