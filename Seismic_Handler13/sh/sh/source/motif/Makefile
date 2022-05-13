# file Makefile
#      ========
#
# version 24, 17-Nov-2005
#
# Makefile of directory $SH_ROOT/source/motif
# K. Stammler, 22-Jul-94

CFLAGS      = $(SH_CCOPTIONS) -I$(SH_SOURCE) -I$(SH_UTIL) -I$(SH_SOURCE)/newgraph -I$(SH_SOURCE)/foreign -DBASECNST=\"basecnst.h\"
CC          = $(SH_COMPILER)
LD          = $(SH_COMPILER)
RANLIB		= $(SH_RANLIB)
LDFLAGS     = -L$(SH_LIB) $(SH_LINKOPTIONS)
LIBS        = -lmotif -lsource -lgraph -lgcf -lseed -lfrgn -lutil -lsource \
		-lm -lMrm -lXm -lXt -lXext -lX11 $(SH_LINKLIBS) $(SH_MOTIFLIBS)
#                                         ^
#   this second '-lsource' is necessary /

PROGNAME		= shm

LIB_SOURCE	= $(SH_LIB)libsource.a
LIB_SEED		= $(SH_LIB)libseed.a
LIB_GCF		= $(SH_LIB)libgcf.a
LIB_GRAPH	= $(SH_LIB)libgraph.a
LIB_UTIL		= $(SH_LIB)libutil.a
LIB_FRGN		= $(SH_LIB)libfrgn.a
LIB_MOTIF	= $(SH_LIB)libmotif.a
ALL_SH_LIBS	= $(LIB_MOTIF) $(LIB_SOURCE) $(LIB_GRAPH) \
				$(LIB_SEED) $(LIB_GCF) $(LIB_UTIL) $(LIB_FRGN)


LOCAL_SRCS	= calibloc.c cblib.c cblib2.c cblib3.c cblib4.c cbutil.c \
	event_id.c get_geo.c mfexec.c motifgraph.c phaseinf.c phasemgr.c \
	pixmaps.c seismics.c spectrum.c partmotion.c polarfil.c trcselect.c

#LOCAL_OBJS	=	$(LOCAL_SRCS:%.c=%.o)
LOCAL_OBJS	=	$(LOCAL_SRCS:.c=.o)

ALL			= $(PROGNAME) $(PROGNAME).uid $(PROGNAME)_smfnt.uid

all:	$(ALL)

$(LIB_MOTIF):	$(LOCAL_OBJS)
	ar r $@ $(LOCAL_OBJS); $(RANLIB) $@

$(LIB_SOURCE):
	@echo "Source-Lib not up to date"

$(LIB_SEED):
	@echo "SEED-Lib not up to date"

$(LIB_UTIL):
	@echo "Util-Lib not up to date"

$(LIB_FRGN):
	@echo "Frgn-Lib not up to date"

$(PROGNAME):	shm.o $(MAKEFILE) $(ALL_SH_LIBS)
	@echo "Linking $(PROGNAME) ..."
	$(LD) shm.o $(LDFLAGS) $(LIBS) -o $@

$(PROGNAME).uid:	*.uil
	(LANG=en; uil -o $@ $(PROGNAME).uil)

$(PROGNAME)_smfnt.uid:	*.uil
	@(grep -v bigfont shm.uil | sed 's/\!smallfont//' >shm_smfnt.uil)
	(LANG=en; uil -o $@ $(PROGNAME)_smfnt.uil)



clean:
	-@rm *% $(ALL) $(LOCAL_OBJS) $(PROGNAME).o $(PROGNAME)_world $(PROGNAME)_world.uid $(PROGNAME)_world.old $(PROGNAME)_smallfont.uid
	-@rm $(LIB_MOTIF)

depend:
	makedepend -s "# DO NOT DELETE" -- $(CFLAGS) -- $(LOCAL_SRCS) shm.c


# DO NOT DELETE

calibloc.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
calibloc.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
calibloc.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
calibloc.o: /usr/include/stdio_impl.h /usr/include/string.h
calibloc.o: /usr/include/iso/string_iso.h /usr/include/math.h
calibloc.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
calibloc.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
calibloc.o: /usr/dt/include/Xm/Xm.h
calibloc.o: /usr/openwin/share/include/X11/Intrinsic.h
calibloc.o: /usr/openwin/share/include/X11/Xlib.h /usr/include/sys/types.h
calibloc.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
calibloc.o: /usr/include/sys/select.h /usr/include/sys/time.h
calibloc.o: /usr/include/sys/time.h /usr/openwin/share/include/X11/X.h
calibloc.o: /usr/openwin/share/include/X11/Xfuncproto.h
calibloc.o: /usr/openwin/share/include/X11/Xosdefs.h /usr/include/stddef.h
calibloc.o: /usr/include/iso/stddef_iso.h
calibloc.o: /usr/openwin/share/include/X11/Xutil.h
calibloc.o: /usr/openwin/share/include/X11/Xresource.h
calibloc.o: /usr/openwin/share/include/X11/Core.h
calibloc.o: /usr/openwin/share/include/X11/Composite.h
calibloc.o: /usr/openwin/share/include/X11/Constraint.h
calibloc.o: /usr/openwin/share/include/X11/Object.h
calibloc.o: /usr/openwin/share/include/X11/RectObj.h
calibloc.o: /usr/openwin/share/include/X11/Shell.h
calibloc.o: /usr/openwin/share/include/X11/SM/SMlib.h
calibloc.o: /usr/openwin/share/include/X11/SM/SM.h
calibloc.o: /usr/openwin/share/include/X11/ICE/ICElib.h
calibloc.o: /usr/openwin/share/include/X11/ICE/ICE.h
calibloc.o: /usr/openwin/share/include/X11/Xatom.h
calibloc.o: /usr/dt/include/Xm/XmStrDefs.h
calibloc.o: /usr/openwin/share/include/X11/StringDefs.h
calibloc.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/Transfer.h
calibloc.o: /usr/dt/include/Xm/DragDrop.h /usr/dt/include/Xm/DragC.h
calibloc.o: /usr/dt/include/Xm/Display.h /usr/dt/include/Xm/DropSMgr.h
calibloc.o: /usr/dt/include/Xm/DragIcon.h /usr/dt/include/Xm/DropTrans.h
calibloc.o: /usr/dt/include/Xm/DragOverS.h /usr/dt/include/Xm/Primitive.h
calibloc.o: /usr/dt/include/Xm/Manager.h /usr/dt/include/Xm/Gadget.h
calibloc.o: /usr/dt/include/Xm/TxtPropCv.h /usr/dt/include/Xm/VendorS.h
calibloc.o: /usr/dt/include/Xm/XmIm.h /programs/sol2/sh/source/sysbase.h
calibloc.o: /programs/sol2/sh/source/fousrdef.h
calibloc.o: /programs/sol2/sh/source/shvars.h
calibloc.o: /programs/sol2/sh/source/shconst.h
calibloc.o: /programs/sol2/sh/source/erusrdef.h
calibloc.o: /programs/sol2/sh/source/infoidx.h
calibloc.o: /programs/sol2/sh/source/tcusrdef.h
calibloc.o: /programs/sol2/sh/source/newgraph/gcusrdef.h
calibloc.o: /programs/sol2/sh/source/newgraph/graphbas.h
calibloc.o: /programs/sol2/sh/source/ptusrdef.h
calibloc.o: /programs/sol2/sh/source/earthloc.h pixmaps.h motifgraph.h
calibloc.o: phaseinf.h phasemgr.h cbutil.h
calibloc.o: /programs/sol2/sh/source/glusrdef.h calibloc.h
calibloc.o: /programs/sol2/sh/source/globalparams.h
cblib.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
cblib.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
cblib.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
cblib.o: /usr/include/stdio_impl.h /usr/include/string.h
cblib.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
cblib.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
cblib.o: /usr/dt/include/Xm/Xm.h /usr/openwin/share/include/X11/Intrinsic.h
cblib.o: /usr/openwin/share/include/X11/Xlib.h /usr/include/sys/types.h
cblib.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
cblib.o: /usr/include/sys/select.h /usr/include/sys/time.h
cblib.o: /usr/include/sys/time.h /usr/openwin/share/include/X11/X.h
cblib.o: /usr/openwin/share/include/X11/Xfuncproto.h
cblib.o: /usr/openwin/share/include/X11/Xosdefs.h /usr/include/stddef.h
cblib.o: /usr/include/iso/stddef_iso.h /usr/openwin/share/include/X11/Xutil.h
cblib.o: /usr/openwin/share/include/X11/Xresource.h
cblib.o: /usr/openwin/share/include/X11/Core.h
cblib.o: /usr/openwin/share/include/X11/Composite.h
cblib.o: /usr/openwin/share/include/X11/Constraint.h
cblib.o: /usr/openwin/share/include/X11/Object.h
cblib.o: /usr/openwin/share/include/X11/RectObj.h
cblib.o: /usr/openwin/share/include/X11/Shell.h
cblib.o: /usr/openwin/share/include/X11/SM/SMlib.h
cblib.o: /usr/openwin/share/include/X11/SM/SM.h
cblib.o: /usr/openwin/share/include/X11/ICE/ICElib.h
cblib.o: /usr/openwin/share/include/X11/ICE/ICE.h
cblib.o: /usr/openwin/share/include/X11/Xatom.h
cblib.o: /usr/dt/include/Xm/XmStrDefs.h
cblib.o: /usr/openwin/share/include/X11/StringDefs.h
cblib.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/Transfer.h
cblib.o: /usr/dt/include/Xm/DragDrop.h /usr/dt/include/Xm/DragC.h
cblib.o: /usr/dt/include/Xm/Display.h /usr/dt/include/Xm/DropSMgr.h
cblib.o: /usr/dt/include/Xm/DragIcon.h /usr/dt/include/Xm/DropTrans.h
cblib.o: /usr/dt/include/Xm/DragOverS.h /usr/dt/include/Xm/Primitive.h
cblib.o: /usr/dt/include/Xm/Manager.h /usr/dt/include/Xm/Gadget.h
cblib.o: /usr/dt/include/Xm/TxtPropCv.h /usr/dt/include/Xm/VendorS.h
cblib.o: /usr/dt/include/Xm/XmIm.h /usr/dt/include/Xm/Text.h
cblib.o: /programs/sol2/sh/source/sysbase.h
cblib.o: /programs/sol2/sh/source/fousrdef.h
cblib.o: /programs/sol2/sh/source/infoidx.h
cblib.o: /programs/sol2/sh/source/shconst.h
cblib.o: /programs/sol2/sh/source/tcusrdef.h
cblib.o: /programs/sol2/sh/source/newgraph/gcusrdef.h
cblib.o: /programs/sol2/sh/source/newgraph/graphbas.h
cblib.o: /programs/sol2/sh/source/ptusrdef.h
cblib.o: /programs/sol2/sh/source/fctxdm.h /programs/sol2/sh/source/fctxsl.h
cblib.o: /programs/sol2/sh/source/cpusrdef.h
cblib.o: /programs/sol2/sh/source/rfusrdef.h
cblib.o: /programs/sol2/sh/source/shvars.h
cblib.o: /programs/sol2/sh/source/utusrdef.h motifgraph.h phaseinf.h
cblib.o: phasemgr.h cbutil.h /programs/sol2/sh/source/glusrdef.h cblib.h
cblib.o: shm_widgets.h sysext.h /programs/sol2/sh/source/globalparams.h
cblib2.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
cblib2.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
cblib2.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
cblib2.o: /usr/include/stdio_impl.h /usr/include/string.h
cblib2.o: /usr/include/iso/string_iso.h /usr/include/math.h
cblib2.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
cblib2.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
cblib2.o: /usr/dt/include/Xm/Xm.h /usr/openwin/share/include/X11/Intrinsic.h
cblib2.o: /usr/openwin/share/include/X11/Xlib.h /usr/include/sys/types.h
cblib2.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
cblib2.o: /usr/include/sys/select.h /usr/include/sys/time.h
cblib2.o: /usr/include/sys/time.h /usr/openwin/share/include/X11/X.h
cblib2.o: /usr/openwin/share/include/X11/Xfuncproto.h
cblib2.o: /usr/openwin/share/include/X11/Xosdefs.h /usr/include/stddef.h
cblib2.o: /usr/include/iso/stddef_iso.h
cblib2.o: /usr/openwin/share/include/X11/Xutil.h
cblib2.o: /usr/openwin/share/include/X11/Xresource.h
cblib2.o: /usr/openwin/share/include/X11/Core.h
cblib2.o: /usr/openwin/share/include/X11/Composite.h
cblib2.o: /usr/openwin/share/include/X11/Constraint.h
cblib2.o: /usr/openwin/share/include/X11/Object.h
cblib2.o: /usr/openwin/share/include/X11/RectObj.h
cblib2.o: /usr/openwin/share/include/X11/Shell.h
cblib2.o: /usr/openwin/share/include/X11/SM/SMlib.h
cblib2.o: /usr/openwin/share/include/X11/SM/SM.h
cblib2.o: /usr/openwin/share/include/X11/ICE/ICElib.h
cblib2.o: /usr/openwin/share/include/X11/ICE/ICE.h
cblib2.o: /usr/openwin/share/include/X11/Xatom.h
cblib2.o: /usr/dt/include/Xm/XmStrDefs.h
cblib2.o: /usr/openwin/share/include/X11/StringDefs.h
cblib2.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/Transfer.h
cblib2.o: /usr/dt/include/Xm/DragDrop.h /usr/dt/include/Xm/DragC.h
cblib2.o: /usr/dt/include/Xm/Display.h /usr/dt/include/Xm/DropSMgr.h
cblib2.o: /usr/dt/include/Xm/DragIcon.h /usr/dt/include/Xm/DropTrans.h
cblib2.o: /usr/dt/include/Xm/DragOverS.h /usr/dt/include/Xm/Primitive.h
cblib2.o: /usr/dt/include/Xm/Manager.h /usr/dt/include/Xm/Gadget.h
cblib2.o: /usr/dt/include/Xm/TxtPropCv.h /usr/dt/include/Xm/VendorS.h
cblib2.o: /usr/dt/include/Xm/XmIm.h /usr/dt/include/Xm/Text.h
cblib2.o: /usr/dt/include/Xm/Scale.h /usr/include/unistd.h
cblib2.o: /usr/include/sys/unistd.h /programs/sol2/sh/source/sysbase.h
cblib2.o: /programs/sol2/sh/source/fousrdef.h
cblib2.o: /programs/sol2/sh/source/shvars.h
cblib2.o: /programs/sol2/sh/source/shconst.h
cblib2.o: /programs/sol2/sh/source/infoidx.h
cblib2.o: /programs/sol2/sh/source/tcusrdef.h
cblib2.o: /programs/sol2/sh/source/newgraph/gcusrdef.h
cblib2.o: /programs/sol2/sh/source/newgraph/graphbas.h
cblib2.o: /programs/sol2/sh/source/glusrdef.h
cblib2.o: /programs/sol2/sh/source/earthloc.h
cblib2.o: /programs/sol2/sh/source/erusrdef.h
cblib2.o: /programs/sol2/sh/source/ptusrdef.h motifgraph.h phaseinf.h
cblib2.o: phasemgr.h cbutil.h cblib2.h seismics.h shm_widgets.h sysext.h
cblib2.o: /programs/sol2/sh/source/globalparams.h
cblib3.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
cblib3.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
cblib3.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
cblib3.o: /usr/include/stdio_impl.h /usr/include/string.h
cblib3.o: /usr/include/iso/string_iso.h /usr/include/math.h
cblib3.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
cblib3.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
cblib3.o: /usr/dt/include/Xm/Xm.h /usr/openwin/share/include/X11/Intrinsic.h
cblib3.o: /usr/openwin/share/include/X11/Xlib.h /usr/include/sys/types.h
cblib3.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
cblib3.o: /usr/include/sys/select.h /usr/include/sys/time.h
cblib3.o: /usr/include/sys/time.h /usr/openwin/share/include/X11/X.h
cblib3.o: /usr/openwin/share/include/X11/Xfuncproto.h
cblib3.o: /usr/openwin/share/include/X11/Xosdefs.h /usr/include/stddef.h
cblib3.o: /usr/include/iso/stddef_iso.h
cblib3.o: /usr/openwin/share/include/X11/Xutil.h
cblib3.o: /usr/openwin/share/include/X11/Xresource.h
cblib3.o: /usr/openwin/share/include/X11/Core.h
cblib3.o: /usr/openwin/share/include/X11/Composite.h
cblib3.o: /usr/openwin/share/include/X11/Constraint.h
cblib3.o: /usr/openwin/share/include/X11/Object.h
cblib3.o: /usr/openwin/share/include/X11/RectObj.h
cblib3.o: /usr/openwin/share/include/X11/Shell.h
cblib3.o: /usr/openwin/share/include/X11/SM/SMlib.h
cblib3.o: /usr/openwin/share/include/X11/SM/SM.h
cblib3.o: /usr/openwin/share/include/X11/ICE/ICElib.h
cblib3.o: /usr/openwin/share/include/X11/ICE/ICE.h
cblib3.o: /usr/openwin/share/include/X11/Xatom.h
cblib3.o: /usr/dt/include/Xm/XmStrDefs.h
cblib3.o: /usr/openwin/share/include/X11/StringDefs.h
cblib3.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/Transfer.h
cblib3.o: /usr/dt/include/Xm/DragDrop.h /usr/dt/include/Xm/DragC.h
cblib3.o: /usr/dt/include/Xm/Display.h /usr/dt/include/Xm/DropSMgr.h
cblib3.o: /usr/dt/include/Xm/DragIcon.h /usr/dt/include/Xm/DropTrans.h
cblib3.o: /usr/dt/include/Xm/DragOverS.h /usr/dt/include/Xm/Primitive.h
cblib3.o: /usr/dt/include/Xm/Manager.h /usr/dt/include/Xm/Gadget.h
cblib3.o: /usr/dt/include/Xm/TxtPropCv.h /usr/dt/include/Xm/VendorS.h
cblib3.o: /usr/dt/include/Xm/XmIm.h /usr/dt/include/Xm/ToggleB.h
cblib3.o: /usr/dt/include/Xm/Scale.h /programs/sol2/sh/source/sysbase.h
cblib3.o: /programs/sol2/sh/source/fousrdef.h sysext.h
cblib3.o: /programs/sol2/sh/source/shvars.h
cblib3.o: /programs/sol2/sh/source/shconst.h
cblib3.o: /programs/sol2/sh/source/infoidx.h
cblib3.o: /programs/sol2/sh/source/tcusrdef.h
cblib3.o: /programs/sol2/sh/source/newgraph/gcusrdef.h
cblib3.o: /programs/sol2/sh/source/newgraph/graphbas.h
cblib3.o: /programs/sol2/sh/source/erusrdef.h
cblib3.o: /programs/sol2/sh/source/station_no.h
cblib3.o: /programs/sol2/sh/source/glusrdef.h
cblib3.o: /programs/sol2/sh/source/utusrdef.h
cblib3.o: /programs/sol2/sh/source/earthloc.h
cblib3.o: /programs/sol2/sh/source/callsh.h /programs/sol2/sh/util/eventdsc.h
cblib3.o: shm_widgets.h event_id.h phaseinf.h motifgraph.h phasemgr.h
cblib3.o: seismics.h cbutil.h mfexec.h cblib.h cblib3.h
cblib3.o: /programs/sol2/sh/source/globalparams.h
cblib4.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
cblib4.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
cblib4.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
cblib4.o: /usr/include/stdio_impl.h /usr/include/string.h
cblib4.o: /usr/include/iso/string_iso.h /usr/include/math.h
cblib4.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
cblib4.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
cblib4.o: /usr/dt/include/Xm/Xm.h /usr/openwin/share/include/X11/Intrinsic.h
cblib4.o: /usr/openwin/share/include/X11/Xlib.h /usr/include/sys/types.h
cblib4.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
cblib4.o: /usr/include/sys/select.h /usr/include/sys/time.h
cblib4.o: /usr/include/sys/time.h /usr/openwin/share/include/X11/X.h
cblib4.o: /usr/openwin/share/include/X11/Xfuncproto.h
cblib4.o: /usr/openwin/share/include/X11/Xosdefs.h /usr/include/stddef.h
cblib4.o: /usr/include/iso/stddef_iso.h
cblib4.o: /usr/openwin/share/include/X11/Xutil.h
cblib4.o: /usr/openwin/share/include/X11/Xresource.h
cblib4.o: /usr/openwin/share/include/X11/Core.h
cblib4.o: /usr/openwin/share/include/X11/Composite.h
cblib4.o: /usr/openwin/share/include/X11/Constraint.h
cblib4.o: /usr/openwin/share/include/X11/Object.h
cblib4.o: /usr/openwin/share/include/X11/RectObj.h
cblib4.o: /usr/openwin/share/include/X11/Shell.h
cblib4.o: /usr/openwin/share/include/X11/SM/SMlib.h
cblib4.o: /usr/openwin/share/include/X11/SM/SM.h
cblib4.o: /usr/openwin/share/include/X11/ICE/ICElib.h
cblib4.o: /usr/openwin/share/include/X11/ICE/ICE.h
cblib4.o: /usr/openwin/share/include/X11/Xatom.h
cblib4.o: /usr/dt/include/Xm/XmStrDefs.h
cblib4.o: /usr/openwin/share/include/X11/StringDefs.h
cblib4.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/Transfer.h
cblib4.o: /usr/dt/include/Xm/DragDrop.h /usr/dt/include/Xm/DragC.h
cblib4.o: /usr/dt/include/Xm/Display.h /usr/dt/include/Xm/DropSMgr.h
cblib4.o: /usr/dt/include/Xm/DragIcon.h /usr/dt/include/Xm/DropTrans.h
cblib4.o: /usr/dt/include/Xm/DragOverS.h /usr/dt/include/Xm/Primitive.h
cblib4.o: /usr/dt/include/Xm/Manager.h /usr/dt/include/Xm/Gadget.h
cblib4.o: /usr/dt/include/Xm/TxtPropCv.h /usr/dt/include/Xm/VendorS.h
cblib4.o: /usr/dt/include/Xm/XmIm.h /usr/dt/include/Xm/List.h
cblib4.o: /usr/dt/include/Xm/ToggleB.h /usr/include/unistd.h
cblib4.o: /usr/include/sys/unistd.h /programs/sol2/sh/source/sysbase.h
cblib4.o: /programs/sol2/sh/source/fousrdef.h
cblib4.o: /programs/sol2/sh/source/shvars.h
cblib4.o: /programs/sol2/sh/source/shconst.h
cblib4.o: /programs/sol2/sh/source/infoidx.h
cblib4.o: /programs/sol2/sh/source/tcusrdef.h
cblib4.o: /programs/sol2/sh/source/newgraph/gcusrdef.h
cblib4.o: /programs/sol2/sh/source/newgraph/graphbas.h
cblib4.o: /programs/sol2/sh/source/utusrdef.h
cblib4.o: /programs/sol2/sh/source/erusrdef.h
cblib4.o: /programs/sol2/sh/source/ptusrdef.h
cblib4.o: /programs/sol2/sh/source/earthloc.h
cblib4.o: /programs/sol2/sh/source/fctxdm.h
cblib4.o: /programs/sol2/sh/source/station_no.h ../seed_io/seedcfg.h
cblib4.o: ../seed_io/seed_lib.h motifgraph.h phaseinf.h phasemgr.h cbutil.h
cblib4.o: /programs/sol2/sh/source/glusrdef.h mfexec.h cblib4.h shm_widgets.h
cblib4.o: trcselect.h /programs/sol2/sh/source/residual.h
cblib4.o: /programs/sol2/sh/source/globalparams.h
cbutil.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
cbutil.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
cbutil.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
cbutil.o: /usr/include/stdio_impl.h /usr/include/string.h
cbutil.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
cbutil.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
cbutil.o: /usr/dt/include/Xm/Xm.h /usr/openwin/share/include/X11/Intrinsic.h
cbutil.o: /usr/openwin/share/include/X11/Xlib.h /usr/include/sys/types.h
cbutil.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
cbutil.o: /usr/include/sys/select.h /usr/include/sys/time.h
cbutil.o: /usr/include/sys/time.h /usr/openwin/share/include/X11/X.h
cbutil.o: /usr/openwin/share/include/X11/Xfuncproto.h
cbutil.o: /usr/openwin/share/include/X11/Xosdefs.h /usr/include/stddef.h
cbutil.o: /usr/include/iso/stddef_iso.h
cbutil.o: /usr/openwin/share/include/X11/Xutil.h
cbutil.o: /usr/openwin/share/include/X11/Xresource.h
cbutil.o: /usr/openwin/share/include/X11/Core.h
cbutil.o: /usr/openwin/share/include/X11/Composite.h
cbutil.o: /usr/openwin/share/include/X11/Constraint.h
cbutil.o: /usr/openwin/share/include/X11/Object.h
cbutil.o: /usr/openwin/share/include/X11/RectObj.h
cbutil.o: /usr/openwin/share/include/X11/Shell.h
cbutil.o: /usr/openwin/share/include/X11/SM/SMlib.h
cbutil.o: /usr/openwin/share/include/X11/SM/SM.h
cbutil.o: /usr/openwin/share/include/X11/ICE/ICElib.h
cbutil.o: /usr/openwin/share/include/X11/ICE/ICE.h
cbutil.o: /usr/openwin/share/include/X11/Xatom.h
cbutil.o: /usr/dt/include/Xm/XmStrDefs.h
cbutil.o: /usr/openwin/share/include/X11/StringDefs.h
cbutil.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/Transfer.h
cbutil.o: /usr/dt/include/Xm/DragDrop.h /usr/dt/include/Xm/DragC.h
cbutil.o: /usr/dt/include/Xm/Display.h /usr/dt/include/Xm/DropSMgr.h
cbutil.o: /usr/dt/include/Xm/DragIcon.h /usr/dt/include/Xm/DropTrans.h
cbutil.o: /usr/dt/include/Xm/DragOverS.h /usr/dt/include/Xm/Primitive.h
cbutil.o: /usr/dt/include/Xm/Manager.h /usr/dt/include/Xm/Gadget.h
cbutil.o: /usr/dt/include/Xm/TxtPropCv.h /usr/dt/include/Xm/VendorS.h
cbutil.o: /usr/dt/include/Xm/XmIm.h /usr/dt/include/Xm/ToggleB.h
cbutil.o: /usr/dt/include/Xm/Text.h /usr/include/math.h
cbutil.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/sysbase.h
cbutil.o: /programs/sol2/sh/source/fousrdef.h
cbutil.o: /programs/sol2/sh/source/tcusrdef.h
cbutil.o: /programs/sol2/sh/source/infoidx.h
cbutil.o: /programs/sol2/sh/source/shconst.h
cbutil.o: /programs/sol2/sh/source/newgraph/gcusrdef.h
cbutil.o: /programs/sol2/sh/source/newgraph/graphbas.h
cbutil.o: /programs/sol2/sh/source/glusrdef.h
cbutil.o: /programs/sol2/sh/source/fctxmt.h
cbutil.o: /programs/sol2/sh/source/sherrors.h
cbutil.o: /programs/sol2/sh/source/earthloc.h
cbutil.o: /programs/sol2/sh/source/ptusrdef.h
cbutil.o: /programs/sol2/sh/source/ffusrdef.h
cbutil.o: /programs/sol2/sh/source/erusrdef.h
cbutil.o: /programs/sol2/sh/source/utusrdef.h motifgraph.h phaseinf.h
cbutil.o: phasemgr.h seismics.h cbutil.h /programs/sol2/sh/source/shvars.h
cbutil.o: shm_widgets.h /programs/sol2/sh/source/globalparams.h
event_id.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
event_id.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
event_id.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
event_id.o: /usr/include/stdio_impl.h /usr/include/string.h
event_id.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
event_id.o: /programs/sol2/sh/source/sysbase.h
event_id.o: /programs/sol2/sh/source/fousrdef.h
event_id.o: /programs/sol2/sh/source/tcusrdef.h event_id.h
get_geo.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
get_geo.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
get_geo.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
get_geo.o: /usr/include/stdio_impl.h /usr/include/string.h
get_geo.o: /usr/include/iso/string_iso.h /usr/include/stdlib.h
get_geo.o: /usr/include/iso/stdlib_iso.h /usr/include/math.h
get_geo.o: /usr/include/iso/math_iso.h
mfexec.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
mfexec.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
mfexec.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
mfexec.o: /usr/include/stdio_impl.h /usr/include/string.h
mfexec.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
mfexec.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
mfexec.o: /usr/dt/include/Xm/Xm.h /usr/openwin/share/include/X11/Intrinsic.h
mfexec.o: /usr/openwin/share/include/X11/Xlib.h /usr/include/sys/types.h
mfexec.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
mfexec.o: /usr/include/sys/select.h /usr/include/sys/time.h
mfexec.o: /usr/include/sys/time.h /usr/openwin/share/include/X11/X.h
mfexec.o: /usr/openwin/share/include/X11/Xfuncproto.h
mfexec.o: /usr/openwin/share/include/X11/Xosdefs.h /usr/include/stddef.h
mfexec.o: /usr/include/iso/stddef_iso.h
mfexec.o: /usr/openwin/share/include/X11/Xutil.h
mfexec.o: /usr/openwin/share/include/X11/Xresource.h
mfexec.o: /usr/openwin/share/include/X11/Core.h
mfexec.o: /usr/openwin/share/include/X11/Composite.h
mfexec.o: /usr/openwin/share/include/X11/Constraint.h
mfexec.o: /usr/openwin/share/include/X11/Object.h
mfexec.o: /usr/openwin/share/include/X11/RectObj.h
mfexec.o: /usr/openwin/share/include/X11/Shell.h
mfexec.o: /usr/openwin/share/include/X11/SM/SMlib.h
mfexec.o: /usr/openwin/share/include/X11/SM/SM.h
mfexec.o: /usr/openwin/share/include/X11/ICE/ICElib.h
mfexec.o: /usr/openwin/share/include/X11/ICE/ICE.h
mfexec.o: /usr/openwin/share/include/X11/Xatom.h
mfexec.o: /usr/dt/include/Xm/XmStrDefs.h
mfexec.o: /usr/openwin/share/include/X11/StringDefs.h
mfexec.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/Transfer.h
mfexec.o: /usr/dt/include/Xm/DragDrop.h /usr/dt/include/Xm/DragC.h
mfexec.o: /usr/dt/include/Xm/Display.h /usr/dt/include/Xm/DropSMgr.h
mfexec.o: /usr/dt/include/Xm/DragIcon.h /usr/dt/include/Xm/DropTrans.h
mfexec.o: /usr/dt/include/Xm/DragOverS.h /usr/dt/include/Xm/Primitive.h
mfexec.o: /usr/dt/include/Xm/Manager.h /usr/dt/include/Xm/Gadget.h
mfexec.o: /usr/dt/include/Xm/TxtPropCv.h /usr/dt/include/Xm/VendorS.h
mfexec.o: /usr/dt/include/Xm/XmIm.h /usr/dt/include/Xm/ToggleB.h
mfexec.o: /usr/dt/include/Xm/Text.h /programs/sol2/sh/source/sysbase.h
mfexec.o: /programs/sol2/sh/source/fousrdef.h
mfexec.o: /programs/sol2/sh/source/erusrdef.h motifgraph.h phaseinf.h
mfexec.o: phasemgr.h /programs/sol2/sh/source/station_no.h cbutil.h
mfexec.o: /programs/sol2/sh/source/glusrdef.h mfexec.h cblib.h cblib3.h
mfexec.o: /programs/sol2/sh/source/callsh.h
mfexec.o: /programs/sol2/sh/source/infoidx.h
mfexec.o: /programs/sol2/sh/source/shconst.h
mfexec.o: /programs/sol2/sh/source/tcusrdef.h
mfexec.o: /programs/sol2/sh/source/newgraph/gcusrdef.h
mfexec.o: /programs/sol2/sh/source/newgraph/graphbas.h
mfexec.o: /programs/sol2/sh/source/utusrdef.h
mfexec.o: /programs/sol2/sh/source/shvars.h trcselect.h shm_widgets.h
mfexec.o: sysext.h seismics.h /programs/sol2/sh/source/globalparams.h
motifgraph.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
motifgraph.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
motifgraph.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
motifgraph.o: /usr/include/stdio_impl.h /usr/include/string.h
motifgraph.o: /usr/include/iso/string_iso.h /usr/include/math.h
motifgraph.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
motifgraph.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
motifgraph.o: /usr/dt/include/Xm/Xm.h
motifgraph.o: /usr/openwin/share/include/X11/Intrinsic.h
motifgraph.o: /usr/openwin/share/include/X11/Xlib.h /usr/include/sys/types.h
motifgraph.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
motifgraph.o: /usr/include/sys/select.h /usr/include/sys/time.h
motifgraph.o: /usr/include/sys/time.h /usr/openwin/share/include/X11/X.h
motifgraph.o: /usr/openwin/share/include/X11/Xfuncproto.h
motifgraph.o: /usr/openwin/share/include/X11/Xosdefs.h /usr/include/stddef.h
motifgraph.o: /usr/include/iso/stddef_iso.h
motifgraph.o: /usr/openwin/share/include/X11/Xutil.h
motifgraph.o: /usr/openwin/share/include/X11/Xresource.h
motifgraph.o: /usr/openwin/share/include/X11/Core.h
motifgraph.o: /usr/openwin/share/include/X11/Composite.h
motifgraph.o: /usr/openwin/share/include/X11/Constraint.h
motifgraph.o: /usr/openwin/share/include/X11/Object.h
motifgraph.o: /usr/openwin/share/include/X11/RectObj.h
motifgraph.o: /usr/openwin/share/include/X11/Shell.h
motifgraph.o: /usr/openwin/share/include/X11/SM/SMlib.h
motifgraph.o: /usr/openwin/share/include/X11/SM/SM.h
motifgraph.o: /usr/openwin/share/include/X11/ICE/ICElib.h
motifgraph.o: /usr/openwin/share/include/X11/ICE/ICE.h
motifgraph.o: /usr/openwin/share/include/X11/Xatom.h
motifgraph.o: /usr/dt/include/Xm/XmStrDefs.h
motifgraph.o: /usr/openwin/share/include/X11/StringDefs.h
motifgraph.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/Transfer.h
motifgraph.o: /usr/dt/include/Xm/DragDrop.h /usr/dt/include/Xm/DragC.h
motifgraph.o: /usr/dt/include/Xm/Display.h /usr/dt/include/Xm/DropSMgr.h
motifgraph.o: /usr/dt/include/Xm/DragIcon.h /usr/dt/include/Xm/DropTrans.h
motifgraph.o: /usr/dt/include/Xm/DragOverS.h /usr/dt/include/Xm/Primitive.h
motifgraph.o: /usr/dt/include/Xm/Manager.h /usr/dt/include/Xm/Gadget.h
motifgraph.o: /usr/dt/include/Xm/TxtPropCv.h /usr/dt/include/Xm/VendorS.h
motifgraph.o: /usr/dt/include/Xm/XmIm.h /usr/dt/include/Mrm/MrmPublic.h
motifgraph.o: /usr/openwin/share/include/X11/Xmd.h
motifgraph.o: /usr/dt/include/Mrm/MrmDecls.h
motifgraph.o: /usr/openwin/share/include/X11/cursorfont.h
motifgraph.o: /programs/sol2/sh/source/sysbase.h
motifgraph.o: /programs/sol2/sh/source/fousrdef.h
motifgraph.o: /programs/sol2/sh/source/infoidx.h
motifgraph.o: /programs/sol2/sh/source/shconst.h
motifgraph.o: /programs/sol2/sh/source/tcusrdef.h
motifgraph.o: /programs/sol2/sh/source/newgraph/gcusrdef.h
motifgraph.o: /programs/sol2/sh/source/newgraph/graphbas.h
motifgraph.o: /programs/sol2/sh/source/fctxdm.h
motifgraph.o: /programs/sol2/sh/source/trusrdef.h
motifgraph.o: /programs/sol2/sh/source/cpusrdef.h
motifgraph.o: /programs/sol2/sh/source/ffusrdef.h phaseinf.h phasemgr.h
motifgraph.o: pixmaps.h motifgraph.h trcselect.h
motifgraph.o: /programs/sol2/sh/source/globalparams.h
phaseinf.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
phaseinf.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
phaseinf.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
phaseinf.o: /usr/include/stdio_impl.h /usr/include/string.h
phaseinf.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
phaseinf.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
phaseinf.o: /programs/sol2/sh/source/sysbase.h
phaseinf.o: /programs/sol2/sh/source/fousrdef.h
phaseinf.o: /programs/sol2/sh/source/tcusrdef.h phaseinf.h
phaseinf.o: /programs/sol2/sh/source/globalparams.h
phasemgr.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
phasemgr.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
phasemgr.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
phasemgr.o: /usr/include/stdio_impl.h /usr/include/string.h
phasemgr.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
phasemgr.o: /programs/sol2/sh/source/sysbase.h
phasemgr.o: /programs/sol2/sh/source/fousrdef.h
phasemgr.o: /programs/sol2/sh/source/utusrdef.h
phasemgr.o: /programs/sol2/sh/source/infoidx.h
phasemgr.o: /programs/sol2/sh/source/shconst.h
phasemgr.o: /programs/sol2/sh/source/tcusrdef.h
phasemgr.o: /programs/sol2/sh/source/newgraph/gcusrdef.h
phasemgr.o: /programs/sol2/sh/source/newgraph/graphbas.h phaseinf.h
phasemgr.o: phasemgr.h
pixmaps.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
pixmaps.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
pixmaps.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
pixmaps.o: /usr/include/stdio_impl.h /programs/sol2/sh/source/basecnst.h
pixmaps.o: /programs/sol2/sh/source/globalparams.h /usr/include/stdlib.h
pixmaps.o: /usr/include/iso/stdlib_iso.h /usr/dt/include/Xm/Xm.h
pixmaps.o: /usr/openwin/share/include/X11/Intrinsic.h
pixmaps.o: /usr/openwin/share/include/X11/Xlib.h /usr/include/sys/types.h
pixmaps.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
pixmaps.o: /usr/include/sys/select.h /usr/include/sys/time.h
pixmaps.o: /usr/include/sys/time.h /usr/openwin/share/include/X11/X.h
pixmaps.o: /usr/openwin/share/include/X11/Xfuncproto.h
pixmaps.o: /usr/openwin/share/include/X11/Xosdefs.h /usr/include/stddef.h
pixmaps.o: /usr/include/iso/stddef_iso.h
pixmaps.o: /usr/openwin/share/include/X11/Xutil.h
pixmaps.o: /usr/openwin/share/include/X11/Xresource.h /usr/include/string.h
pixmaps.o: /usr/include/iso/string_iso.h
pixmaps.o: /usr/openwin/share/include/X11/Core.h
pixmaps.o: /usr/openwin/share/include/X11/Composite.h
pixmaps.o: /usr/openwin/share/include/X11/Constraint.h
pixmaps.o: /usr/openwin/share/include/X11/Object.h
pixmaps.o: /usr/openwin/share/include/X11/RectObj.h
pixmaps.o: /usr/openwin/share/include/X11/Shell.h
pixmaps.o: /usr/openwin/share/include/X11/SM/SMlib.h
pixmaps.o: /usr/openwin/share/include/X11/SM/SM.h
pixmaps.o: /usr/openwin/share/include/X11/ICE/ICElib.h
pixmaps.o: /usr/openwin/share/include/X11/ICE/ICE.h
pixmaps.o: /usr/openwin/share/include/X11/Xatom.h
pixmaps.o: /usr/dt/include/Xm/XmStrDefs.h
pixmaps.o: /usr/openwin/share/include/X11/StringDefs.h
pixmaps.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/Transfer.h
pixmaps.o: /usr/dt/include/Xm/DragDrop.h /usr/dt/include/Xm/DragC.h
pixmaps.o: /usr/dt/include/Xm/Display.h /usr/dt/include/Xm/DropSMgr.h
pixmaps.o: /usr/dt/include/Xm/DragIcon.h /usr/dt/include/Xm/DropTrans.h
pixmaps.o: /usr/dt/include/Xm/DragOverS.h /usr/dt/include/Xm/Primitive.h
pixmaps.o: /usr/dt/include/Xm/Manager.h /usr/dt/include/Xm/Gadget.h
pixmaps.o: /usr/dt/include/Xm/TxtPropCv.h /usr/dt/include/Xm/VendorS.h
pixmaps.o: /usr/dt/include/Xm/XmIm.h /programs/sol2/sh/source/sysbase.h
pixmaps.o: /programs/sol2/sh/source/fousrdef.h pixmaps.h
seismics.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
seismics.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
seismics.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
seismics.o: /usr/include/stdio_impl.h /usr/include/string.h
seismics.o: /usr/include/iso/string_iso.h /usr/include/math.h
seismics.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
seismics.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
seismics.o: /programs/sol2/sh/source/sysbase.h
seismics.o: /programs/sol2/sh/source/fousrdef.h
seismics.o: /programs/sol2/sh/source/erusrdef.h
seismics.o: /programs/sol2/sh/source/tcusrdef.h
seismics.o: /programs/sol2/sh/source/ptusrdef.h
seismics.o: /programs/sol2/sh/source/earthloc.h
seismics.o: /programs/sol2/sh/source/glusrdef.h seismics.h
seismics.o: /programs/sol2/sh/source/globalparams.h
spectrum.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
spectrum.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
spectrum.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
spectrum.o: /usr/include/stdio_impl.h /usr/include/string.h
spectrum.o: /usr/include/iso/string_iso.h /usr/include/math.h
spectrum.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
spectrum.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
spectrum.o: /programs/sol2/sh/source/sysbase.h
spectrum.o: /programs/sol2/sh/source/fousrdef.h
spectrum.o: /programs/sol2/sh/source/tcusrdef.h /usr/dt/include/Xm/Xm.h
spectrum.o: /usr/openwin/share/include/X11/Intrinsic.h
spectrum.o: /usr/openwin/share/include/X11/Xlib.h /usr/include/sys/types.h
spectrum.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
spectrum.o: /usr/include/sys/select.h /usr/include/sys/time.h
spectrum.o: /usr/include/sys/time.h /usr/openwin/share/include/X11/X.h
spectrum.o: /usr/openwin/share/include/X11/Xfuncproto.h
spectrum.o: /usr/openwin/share/include/X11/Xosdefs.h /usr/include/stddef.h
spectrum.o: /usr/include/iso/stddef_iso.h
spectrum.o: /usr/openwin/share/include/X11/Xutil.h
spectrum.o: /usr/openwin/share/include/X11/Xresource.h
spectrum.o: /usr/openwin/share/include/X11/Core.h
spectrum.o: /usr/openwin/share/include/X11/Composite.h
spectrum.o: /usr/openwin/share/include/X11/Constraint.h
spectrum.o: /usr/openwin/share/include/X11/Object.h
spectrum.o: /usr/openwin/share/include/X11/RectObj.h
spectrum.o: /usr/openwin/share/include/X11/Shell.h
spectrum.o: /usr/openwin/share/include/X11/SM/SMlib.h
spectrum.o: /usr/openwin/share/include/X11/SM/SM.h
spectrum.o: /usr/openwin/share/include/X11/ICE/ICElib.h
spectrum.o: /usr/openwin/share/include/X11/ICE/ICE.h
spectrum.o: /usr/openwin/share/include/X11/Xatom.h
spectrum.o: /usr/dt/include/Xm/XmStrDefs.h
spectrum.o: /usr/openwin/share/include/X11/StringDefs.h
spectrum.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/Transfer.h
spectrum.o: /usr/dt/include/Xm/DragDrop.h /usr/dt/include/Xm/DragC.h
spectrum.o: /usr/dt/include/Xm/Display.h /usr/dt/include/Xm/DropSMgr.h
spectrum.o: /usr/dt/include/Xm/DragIcon.h /usr/dt/include/Xm/DropTrans.h
spectrum.o: /usr/dt/include/Xm/DragOverS.h /usr/dt/include/Xm/Primitive.h
spectrum.o: /usr/dt/include/Xm/Manager.h /usr/dt/include/Xm/Gadget.h
spectrum.o: /usr/dt/include/Xm/TxtPropCv.h /usr/dt/include/Xm/VendorS.h
spectrum.o: /usr/dt/include/Xm/XmIm.h
spectrum.o: /usr/openwin/share/include/X11/cursorfont.h ../numres.h pixmaps.h
spectrum.o: motifgraph.h phaseinf.h phasemgr.h cbutil.h
spectrum.o: /programs/sol2/sh/source/glusrdef.h spectrum.h shm_widgets.h
spectrum.o: /programs/sol2/sh/source/globalparams.h
partmotion.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
partmotion.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
partmotion.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
partmotion.o: /usr/include/stdio_impl.h /usr/include/string.h
partmotion.o: /usr/include/iso/string_iso.h /usr/include/math.h
partmotion.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
partmotion.o: /programs/sol2/sh/source/sysbase.h
partmotion.o: /programs/sol2/sh/source/fousrdef.h /usr/dt/include/Xm/Xm.h
partmotion.o: /usr/openwin/share/include/X11/Intrinsic.h
partmotion.o: /usr/openwin/share/include/X11/Xlib.h /usr/include/sys/types.h
partmotion.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
partmotion.o: /usr/include/sys/select.h /usr/include/sys/time.h
partmotion.o: /usr/include/sys/time.h /usr/openwin/share/include/X11/X.h
partmotion.o: /usr/openwin/share/include/X11/Xfuncproto.h
partmotion.o: /usr/openwin/share/include/X11/Xosdefs.h /usr/include/stddef.h
partmotion.o: /usr/include/iso/stddef_iso.h
partmotion.o: /usr/openwin/share/include/X11/Xutil.h
partmotion.o: /usr/openwin/share/include/X11/Xresource.h
partmotion.o: /usr/openwin/share/include/X11/Core.h
partmotion.o: /usr/openwin/share/include/X11/Composite.h
partmotion.o: /usr/openwin/share/include/X11/Constraint.h
partmotion.o: /usr/openwin/share/include/X11/Object.h
partmotion.o: /usr/openwin/share/include/X11/RectObj.h
partmotion.o: /usr/openwin/share/include/X11/Shell.h
partmotion.o: /usr/openwin/share/include/X11/SM/SMlib.h
partmotion.o: /usr/openwin/share/include/X11/SM/SM.h
partmotion.o: /usr/openwin/share/include/X11/ICE/ICElib.h
partmotion.o: /usr/openwin/share/include/X11/ICE/ICE.h
partmotion.o: /usr/openwin/share/include/X11/Xatom.h
partmotion.o: /usr/dt/include/Xm/XmStrDefs.h
partmotion.o: /usr/openwin/share/include/X11/StringDefs.h
partmotion.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/Transfer.h
partmotion.o: /usr/dt/include/Xm/DragDrop.h /usr/dt/include/Xm/DragC.h
partmotion.o: /usr/dt/include/Xm/Display.h /usr/dt/include/Xm/DropSMgr.h
partmotion.o: /usr/dt/include/Xm/DragIcon.h /usr/dt/include/Xm/DropTrans.h
partmotion.o: /usr/dt/include/Xm/DragOverS.h /usr/dt/include/Xm/Primitive.h
partmotion.o: /usr/dt/include/Xm/Manager.h /usr/dt/include/Xm/Gadget.h
partmotion.o: /usr/dt/include/Xm/TxtPropCv.h /usr/dt/include/Xm/VendorS.h
partmotion.o: /usr/dt/include/Xm/XmIm.h /programs/sol2/sh/source/infoidx.h
partmotion.o: /programs/sol2/sh/source/shconst.h
partmotion.o: /programs/sol2/sh/source/tcusrdef.h
partmotion.o: /programs/sol2/sh/source/newgraph/gcusrdef.h
partmotion.o: /programs/sol2/sh/source/newgraph/graphbas.h
partmotion.o: /programs/sol2/sh/source/fctxdm.h
partmotion.o: /programs/sol2/sh/source/scusrdef.h motifgraph.h phaseinf.h
partmotion.o: phasemgr.h partmotion.h shm_widgets.h pixmaps.h
partmotion.o: /programs/sol2/sh/source/globalparams.h
polarfil.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
polarfil.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
polarfil.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
polarfil.o: /usr/include/stdio_impl.h /usr/include/string.h
polarfil.o: /usr/include/iso/string_iso.h /usr/include/math.h
polarfil.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
polarfil.o: /programs/sol2/sh/source/sysbase.h
polarfil.o: /programs/sol2/sh/source/fousrdef.h /usr/dt/include/Xm/Xm.h
polarfil.o: /usr/openwin/share/include/X11/Intrinsic.h
polarfil.o: /usr/openwin/share/include/X11/Xlib.h /usr/include/sys/types.h
polarfil.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
polarfil.o: /usr/include/sys/select.h /usr/include/sys/time.h
polarfil.o: /usr/include/sys/time.h /usr/openwin/share/include/X11/X.h
polarfil.o: /usr/openwin/share/include/X11/Xfuncproto.h
polarfil.o: /usr/openwin/share/include/X11/Xosdefs.h /usr/include/stddef.h
polarfil.o: /usr/include/iso/stddef_iso.h
polarfil.o: /usr/openwin/share/include/X11/Xutil.h
polarfil.o: /usr/openwin/share/include/X11/Xresource.h
polarfil.o: /usr/openwin/share/include/X11/Core.h
polarfil.o: /usr/openwin/share/include/X11/Composite.h
polarfil.o: /usr/openwin/share/include/X11/Constraint.h
polarfil.o: /usr/openwin/share/include/X11/Object.h
polarfil.o: /usr/openwin/share/include/X11/RectObj.h
polarfil.o: /usr/openwin/share/include/X11/Shell.h
polarfil.o: /usr/openwin/share/include/X11/SM/SMlib.h
polarfil.o: /usr/openwin/share/include/X11/SM/SM.h
polarfil.o: /usr/openwin/share/include/X11/ICE/ICElib.h
polarfil.o: /usr/openwin/share/include/X11/ICE/ICE.h
polarfil.o: /usr/openwin/share/include/X11/Xatom.h
polarfil.o: /usr/dt/include/Xm/XmStrDefs.h
polarfil.o: /usr/openwin/share/include/X11/StringDefs.h
polarfil.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/Transfer.h
polarfil.o: /usr/dt/include/Xm/DragDrop.h /usr/dt/include/Xm/DragC.h
polarfil.o: /usr/dt/include/Xm/Display.h /usr/dt/include/Xm/DropSMgr.h
polarfil.o: /usr/dt/include/Xm/DragIcon.h /usr/dt/include/Xm/DropTrans.h
polarfil.o: /usr/dt/include/Xm/DragOverS.h /usr/dt/include/Xm/Primitive.h
polarfil.o: /usr/dt/include/Xm/Manager.h /usr/dt/include/Xm/Gadget.h
polarfil.o: /usr/dt/include/Xm/TxtPropCv.h /usr/dt/include/Xm/VendorS.h
polarfil.o: /usr/dt/include/Xm/XmIm.h /programs/sol2/sh/source/infoidx.h
polarfil.o: /programs/sol2/sh/source/shconst.h
polarfil.o: /programs/sol2/sh/source/tcusrdef.h
polarfil.o: /programs/sol2/sh/source/newgraph/gcusrdef.h
polarfil.o: /programs/sol2/sh/source/newgraph/graphbas.h
polarfil.o: /programs/sol2/sh/source/fctxdm.h
polarfil.o: /programs/sol2/sh/source/scusrdef.h motifgraph.h phaseinf.h
polarfil.o: phasemgr.h polarfil.h shm_widgets.h ../pfusrdef.h
trcselect.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
trcselect.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
trcselect.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
trcselect.o: /usr/include/stdio_impl.h /usr/include/string.h
trcselect.o: /usr/include/iso/string_iso.h
trcselect.o: /programs/sol2/sh/source/basecnst.h
trcselect.o: /programs/sol2/sh/source/sysbase.h
trcselect.o: /programs/sol2/sh/source/fousrdef.h trcselect.h
shm.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shm.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shm.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shm.o: /usr/include/stdio_impl.h /usr/include/string.h
shm.o: /usr/include/iso/string_iso.h /usr/include/ctype.h
shm.o: /usr/include/iso/ctype_iso.h /usr/include/math.h
shm.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
shm.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
shm.o: /usr/dt/include/Xm/Xm.h /usr/openwin/share/include/X11/Intrinsic.h
shm.o: /usr/openwin/share/include/X11/Xlib.h /usr/include/sys/types.h
shm.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
shm.o: /usr/include/sys/select.h /usr/include/sys/time.h
shm.o: /usr/include/sys/time.h /usr/openwin/share/include/X11/X.h
shm.o: /usr/openwin/share/include/X11/Xfuncproto.h
shm.o: /usr/openwin/share/include/X11/Xosdefs.h /usr/include/stddef.h
shm.o: /usr/include/iso/stddef_iso.h /usr/openwin/share/include/X11/Xutil.h
shm.o: /usr/openwin/share/include/X11/Xresource.h
shm.o: /usr/openwin/share/include/X11/Core.h
shm.o: /usr/openwin/share/include/X11/Composite.h
shm.o: /usr/openwin/share/include/X11/Constraint.h
shm.o: /usr/openwin/share/include/X11/Object.h
shm.o: /usr/openwin/share/include/X11/RectObj.h
shm.o: /usr/openwin/share/include/X11/Shell.h
shm.o: /usr/openwin/share/include/X11/SM/SMlib.h
shm.o: /usr/openwin/share/include/X11/SM/SM.h
shm.o: /usr/openwin/share/include/X11/ICE/ICElib.h
shm.o: /usr/openwin/share/include/X11/ICE/ICE.h
shm.o: /usr/openwin/share/include/X11/Xatom.h /usr/dt/include/Xm/XmStrDefs.h
shm.o: /usr/openwin/share/include/X11/StringDefs.h
shm.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/Transfer.h
shm.o: /usr/dt/include/Xm/DragDrop.h /usr/dt/include/Xm/DragC.h
shm.o: /usr/dt/include/Xm/Display.h /usr/dt/include/Xm/DropSMgr.h
shm.o: /usr/dt/include/Xm/DragIcon.h /usr/dt/include/Xm/DropTrans.h
shm.o: /usr/dt/include/Xm/DragOverS.h /usr/dt/include/Xm/Primitive.h
shm.o: /usr/dt/include/Xm/Manager.h /usr/dt/include/Xm/Gadget.h
shm.o: /usr/dt/include/Xm/TxtPropCv.h /usr/dt/include/Xm/VendorS.h
shm.o: /usr/dt/include/Xm/XmIm.h /usr/dt/include/Xm/ToggleB.h
shm.o: /usr/dt/include/Xm/Scale.h /usr/dt/include/Xm/Text.h
shm.o: /usr/dt/include/Mrm/MrmPublic.h /usr/openwin/share/include/X11/Xmd.h
shm.o: /usr/dt/include/Mrm/MrmDecls.h /programs/sol2/sh/source/sysbase.h
shm.o: /programs/sol2/sh/source/fousrdef.h /programs/sol2/sh/source/shconst.h
shm.o: /programs/sol2/sh/source/shvars.h /programs/sol2/sh/source/shdirs.h
shm.o: /programs/sol2/sh/source/tcusrdef.h
shm.o: /programs/sol2/sh/source/earthloc.h phaseinf.h phasemgr.h
shm.o: /programs/sol2/sh/source/seusrdef.h /programs/sol2/sh/source/sysbase.h
shm.o: /programs/sol2/sh/source/newgraph/gcusrdef.h
shm.o: /programs/sol2/sh/source/newgraph/graphbas.h
shm.o: /programs/sol2/sh/source/cpusrdef.h /programs/sol2/sh/source/fctxmn5.h
shm.o: /programs/sol2/sh/source/callsh.h /programs/sol2/sh/source/infoidx.h
shm.o: /programs/sol2/sh/source/glusrdef.h motifgraph.h cbutil.h mfexec.h
shm.o: /programs/sol2/sh/source/station_no.h pixmaps.h seismics.h cblib.h
shm.o: cblib2.h cblib3.h cblib4.h calibloc.h spectrum.h partmotion.h
shm.o: polarfil.h shm_widgets.h /programs/sol2/sh/source/seed_io/seedcfg.h
shm.o: /programs/sol2/sh/source/seed_io/seed_lib.h
shm.o: /programs/sol2/sh/source/globalparams.h
shm.o: /programs/sol2/sh/source/sqliface.h
