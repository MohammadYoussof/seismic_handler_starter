# file Makefile
#      ========
#
# version 14, 7-Oct-2006
#
# Makefile of directory $SH_ROOT/source/img
# K. Stammler, 7-Mar-95

CFLAGS      = $(SH_CCOPTIONS) -I$(SH_SOURCE) -I$(SH_UTIL) -I$(SH_SOURCE)/newgraph -I$(SH_SOURCE)/foreign -DBASECNST=\"basecnst.h\"
CC          = $(SH_COMPILER)
LD          = $(SH_COMPILER)
LDFLAGS     = -L$(SH_LIB) $(SH_LINKOPTIONS)
#LIBS        = -lmotif -lsource -lm -lMrm -lXm -lXt -lXp -lXext -lX11 $(SH_LINKLIBS) \
#		$(SH_MOTIFLIBS)
LIBS        = -lmotif -lsource -lm -lMrm -lXm -lXt -lXext -lX11 $(SH_LINKLIBS) \
		$(SH_MOTIFLIBS)

ANINAME     = animatrix
PROGNAME		= mapmatrix

LIB_SOURCE	= $(SH_LIB)libsource.a
LIB_MOTIF	= $(SH_LIB)libmotif.a
ALL_IMG_LIBS	= $(LIB_MOTIF) $(LIB_SOURCE)


all:	$(PROGNAME) $(PROGNAME).uid $(ANINAME) $(ANINAME).uid mapslow mapslow.uid mapspec mapspec.uid

$(LIB_MOTIF):	$(LOCAL_OBJS)
	@echo "Motif-Lib not up to date"

$(LIB_SOURCE):
	@echo "Source-Lib not up to date"

$(PROGNAME):	$(PROGNAME).o $(ALL_IMG_LIBS)
	@echo "Linking $(PROGNAME) ..."
	$(LD) $(PROGNAME).o $(LDFLAGS) $(LIBS) -o $@

$(ANINAME):	$(ANINAME).o $(ALL_IMG_LIBS)
	@echo "Linking $(ANINAME) ..."
	$(LD) $(ANINAME).o $(LDFLAGS) $(LIBS) -o $@

mapslow:	mapslow.o $(ALL_IMG_LIBS)
	@echo "Linking mapslow ..."
	$(LD) mapslow.o $(LDFLAGS) $(LIBS) -o $@

mapspec:	mapspec.o $(ALL_IMG_LIBS)
	@echo "Linking mapspec ..."
	$(LD) mapspec.o $(LDFLAGS) $(LIBS) -o $@

$(PROGNAME).uid:	$(PROGNAME).uil
	(LANG=en; uil -o $@ $(PROGNAME).uil)

$(ANINAME).uid:	$(ANINAME).uil
	(LANG=en; uil -o $@ $(ANINAME).uil)

mapslow.uid:	mapslow.uil
	(LANG=en; uil -o $@ mapslow.uil)

mapspec.uid:	mapspec.uil
	(LANG=en; uil -o $@ mapspec.uil)


clean:
	-@rm $(PROGNAME).o $(PROGNAME) $(PROGNAME).uid $(ANINAME).o $(ANINAME) $(ANINAME).uid mapslow mapslow.o mapslow.uid mapspec mapspec.o mapspec.uid

depend:
	makedepend -s "# DO NOT DELETE" -- $(CFLAGS) -- mapmatrix.c animatrix.c mapslow.c

# DO NOT DELETE

mapmatrix.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
mapmatrix.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
mapmatrix.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
mapmatrix.o: /usr/include/stdio_impl.h /usr/include/math.h
mapmatrix.o: /usr/include/iso/math_iso.h ../basecnst.h
mapmatrix.o: /usr/dt/include/Xm/Xm.h
mapmatrix.o: /usr/openwin/share/include/X11/Intrinsic.h
mapmatrix.o: /usr/openwin/share/include/X11/Xlib.h /usr/include/sys/types.h
mapmatrix.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
mapmatrix.o: /usr/include/sys/select.h /usr/include/sys/time.h
mapmatrix.o: /usr/include/sys/time.h /usr/openwin/share/include/X11/X.h
mapmatrix.o: /usr/openwin/share/include/X11/Xfuncproto.h
mapmatrix.o: /usr/openwin/share/include/X11/Xosdefs.h /usr/include/stddef.h
mapmatrix.o: /usr/include/iso/stddef_iso.h
mapmatrix.o: /usr/openwin/share/include/X11/Xutil.h
mapmatrix.o: /usr/openwin/share/include/X11/Xresource.h /usr/include/string.h
mapmatrix.o: /usr/include/iso/string_iso.h
mapmatrix.o: /usr/openwin/share/include/X11/Core.h
mapmatrix.o: /usr/openwin/share/include/X11/Composite.h
mapmatrix.o: /usr/openwin/share/include/X11/Constraint.h
mapmatrix.o: /usr/openwin/share/include/X11/Object.h
mapmatrix.o: /usr/openwin/share/include/X11/RectObj.h
mapmatrix.o: /usr/openwin/share/include/X11/Shell.h
mapmatrix.o: /usr/openwin/share/include/X11/SM/SMlib.h
mapmatrix.o: /usr/openwin/share/include/X11/SM/SM.h
mapmatrix.o: /usr/openwin/share/include/X11/ICE/ICElib.h
mapmatrix.o: /usr/openwin/share/include/X11/ICE/ICE.h
mapmatrix.o: /usr/openwin/share/include/X11/Xatom.h
mapmatrix.o: /usr/dt/include/Xm/XmStrDefs.h
mapmatrix.o: /usr/openwin/share/include/X11/StringDefs.h
mapmatrix.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/Transfer.h
mapmatrix.o: /usr/dt/include/Xm/DragDrop.h /usr/dt/include/Xm/DragC.h
mapmatrix.o: /usr/dt/include/Xm/Display.h /usr/dt/include/Xm/DropSMgr.h
mapmatrix.o: /usr/dt/include/Xm/DragIcon.h /usr/dt/include/Xm/DropTrans.h
mapmatrix.o: /usr/dt/include/Xm/DragOverS.h /usr/dt/include/Xm/Primitive.h
mapmatrix.o: /usr/dt/include/Xm/Manager.h /usr/dt/include/Xm/Gadget.h
mapmatrix.o: /usr/dt/include/Xm/TxtPropCv.h /usr/dt/include/Xm/VendorS.h
mapmatrix.o: /usr/dt/include/Xm/XmIm.h /usr/dt/include/Mrm/MrmPublic.h
mapmatrix.o: /usr/openwin/share/include/X11/Xmd.h
mapmatrix.o: /usr/dt/include/Mrm/MrmDecls.h /usr/include/stdlib.h
mapmatrix.o: /usr/include/iso/stdlib_iso.h /programs/sol2/sh/source/sysbase.h
mapmatrix.o: /programs/sol2/sh/source/fousrdef.h
mapmatrix.o: /programs/sol2/sh/source/cpar.h ../motif/pixmaps.h ../tcusrdef.h
animatrix.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
animatrix.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
animatrix.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
animatrix.o: /usr/include/stdio_impl.h /usr/include/math.h
animatrix.o: /usr/include/iso/math_iso.h ../basecnst.h
animatrix.o: /usr/dt/include/Xm/Xm.h
animatrix.o: /usr/openwin/share/include/X11/Intrinsic.h
animatrix.o: /usr/openwin/share/include/X11/Xlib.h /usr/include/sys/types.h
animatrix.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
animatrix.o: /usr/include/sys/select.h /usr/include/sys/time.h
animatrix.o: /usr/include/sys/time.h /usr/openwin/share/include/X11/X.h
animatrix.o: /usr/openwin/share/include/X11/Xfuncproto.h
animatrix.o: /usr/openwin/share/include/X11/Xosdefs.h /usr/include/stddef.h
animatrix.o: /usr/include/iso/stddef_iso.h
animatrix.o: /usr/openwin/share/include/X11/Xutil.h
animatrix.o: /usr/openwin/share/include/X11/Xresource.h /usr/include/string.h
animatrix.o: /usr/include/iso/string_iso.h
animatrix.o: /usr/openwin/share/include/X11/Core.h
animatrix.o: /usr/openwin/share/include/X11/Composite.h
animatrix.o: /usr/openwin/share/include/X11/Constraint.h
animatrix.o: /usr/openwin/share/include/X11/Object.h
animatrix.o: /usr/openwin/share/include/X11/RectObj.h
animatrix.o: /usr/openwin/share/include/X11/Shell.h
animatrix.o: /usr/openwin/share/include/X11/SM/SMlib.h
animatrix.o: /usr/openwin/share/include/X11/SM/SM.h
animatrix.o: /usr/openwin/share/include/X11/ICE/ICElib.h
animatrix.o: /usr/openwin/share/include/X11/ICE/ICE.h
animatrix.o: /usr/openwin/share/include/X11/Xatom.h
animatrix.o: /usr/dt/include/Xm/XmStrDefs.h
animatrix.o: /usr/openwin/share/include/X11/StringDefs.h
animatrix.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/Transfer.h
animatrix.o: /usr/dt/include/Xm/DragDrop.h /usr/dt/include/Xm/DragC.h
animatrix.o: /usr/dt/include/Xm/Display.h /usr/dt/include/Xm/DropSMgr.h
animatrix.o: /usr/dt/include/Xm/DragIcon.h /usr/dt/include/Xm/DropTrans.h
animatrix.o: /usr/dt/include/Xm/DragOverS.h /usr/dt/include/Xm/Primitive.h
animatrix.o: /usr/dt/include/Xm/Manager.h /usr/dt/include/Xm/Gadget.h
animatrix.o: /usr/dt/include/Xm/TxtPropCv.h /usr/dt/include/Xm/VendorS.h
animatrix.o: /usr/dt/include/Xm/XmIm.h /usr/dt/include/Mrm/MrmPublic.h
animatrix.o: /usr/openwin/share/include/X11/Xmd.h
animatrix.o: /usr/dt/include/Mrm/MrmDecls.h /usr/include/stdlib.h
animatrix.o: /usr/include/iso/stdlib_iso.h /programs/sol2/sh/source/sysbase.h
animatrix.o: /programs/sol2/sh/source/fousrdef.h
animatrix.o: /programs/sol2/sh/source/cpar.h ../motif/pixmaps.h ../tcusrdef.h
mapslow.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
mapslow.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
mapslow.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
mapslow.o: /usr/include/stdio_impl.h /usr/include/math.h
mapslow.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
mapslow.o: /usr/dt/include/Xm/Xm.h /usr/openwin/share/include/X11/Intrinsic.h
mapslow.o: /usr/openwin/share/include/X11/Xlib.h /usr/include/sys/types.h
mapslow.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
mapslow.o: /usr/include/sys/select.h /usr/include/sys/time.h
mapslow.o: /usr/include/sys/time.h /usr/openwin/share/include/X11/X.h
mapslow.o: /usr/openwin/share/include/X11/Xfuncproto.h
mapslow.o: /usr/openwin/share/include/X11/Xosdefs.h /usr/include/stddef.h
mapslow.o: /usr/include/iso/stddef_iso.h
mapslow.o: /usr/openwin/share/include/X11/Xutil.h
mapslow.o: /usr/openwin/share/include/X11/Xresource.h /usr/include/string.h
mapslow.o: /usr/include/iso/string_iso.h
mapslow.o: /usr/openwin/share/include/X11/Core.h
mapslow.o: /usr/openwin/share/include/X11/Composite.h
mapslow.o: /usr/openwin/share/include/X11/Constraint.h
mapslow.o: /usr/openwin/share/include/X11/Object.h
mapslow.o: /usr/openwin/share/include/X11/RectObj.h
mapslow.o: /usr/openwin/share/include/X11/Shell.h
mapslow.o: /usr/openwin/share/include/X11/SM/SMlib.h
mapslow.o: /usr/openwin/share/include/X11/SM/SM.h
mapslow.o: /usr/openwin/share/include/X11/ICE/ICElib.h
mapslow.o: /usr/openwin/share/include/X11/ICE/ICE.h
mapslow.o: /usr/openwin/share/include/X11/Xatom.h
mapslow.o: /usr/dt/include/Xm/XmStrDefs.h
mapslow.o: /usr/openwin/share/include/X11/StringDefs.h
mapslow.o: /usr/dt/include/Xm/VirtKeys.h /usr/dt/include/Xm/Transfer.h
mapslow.o: /usr/dt/include/Xm/DragDrop.h /usr/dt/include/Xm/DragC.h
mapslow.o: /usr/dt/include/Xm/Display.h /usr/dt/include/Xm/DropSMgr.h
mapslow.o: /usr/dt/include/Xm/DragIcon.h /usr/dt/include/Xm/DropTrans.h
mapslow.o: /usr/dt/include/Xm/DragOverS.h /usr/dt/include/Xm/Primitive.h
mapslow.o: /usr/dt/include/Xm/Manager.h /usr/dt/include/Xm/Gadget.h
mapslow.o: /usr/dt/include/Xm/TxtPropCv.h /usr/dt/include/Xm/VendorS.h
mapslow.o: /usr/dt/include/Xm/XmIm.h /usr/dt/include/Mrm/MrmPublic.h
mapslow.o: /usr/openwin/share/include/X11/Xmd.h
mapslow.o: /usr/dt/include/Mrm/MrmDecls.h
mapslow.o: /usr/openwin/share/include/X11/cursorfont.h /usr/include/stdlib.h
mapslow.o: /usr/include/iso/stdlib_iso.h /programs/sol2/sh/source/sysbase.h
mapslow.o: /programs/sol2/sh/source/fousrdef.h
mapslow.o: /programs/sol2/sh/source/cpar.h ../motif/pixmaps.h
mapslow.o: /programs/sol2/sh/source/ptusrdef.h
mapslow.o: /programs/sol2/sh/source/tcusrdef.h
