
# Makefile for SH
# ===============
#
# version 21, 21-Dec-2006
#
# Makefile for directory $SH_SOURCE
# K. Stammler, 11-Feb-93

CFLAGS      = $(SH_CCOPTIONS) -I$(SH_SOURCE) -I$(SH_UTIL) -I$(SH_SOURCE)/newgraph -I$(SH_SOURCE)/foreign -DBASECNST=\"basecnst.h\"
CC          = $(SH_COMPILER)
LD          = $(SH_COMPILER)
RANLIB		= $(SH_RANLIB)
LDFLAGS     = -L$(SH_LIB) $(SH_LINKOPTIONS)
LIBS        = -lgraph -lsource -lgcf -lseed -lfrgn -lsource -lm -lX11 $(SH_LINKLIBS)
#                                              ^
#          this second '-lsource' is necessary /

MAKEFILE		= Makefile
PROGNAME    = ../shc
SHDAEMON    = ../shd

LIB_LOCAL	= $(SH_LIB)libsource.a
LIB_SEED		= $(SH_LIB)libseed.a
LIB_GCF		= $(SH_LIB)libgcf.a
LIB_GRAPH	= $(SH_LIB)libgraph.a
LIB_UTIL		= $(SH_LIB)libutil.a
LIB_FRGN		= $(SH_LIB)libfrgn.a
ALL_SH_LIBS	= $(LIB_LOCAL) $(LIB_GRAPH) $(LIB_SEED) $(LIB_GCF) $(LIB_UTIL) $(LIB_FRGN)
SHD_OBJS        = port_io.o
SHD_LIBS        = -lsocket -lnsl

LOCAL_SRCS	= \
	cmdargs.c   logplot.c   recfiltr.c  shlib.c     shmenuio.c  timeconv.c \
	cmdparse.c  matrixhd.c  shmath.c    shmenux.c   travtime.c  residual.c \
	cpar.c      numres.c    shcorr.c    shmenu0.c   shmidlev.c  usrinpw.c \
	earthloc.c  polfiltr.c  shcursor.c  shmenu1.c   shpm.c      utility.c \
	errmsg.c    ptravtim.c  shdataba.c  shmenu2.c   shsymbol.c  port_io.c \
	fftfiltr.c  qfileio.c   shdspmgr.c  shmenu3.c   shtransl.c  sqliface.c \
	fileopen.c  qfintrfc.c  shexec.c    shmenu4.c   statloc.c   globalparams.c \
	incitheo.c  shhelp.c    shmenu5.c   syscall.c   callsh.c

#LOCAL_OBJS	=	$(LOCAL_SRCS:%.c=%.o)
LOCAL_OBJS	=	$(LOCAL_SRCS:.c=.o)

all:  $(PROGNAME)

$(LIB_LOCAL):	$(LOCAL_OBJS)
	ar r $@ $(LOCAL_OBJS); $(RANLIB) $@

$(LIB_GRAPH):  FORCE
	cd newgraph; make

$(LIB_SEED):   FORCE
	cd seed_io; make

$(LIB_GCF):   FORCE
	cd gcf; make

$(LIB_UTIL):   FORCE
	cd ../util; make

$(LIB_FRGN):   FORCE
	cd foreign; make

FORCE:

$(PROGNAME):	seismhan.o $(MAKEFILE) $(ALL_SH_LIBS)
	@echo "Linking $(PROGNAME) ..."
	$(LD) seismhan.o $(LDFLAGS) $(LIBS) -o $(PROGNAME)

#$(SHDAEMON):	shd.o $(MAKEFILE) $(SHD_OBJS)
#	@echo "Linking $(SHDAEMON) ..."
#	$(LD) shd.o $(SHD_OBJS) $(SHD_LIBS) -o $(SHDAEMON)


clean:
	-@rm *% $(LOCAL_OBJS) seismhan.o $(ALL_SH_LIBS) $(PROGNAME) $(PROGNAME)_world
	-@cd seed_io; make clean
	-@cd foreign; make clean
	-@cd newgraph; make clean
	-@cd ../util; make clean


depend:
	makedepend -s "# DO NOT DELETE" -- $(CFLAGS) -- $(LOCAL_SRCS) seismhan.c


# DO NOT DELETE

cmdargs.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
cmdargs.o: /usr/include/sys/_symbol_aliasing.h
cmdargs.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
cmdargs.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
cmdargs.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
cmdargs.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
cmdargs.o: /usr/include/secure/_common.h /usr/include/string.h
cmdargs.o: /usr/include/strings.h /usr/include/secure/_string.h
cmdargs.o: /usr/include/ctype.h /usr/include/runetype.h basecnst.h sysbase.h
cmdargs.o: fousrdef.h
logplot.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
logplot.o: /usr/include/sys/_symbol_aliasing.h
logplot.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
logplot.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
logplot.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
logplot.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
logplot.o: /usr/include/secure/_common.h /usr/include/string.h
logplot.o: /usr/include/strings.h /usr/include/secure/_string.h
logplot.o: /usr/include/math.h /usr/include/architecture/i386/math.h
logplot.o: basecnst.h sysbase.h fousrdef.h
logplot.o: /usr/local/sh/sh/source/newgraph/gcusrdef.h
logplot.o: /usr/local/sh/sh/source/newgraph/graphbas.h shconst.h lgusrdef.h
recfiltr.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
recfiltr.o: /usr/include/sys/_symbol_aliasing.h
recfiltr.o: /usr/include/sys/_posix_availability.h
recfiltr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
recfiltr.o: /usr/include/_types.h /usr/include/sys/_types.h
recfiltr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
recfiltr.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
recfiltr.o: /usr/include/string.h /usr/include/strings.h
recfiltr.o: /usr/include/secure/_string.h basecnst.h /usr/include/stdlib.h
recfiltr.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
recfiltr.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
recfiltr.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
recfiltr.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
recfiltr.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
recfiltr.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
recfiltr.o: /usr/include/libkern/_OSByteOrder.h
recfiltr.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
recfiltr.o: /usr/include/machine/types.h /usr/include/i386/types.h sysbase.h
recfiltr.o: fousrdef.h shconst.h shvars.h erusrdef.h numres.h rfusrdef.h
recfiltr.o: flerrors.h globalparams.h
shlib.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
shlib.o: /usr/include/sys/_symbol_aliasing.h
shlib.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
shlib.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
shlib.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
shlib.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
shlib.o: /usr/include/secure/_common.h /usr/include/string.h
shlib.o: /usr/include/strings.h /usr/include/secure/_string.h
shlib.o: /usr/include/ctype.h /usr/include/runetype.h /usr/include/math.h
shlib.o: /usr/include/architecture/i386/math.h basecnst.h sysbase.h
shlib.o: fousrdef.h shconst.h utusrdef.h cpusrdef.h ssusrdef.h erusrdef.h
shlib.o: trusrdef.h tcusrdef.h /usr/local/sh/sh/source/newgraph/gcusrdef.h
shlib.o: /usr/local/sh/sh/source/newgraph/graphbas.h fctxsl.h sserrors.h
shlib.o: sherrors.h globalparams.h seed_io/seedcfg.h sqliface.h
shmenuio.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
shmenuio.o: /usr/include/sys/_symbol_aliasing.h
shmenuio.o: /usr/include/sys/_posix_availability.h
shmenuio.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
shmenuio.o: /usr/include/_types.h /usr/include/sys/_types.h
shmenuio.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
shmenuio.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
shmenuio.o: /usr/include/string.h /usr/include/strings.h
shmenuio.o: /usr/include/secure/_string.h /usr/include/ctype.h
shmenuio.o: /usr/include/runetype.h basecnst.h /usr/include/stdlib.h
shmenuio.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
shmenuio.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
shmenuio.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
shmenuio.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
shmenuio.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
shmenuio.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
shmenuio.o: /usr/include/libkern/_OSByteOrder.h
shmenuio.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
shmenuio.o: /usr/include/machine/types.h /usr/include/i386/types.h sysbase.h
shmenuio.o: fousrdef.h /usr/local/sh/sh/source/newgraph/gcusrdef.h
shmenuio.o: /usr/local/sh/sh/source/newgraph/graphbas.h shconst.h shvars.h
shmenuio.o: cpusrdef.h qfusrdef.h qiusrdef.h utusrdef.h erusrdef.h tcusrdef.h
shmenuio.o: glusrdef.h infoidx.h fctxml.h fctxsl.h
shmenuio.o: /usr/local/sh/sh/source/foreign/shfrgn.h sherrors.h
shmenuio.o: seed_io/seedcfg.h seed_io/seed_lib.h seed_io/seed_cd.h
shmenuio.o: gcf/gcflib.h sqliface.h globalparams.h
timeconv.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
timeconv.o: /usr/include/sys/_symbol_aliasing.h
timeconv.o: /usr/include/sys/_posix_availability.h
timeconv.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
timeconv.o: /usr/include/_types.h /usr/include/sys/_types.h
timeconv.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
timeconv.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
timeconv.o: /usr/include/ctype.h /usr/include/runetype.h
timeconv.o: /usr/include/string.h /usr/include/strings.h
timeconv.o: /usr/include/secure/_string.h basecnst.h sysbase.h fousrdef.h
timeconv.o: tcusrdef.h utusrdef.h tcerrors.h
cmdparse.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
cmdparse.o: /usr/include/sys/_symbol_aliasing.h
cmdparse.o: /usr/include/sys/_posix_availability.h
cmdparse.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
cmdparse.o: /usr/include/_types.h /usr/include/sys/_types.h
cmdparse.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
cmdparse.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
cmdparse.o: /usr/include/string.h /usr/include/strings.h
cmdparse.o: /usr/include/secure/_string.h basecnst.h sysbase.h fousrdef.h
cmdparse.o: erusrdef.h cpusrdef.h cperrors.h uiusrdef.h
cmdparse.o: /usr/local/sh/sh/source/newgraph/gcusrdef.h
cmdparse.o: /usr/local/sh/sh/source/newgraph/graphbas.h
cmdparse.o: /usr/local/sh/sh/source/newgraph/gcusrdef.h
matrixhd.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
matrixhd.o: /usr/include/sys/_symbol_aliasing.h
matrixhd.o: /usr/include/sys/_posix_availability.h
matrixhd.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
matrixhd.o: /usr/include/_types.h /usr/include/sys/_types.h
matrixhd.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
matrixhd.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
matrixhd.o: /usr/include/math.h /usr/include/architecture/i386/math.h
matrixhd.o: basecnst.h sysbase.h fousrdef.h shconst.h mxusrdef.h
shmath.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
shmath.o: /usr/include/sys/_symbol_aliasing.h
shmath.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
shmath.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
shmath.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
shmath.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
shmath.o: /usr/include/secure/_common.h /usr/include/string.h
shmath.o: /usr/include/strings.h /usr/include/secure/_string.h
shmath.o: /usr/include/math.h /usr/include/architecture/i386/math.h
shmath.o: basecnst.h /usr/include/stdlib.h /usr/include/sys/wait.h
shmath.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
shmath.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
shmath.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
shmath.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
shmath.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
shmath.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
shmath.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
shmath.o: /usr/include/machine/types.h /usr/include/i386/types.h sysbase.h
shmath.o: fousrdef.h tcusrdef.h shconst.h shvars.h glusrdef.h fctxmt.h
shmath.o: numres.h sherrors.h
shmenux.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
shmenux.o: /usr/include/sys/_symbol_aliasing.h
shmenux.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
shmenux.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
shmenux.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
shmenux.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
shmenux.o: /usr/include/secure/_common.h /usr/include/string.h
shmenux.o: /usr/include/strings.h /usr/include/secure/_string.h
shmenux.o: /usr/include/math.h /usr/include/architecture/i386/math.h
shmenux.o: basecnst.h sysbase.h fousrdef.h /usr/include/stdlib.h
shmenux.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
shmenux.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
shmenux.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
shmenux.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
shmenux.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
shmenux.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
shmenux.o: /usr/include/libkern/_OSByteOrder.h
shmenux.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
shmenux.o: /usr/include/machine/types.h /usr/include/i386/types.h shconst.h
shmenux.o: shvars.h cpusrdef.h glusrdef.h uiusrdef.h
shmenux.o: /usr/local/sh/sh/source/newgraph/gcusrdef.h ssusrdef.h ttusrdef.h
shmenux.o: tcusrdef.h infoidx.h fctxsl.h fctxdm.h fctxml.h fctxcr.h fctxmt.h
shmenux.o: fctxpm.h residual.h sherrors.h
travtime.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
travtime.o: /usr/include/sys/_symbol_aliasing.h
travtime.o: /usr/include/sys/_posix_availability.h
travtime.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
travtime.o: /usr/include/_types.h /usr/include/sys/_types.h
travtime.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
travtime.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
travtime.o: /usr/include/string.h /usr/include/strings.h
travtime.o: /usr/include/secure/_string.h /usr/include/math.h
travtime.o: /usr/include/architecture/i386/math.h basecnst.h sysbase.h
travtime.o: fousrdef.h numres.h erusrdef.h ttusrdef.h tterrors.h
residual.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
residual.o: /usr/include/sys/_symbol_aliasing.h
residual.o: /usr/include/sys/_posix_availability.h
residual.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
residual.o: /usr/include/_types.h /usr/include/sys/_types.h
residual.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
residual.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
residual.o: /usr/include/string.h /usr/include/strings.h
residual.o: /usr/include/secure/_string.h basecnst.h /usr/include/stdlib.h
residual.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
residual.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
residual.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
residual.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
residual.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
residual.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
residual.o: /usr/include/libkern/_OSByteOrder.h
residual.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
residual.o: /usr/include/machine/types.h /usr/include/i386/types.h sysbase.h
residual.o: fousrdef.h erusrdef.h utusrdef.h residual.h
cpar.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
cpar.o: /usr/include/sys/_symbol_aliasing.h
cpar.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
cpar.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
cpar.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
cpar.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
cpar.o: /usr/include/secure/_common.h /usr/include/string.h
cpar.o: /usr/include/strings.h /usr/include/secure/_string.h basecnst.h
cpar.o: sysbase.h fousrdef.h cpar.h
numres.o: /usr/include/math.h /usr/include/architecture/i386/math.h
numres.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
numres.o: /usr/include/sys/_posix_availability.h /usr/include/stdio.h
numres.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
numres.o: /usr/include/_types.h /usr/include/sys/_types.h
numres.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
numres.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
numres.o: basecnst.h sysbase.h fousrdef.h /usr/include/stdlib.h
numres.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
numres.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
numres.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
numres.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
numres.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
numres.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
numres.o: /usr/include/libkern/_OSByteOrder.h
numres.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
numres.o: /usr/include/machine/types.h /usr/include/i386/types.h numres.h
shcorr.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
shcorr.o: /usr/include/sys/_symbol_aliasing.h
shcorr.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
shcorr.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
shcorr.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
shcorr.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
shcorr.o: /usr/include/secure/_common.h /usr/include/math.h
shcorr.o: /usr/include/architecture/i386/math.h basecnst.h sysbase.h
shcorr.o: fousrdef.h shconst.h mxusrdef.h scusrdef.h scerrors.h
shmenu0.o: /usr/include/string.h /usr/include/_types.h
shmenu0.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
shmenu0.o: /usr/include/sys/_symbol_aliasing.h
shmenu0.o: /usr/include/sys/_posix_availability.h
shmenu0.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
shmenu0.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
shmenu0.o: /usr/include/strings.h /usr/include/secure/_string.h
shmenu0.o: /usr/include/secure/_common.h /usr/include/stdio.h
shmenu0.o: /usr/include/secure/_stdio.h /usr/include/math.h
shmenu0.o: /usr/include/architecture/i386/math.h basecnst.h sysbase.h
shmenu0.o: fousrdef.h /usr/include/stdlib.h /usr/include/sys/wait.h
shmenu0.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
shmenu0.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
shmenu0.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
shmenu0.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
shmenu0.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
shmenu0.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
shmenu0.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
shmenu0.o: /usr/include/machine/types.h /usr/include/i386/types.h
shmenu0.o: /usr/local/sh/sh/source/newgraph/gcusrdef.h shconst.h shvars.h
shmenu0.o: cpusrdef.h uiusrdef.h utusrdef.h scusrdef.h ssusrdef.h glusrdef.h
shmenu0.o: qfusrdef.h ttusrdef.h ptusrdef.h itusrdef.h erusrdef.h tcusrdef.h
shmenu0.o: infoidx.h fctxmn0.h fctxmni.h fctxhl.h fctxsl.h fctxdm.h
shmenu0.o: sherrors.h sserrors.h earthloc.h globalparams.h seed_io/seedcfg.h
shmenu0.o: seed_io/seed_lib.h
shmidlev.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
shmidlev.o: /usr/include/sys/_symbol_aliasing.h
shmidlev.o: /usr/include/sys/_posix_availability.h
shmidlev.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
shmidlev.o: /usr/include/_types.h /usr/include/sys/_types.h
shmidlev.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
shmidlev.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
shmidlev.o: /usr/include/string.h /usr/include/strings.h
shmidlev.o: /usr/include/secure/_string.h /usr/include/ctype.h
shmidlev.o: /usr/include/runetype.h basecnst.h /usr/include/stdlib.h
shmidlev.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
shmidlev.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
shmidlev.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
shmidlev.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
shmidlev.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
shmidlev.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
shmidlev.o: /usr/include/libkern/_OSByteOrder.h
shmidlev.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
shmidlev.o: /usr/include/machine/types.h /usr/include/i386/types.h sysbase.h
shmidlev.o: fousrdef.h shconst.h shvars.h cpusrdef.h utusrdef.h tcusrdef.h
shmidlev.o: infoidx.h fctxsl.h fctxcr.h fctxml.h fctxdm.h sherrors.h
usrinpw.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
usrinpw.o: /usr/include/sys/_symbol_aliasing.h
usrinpw.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
usrinpw.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
usrinpw.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
usrinpw.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
usrinpw.o: /usr/include/secure/_common.h /usr/include/string.h
usrinpw.o: /usr/include/strings.h /usr/include/secure/_string.h
usrinpw.o: /usr/include/ctype.h /usr/include/runetype.h basecnst.h sysbase.h
usrinpw.o: fousrdef.h /usr/local/sh/sh/source/newgraph/gcusrdef.h erusrdef.h
usrinpw.o: utusrdef.h uiusrdef.h uierrors.h globalparams.h
earthloc.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
earthloc.o: /usr/include/sys/_symbol_aliasing.h
earthloc.o: /usr/include/sys/_posix_availability.h
earthloc.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
earthloc.o: /usr/include/_types.h /usr/include/sys/_types.h
earthloc.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
earthloc.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
earthloc.o: /usr/include/string.h /usr/include/strings.h
earthloc.o: /usr/include/secure/_string.h /usr/include/math.h
earthloc.o: /usr/include/architecture/i386/math.h basecnst.h sysbase.h
earthloc.o: fousrdef.h shconst.h earthloc.h
polfiltr.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
polfiltr.o: /usr/include/sys/_symbol_aliasing.h
polfiltr.o: /usr/include/sys/_posix_availability.h
polfiltr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
polfiltr.o: /usr/include/_types.h /usr/include/sys/_types.h
polfiltr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
polfiltr.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
polfiltr.o: /usr/include/string.h /usr/include/strings.h
polfiltr.o: /usr/include/secure/_string.h /usr/include/math.h
polfiltr.o: /usr/include/architecture/i386/math.h basecnst.h sysbase.h
polfiltr.o: fousrdef.h shconst.h mxusrdef.h
shcursor.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
shcursor.o: /usr/include/sys/_symbol_aliasing.h
shcursor.o: /usr/include/sys/_posix_availability.h
shcursor.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
shcursor.o: /usr/include/_types.h /usr/include/sys/_types.h
shcursor.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
shcursor.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
shcursor.o: basecnst.h sysbase.h fousrdef.h shconst.h shvars.h
shcursor.o: /usr/local/sh/sh/source/newgraph/gcusrdef.h uiusrdef.h tcusrdef.h
shcursor.o: infoidx.h fctxcr.h fctxdm.h sherrors.h
shmenu1.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
shmenu1.o: /usr/include/sys/_symbol_aliasing.h
shmenu1.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
shmenu1.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
shmenu1.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
shmenu1.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
shmenu1.o: /usr/include/secure/_common.h /usr/include/string.h
shmenu1.o: /usr/include/strings.h /usr/include/secure/_string.h
shmenu1.o: /usr/include/math.h /usr/include/architecture/i386/math.h
shmenu1.o: basecnst.h sysbase.h fousrdef.h shconst.h shvars.h cpusrdef.h
shmenu1.o: uiusrdef.h /usr/local/sh/sh/source/newgraph/gcusrdef.h qfusrdef.h
shmenu1.o: qiusrdef.h ssusrdef.h erusrdef.h tcusrdef.h infoidx.h fctxml.h
shmenu1.o: fctxsl.h fctxdm.h fctxmt.h fctxcr.h sherrors.h qferrors.h
shpm.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
shpm.o: /usr/include/sys/_symbol_aliasing.h
shpm.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
shpm.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
shpm.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
shpm.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
shpm.o: /usr/include/secure/_common.h /usr/include/math.h
shpm.o: /usr/include/architecture/i386/math.h basecnst.h sysbase.h fousrdef.h
shpm.o: shconst.h /usr/local/sh/sh/source/newgraph/gcusrdef.h fctxsl.h
shpm.o: fctxcr.h fctxpm.h sherrors.h
utility.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
utility.o: /usr/include/sys/_symbol_aliasing.h
utility.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
utility.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
utility.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
utility.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
utility.o: /usr/include/secure/_common.h /usr/include/ctype.h
utility.o: /usr/include/runetype.h /usr/include/string.h
utility.o: /usr/include/strings.h /usr/include/secure/_string.h basecnst.h
utility.o: sysbase.h fousrdef.h uterrors.h utusrdef.h
errmsg.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
errmsg.o: /usr/include/sys/_symbol_aliasing.h
errmsg.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
errmsg.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
errmsg.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
errmsg.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
errmsg.o: /usr/include/secure/_common.h /usr/include/string.h
errmsg.o: /usr/include/strings.h /usr/include/secure/_string.h basecnst.h
errmsg.o: /usr/include/stdlib.h /usr/include/sys/wait.h
errmsg.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
errmsg.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
errmsg.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
errmsg.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
errmsg.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
errmsg.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
errmsg.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
errmsg.o: /usr/include/machine/types.h /usr/include/i386/types.h sysbase.h
errmsg.o: fousrdef.h erusrdef.h globalparams.h
ptravtim.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
ptravtim.o: /usr/include/sys/_symbol_aliasing.h
ptravtim.o: /usr/include/sys/_posix_availability.h
ptravtim.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
ptravtim.o: /usr/include/_types.h /usr/include/sys/_types.h
ptravtim.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
ptravtim.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
ptravtim.o: /usr/include/string.h /usr/include/strings.h
ptravtim.o: /usr/include/secure/_string.h /usr/include/ctype.h
ptravtim.o: /usr/include/runetype.h basecnst.h sysbase.h fousrdef.h
ptravtim.o: erusrdef.h ptusrdef.h pterrors.h
shdataba.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
shdataba.o: /usr/include/sys/_symbol_aliasing.h
shdataba.o: /usr/include/sys/_posix_availability.h
shdataba.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
shdataba.o: /usr/include/_types.h /usr/include/sys/_types.h
shdataba.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
shdataba.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
shdataba.o: /usr/include/string.h /usr/include/strings.h
shdataba.o: /usr/include/secure/_string.h basecnst.h /usr/include/stdlib.h
shdataba.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
shdataba.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
shdataba.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
shdataba.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
shdataba.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
shdataba.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
shdataba.o: /usr/include/libkern/_OSByteOrder.h
shdataba.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
shdataba.o: /usr/include/machine/types.h /usr/include/i386/types.h sysbase.h
shdataba.o: fousrdef.h shconst.h shvars.h qfusrdef.h qiusrdef.h tcusrdef.h
shdataba.o: /usr/local/sh/sh/source/newgraph/gcusrdef.h infoidx.h sherrors.h
shmenu2.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
shmenu2.o: /usr/include/sys/_symbol_aliasing.h
shmenu2.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
shmenu2.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
shmenu2.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
shmenu2.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
shmenu2.o: /usr/include/secure/_common.h /usr/include/string.h
shmenu2.o: /usr/include/strings.h /usr/include/secure/_string.h
shmenu2.o: /usr/include/math.h /usr/include/architecture/i386/math.h
shmenu2.o: basecnst.h /usr/include/stdlib.h /usr/include/sys/wait.h
shmenu2.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
shmenu2.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
shmenu2.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
shmenu2.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
shmenu2.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
shmenu2.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
shmenu2.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
shmenu2.o: /usr/include/machine/types.h /usr/include/i386/types.h sysbase.h
shmenu2.o: fousrdef.h shconst.h shvars.h cpusrdef.h rfusrdef.h ffusrdef.h
shmenu2.o: scusrdef.h ssusrdef.h erusrdef.h qiusrdef.h qfusrdef.h uiusrdef.h
shmenu2.o: /usr/local/sh/sh/source/newgraph/gcusrdef.h tcusrdef.h infoidx.h
shmenu2.o: fctxcr.h fctxdm.h fctxsl.h fctxml.h fctxmn2.h sherrors.h
shsymbol.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
shsymbol.o: /usr/include/sys/_symbol_aliasing.h
shsymbol.o: /usr/include/sys/_posix_availability.h
shsymbol.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
shsymbol.o: /usr/include/_types.h /usr/include/sys/_types.h
shsymbol.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
shsymbol.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
shsymbol.o: /usr/include/string.h /usr/include/strings.h
shsymbol.o: /usr/include/secure/_string.h /usr/include/ctype.h
shsymbol.o: /usr/include/runetype.h basecnst.h sysbase.h fousrdef.h shconst.h
shsymbol.o: shvars.h erusrdef.h cpusrdef.h sserrors.h ssusrdef.h
port_io.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
port_io.o: /usr/include/sys/_symbol_aliasing.h
port_io.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
port_io.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
port_io.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
port_io.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
port_io.o: /usr/include/secure/_common.h /usr/include/errno.h
port_io.o: /usr/include/sys/errno.h /usr/include/stdlib.h
port_io.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
port_io.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
port_io.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
port_io.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
port_io.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
port_io.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
port_io.o: /usr/include/libkern/_OSByteOrder.h
port_io.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
port_io.o: /usr/include/machine/types.h /usr/include/i386/types.h
port_io.o: /usr/include/unistd.h /usr/include/sys/unistd.h
port_io.o: /usr/include/sys/select.h /usr/include/sys/_select.h
port_io.o: /usr/include/sys/types.h /usr/include/sys/socket.h
port_io.o: /usr/include/machine/_param.h /usr/include/i386/_param.h
port_io.o: /usr/include/netinet/in.h /usr/include/stdint.h
port_io.o: /usr/include/netinet6/in6.h /usr/include/netdb.h port_io.h
fftfiltr.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
fftfiltr.o: /usr/include/sys/_symbol_aliasing.h
fftfiltr.o: /usr/include/sys/_posix_availability.h
fftfiltr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
fftfiltr.o: /usr/include/_types.h /usr/include/sys/_types.h
fftfiltr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
fftfiltr.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
fftfiltr.o: /usr/include/string.h /usr/include/strings.h
fftfiltr.o: /usr/include/secure/_string.h /usr/include/math.h
fftfiltr.o: /usr/include/architecture/i386/math.h basecnst.h sysbase.h
fftfiltr.o: fousrdef.h shconst.h shvars.h erusrdef.h ffusrdef.h fctxmt.h
fftfiltr.o: flerrors.h numres.h globalparams.h
qfileio.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
qfileio.o: /usr/include/sys/_symbol_aliasing.h
qfileio.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
qfileio.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
qfileio.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
qfileio.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
qfileio.o: /usr/include/secure/_common.h /usr/include/string.h
qfileio.o: /usr/include/strings.h /usr/include/secure/_string.h basecnst.h
qfileio.o: sysbase.h fousrdef.h /usr/include/stdlib.h /usr/include/sys/wait.h
qfileio.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
qfileio.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
qfileio.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
qfileio.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
qfileio.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
qfileio.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
qfileio.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
qfileio.o: /usr/include/machine/types.h /usr/include/i386/types.h
qfileio.o: /usr/include/unistd.h /usr/include/sys/unistd.h
qfileio.o: /usr/include/sys/select.h /usr/include/sys/_select.h qferrors.h
qfileio.o: qfusrdef.h
shdspmgr.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
shdspmgr.o: /usr/include/sys/_symbol_aliasing.h
shdspmgr.o: /usr/include/sys/_posix_availability.h
shdspmgr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
shdspmgr.o: /usr/include/_types.h /usr/include/sys/_types.h
shdspmgr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
shdspmgr.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
shdspmgr.o: /usr/include/string.h /usr/include/strings.h
shdspmgr.o: /usr/include/secure/_string.h basecnst.h /usr/include/stdlib.h
shdspmgr.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
shdspmgr.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
shdspmgr.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
shdspmgr.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
shdspmgr.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
shdspmgr.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
shdspmgr.o: /usr/include/libkern/_OSByteOrder.h
shdspmgr.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
shdspmgr.o: /usr/include/machine/types.h /usr/include/i386/types.h
shdspmgr.o: /usr/include/math.h /usr/include/architecture/i386/math.h
shdspmgr.o: sysbase.h fousrdef.h shconst.h shvars.h
shdspmgr.o: /usr/local/sh/sh/source/newgraph/gcusrdef.h cpusrdef.h trusrdef.h
shdspmgr.o: tcusrdef.h infoidx.h fctxdm.h fctxsl.h sherrors.h
shmenu3.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
shmenu3.o: /usr/include/sys/_symbol_aliasing.h
shmenu3.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
shmenu3.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
shmenu3.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
shmenu3.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
shmenu3.o: /usr/include/secure/_common.h /usr/include/string.h
shmenu3.o: /usr/include/strings.h /usr/include/secure/_string.h
shmenu3.o: /usr/include/math.h /usr/include/architecture/i386/math.h
shmenu3.o: basecnst.h /usr/include/stdlib.h /usr/include/sys/wait.h
shmenu3.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
shmenu3.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
shmenu3.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
shmenu3.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
shmenu3.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
shmenu3.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
shmenu3.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
shmenu3.o: /usr/include/machine/types.h /usr/include/i386/types.h sysbase.h
shmenu3.o: fousrdef.h shconst.h shvars.h cpusrdef.h ttusrdef.h ptusrdef.h
shmenu3.o: itusrdef.h utusrdef.h erusrdef.h glusrdef.h ffusrdef.h
shmenu3.o: /usr/local/sh/sh/source/newgraph/gcusrdef.h infoidx.h fctxml.h
shmenu3.o: fctxmt.h fctxsl.h fctxdm.h fctxmn3.h sherrors.h earthloc.h
shtransl.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
shtransl.o: /usr/include/sys/_symbol_aliasing.h
shtransl.o: /usr/include/sys/_posix_availability.h
shtransl.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
shtransl.o: /usr/include/_types.h /usr/include/sys/_types.h
shtransl.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
shtransl.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
shtransl.o: /usr/include/string.h /usr/include/strings.h
shtransl.o: /usr/include/secure/_string.h /usr/include/ctype.h
shtransl.o: /usr/include/runetype.h basecnst.h sysbase.h fousrdef.h shconst.h
shtransl.o: shvars.h cpusrdef.h erusrdef.h trusrdef.h ssusrdef.h tcusrdef.h
shtransl.o: infoidx.h fctxsl.h fctxml.h fctxdm.h trerrors.h utusrdef.h
shtransl.o: sserrors.h sherrors.h globalparams.h
sqliface.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
sqliface.o: /usr/include/sys/_symbol_aliasing.h
sqliface.o: /usr/include/sys/_posix_availability.h
sqliface.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
sqliface.o: /usr/include/_types.h /usr/include/sys/_types.h
sqliface.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
sqliface.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
sqliface.o: /usr/include/string.h /usr/include/strings.h
sqliface.o: /usr/include/secure/_string.h basecnst.h /usr/include/stdlib.h
sqliface.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
sqliface.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
sqliface.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
sqliface.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
sqliface.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
sqliface.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
sqliface.o: /usr/include/libkern/_OSByteOrder.h
sqliface.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
sqliface.o: /usr/include/machine/types.h /usr/include/i386/types.h sysbase.h
sqliface.o: fousrdef.h shconst.h tcusrdef.h seed_io/seedcfg.h sqliface.h
sqliface.o: globalparams.h
fileopen.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
fileopen.o: /usr/include/sys/_symbol_aliasing.h
fileopen.o: /usr/include/sys/_posix_availability.h
fileopen.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
fileopen.o: /usr/include/_types.h /usr/include/sys/_types.h
fileopen.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
fileopen.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
fileopen.o: /usr/include/string.h /usr/include/strings.h
fileopen.o: /usr/include/secure/_string.h basecnst.h sysbase.h fousrdef.h
fileopen.o: /usr/include/stdlib.h /usr/include/sys/wait.h
fileopen.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
fileopen.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
fileopen.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
fileopen.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
fileopen.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
fileopen.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
fileopen.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
fileopen.o: /usr/include/machine/types.h /usr/include/i386/types.h foerrors.h
qfintrfc.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
qfintrfc.o: /usr/include/sys/_symbol_aliasing.h
qfintrfc.o: /usr/include/sys/_posix_availability.h
qfintrfc.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
qfintrfc.o: /usr/include/_types.h /usr/include/sys/_types.h
qfintrfc.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
qfintrfc.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
qfintrfc.o: basecnst.h sysbase.h fousrdef.h shconst.h qfusrdef.h qiusrdef.h
qfintrfc.o: tcusrdef.h infoidx.h qierrors.h
shexec.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
shexec.o: /usr/include/sys/_symbol_aliasing.h
shexec.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
shexec.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
shexec.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
shexec.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
shexec.o: /usr/include/secure/_common.h /usr/include/string.h
shexec.o: /usr/include/strings.h /usr/include/secure/_string.h basecnst.h
shexec.o: sysbase.h fousrdef.h /usr/include/stdlib.h /usr/include/sys/wait.h
shexec.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
shexec.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
shexec.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
shexec.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
shexec.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
shexec.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
shexec.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
shexec.o: /usr/include/machine/types.h /usr/include/i386/types.h
shexec.o: /usr/include/unistd.h /usr/include/sys/unistd.h
shexec.o: /usr/include/sys/select.h /usr/include/sys/_select.h seusrdef.h
shexec.o: sysbase.h /usr/local/sh/sh/source/newgraph/gcusrdef.h shconst.h
shexec.o: cpusrdef.h fctxmn0.h fctxmn1.h fctxmn2.h fctxmn3.h fctxmn4.h
shexec.o: fctxmn5.h fctxmni.h fctxmnx.h fctxdm.h fctxmt.h shvars.h erusrdef.h
shexec.o: ssusrdef.h uiusrdef.h causrdef.h qiusrdef.h qfusrdef.h earthloc.h
shexec.o: sherrors.h residual.h
shmenu4.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
shmenu4.o: /usr/include/sys/_symbol_aliasing.h
shmenu4.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
shmenu4.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
shmenu4.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
shmenu4.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
shmenu4.o: /usr/include/secure/_common.h /usr/include/string.h
shmenu4.o: /usr/include/strings.h /usr/include/secure/_string.h
shmenu4.o: /usr/include/math.h /usr/include/architecture/i386/math.h
shmenu4.o: basecnst.h /usr/include/stdlib.h /usr/include/sys/wait.h
shmenu4.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
shmenu4.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
shmenu4.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
shmenu4.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
shmenu4.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
shmenu4.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
shmenu4.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
shmenu4.o: /usr/include/machine/types.h /usr/include/i386/types.h sysbase.h
shmenu4.o: shconst.h shvars.h cpusrdef.h mxusrdef.h pfusrdef.h erusrdef.h
shmenu4.o: ssusrdef.h uiusrdef.h /usr/local/sh/sh/source/newgraph/gcusrdef.h
shmenu4.o: infoidx.h fctxml.h fctxmt.h fctxsl.h fctxdm.h fctxmn4.h numres.h
shmenu4.o: sherrors.h globalparams.h
statloc.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
statloc.o: /usr/include/sys/_symbol_aliasing.h
statloc.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
statloc.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
statloc.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
statloc.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
statloc.o: /usr/include/secure/_common.h /usr/include/string.h
statloc.o: /usr/include/strings.h /usr/include/secure/_string.h basecnst.h
statloc.o: /usr/include/stdlib.h /usr/include/sys/wait.h
statloc.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
statloc.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
statloc.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
statloc.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
statloc.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
statloc.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
statloc.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
statloc.o: /usr/include/machine/types.h /usr/include/i386/types.h sysbase.h
statloc.o: erusrdef.h glusrdef.h globalparams.h
globalparams.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
globalparams.o: /usr/include/sys/_symbol_aliasing.h
globalparams.o: /usr/include/sys/_posix_availability.h
globalparams.o: /usr/include/Availability.h
globalparams.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
globalparams.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
globalparams.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
globalparams.o: /usr/include/secure/_common.h /usr/include/string.h
globalparams.o: /usr/include/strings.h /usr/include/secure/_string.h
globalparams.o: basecnst.h /usr/include/stdlib.h /usr/include/sys/wait.h
globalparams.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
globalparams.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
globalparams.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
globalparams.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
globalparams.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
globalparams.o: /usr/include/sys/_endian.h
globalparams.o: /usr/include/libkern/_OSByteOrder.h
globalparams.o: /usr/include/libkern/i386/_OSByteOrder.h
globalparams.o: /usr/include/alloca.h /usr/include/machine/types.h
globalparams.o: /usr/include/i386/types.h utusrdef.h globalparams.h
incitheo.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
incitheo.o: /usr/include/sys/_symbol_aliasing.h
incitheo.o: /usr/include/sys/_posix_availability.h
incitheo.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
incitheo.o: /usr/include/_types.h /usr/include/sys/_types.h
incitheo.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
incitheo.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
incitheo.o: /usr/include/string.h /usr/include/strings.h
incitheo.o: /usr/include/secure/_string.h basecnst.h sysbase.h erusrdef.h
incitheo.o: itusrdef.h
shhelp.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
shhelp.o: /usr/include/sys/_symbol_aliasing.h
shhelp.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
shhelp.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
shhelp.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
shhelp.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
shhelp.o: /usr/include/secure/_common.h /usr/include/string.h
shhelp.o: /usr/include/strings.h /usr/include/secure/_string.h basecnst.h
shhelp.o: ascii.h sysbase.h shconst.h
shhelp.o: /usr/local/sh/sh/source/newgraph/gcusrdef.h fctxhl.h fctxsl.h
shhelp.o: utusrdef.h sherrors.h
shmenu5.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
shmenu5.o: /usr/include/sys/_symbol_aliasing.h
shmenu5.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
shmenu5.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
shmenu5.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
shmenu5.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
shmenu5.o: /usr/include/secure/_common.h /usr/include/string.h
shmenu5.o: /usr/include/strings.h /usr/include/secure/_string.h
shmenu5.o: /usr/include/math.h /usr/include/architecture/i386/math.h
shmenu5.o: /usr/include/ctype.h /usr/include/runetype.h basecnst.h sysbase.h
shmenu5.o: /usr/include/stdlib.h /usr/include/sys/wait.h
shmenu5.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
shmenu5.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
shmenu5.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
shmenu5.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
shmenu5.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
shmenu5.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
shmenu5.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
shmenu5.o: /usr/include/machine/types.h /usr/include/i386/types.h shconst.h
shmenu5.o: shvars.h cpusrdef.h utusrdef.h ffusrdef.h erusrdef.h
shmenu5.o: /usr/local/sh/sh/source/newgraph/gcusrdef.h infoidx.h fctxml.h
shmenu5.o: fctxmt.h fctxsl.h fctxdm.h fctxmn5.h numres.h lgusrdef.h
shmenu5.o: sherrors.h
syscall.o: basecnst.h /usr/include/stdio.h /usr/include/sys/cdefs.h
syscall.o: /usr/include/sys/_symbol_aliasing.h
syscall.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
syscall.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
syscall.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
syscall.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
syscall.o: /usr/include/secure/_common.h /usr/include/string.h
syscall.o: /usr/include/strings.h /usr/include/secure/_string.h
syscall.o: /usr/include/stdlib.h /usr/include/sys/wait.h
syscall.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
syscall.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
syscall.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
syscall.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
syscall.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
syscall.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
syscall.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
syscall.o: /usr/include/machine/types.h /usr/include/i386/types.h
syscall.o: /usr/include/sys/types.h /usr/include/sys/times.h sysbase.h
syscall.o: syerrors.h
callsh.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
callsh.o: /usr/include/sys/_symbol_aliasing.h
callsh.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
callsh.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
callsh.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
callsh.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
callsh.o: /usr/include/secure/_common.h /usr/include/string.h
callsh.o: /usr/include/strings.h /usr/include/secure/_string.h basecnst.h
callsh.o: /usr/include/stdlib.h /usr/include/sys/wait.h
callsh.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
callsh.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
callsh.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
callsh.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
callsh.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
callsh.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
callsh.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
callsh.o: /usr/include/machine/types.h /usr/include/i386/types.h sysbase.h
callsh.o: shvars.h seusrdef.h /usr/local/sh/sh/source/newgraph/gcusrdef.h
callsh.o: shconst.h cpusrdef.h sherrors.h uierrors.h uiusrdef.h utusrdef.h
callsh.o: trusrdef.h erusrdef.h fctxmn0.h fctxdm.h
seismhan.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
seismhan.o: /usr/include/sys/_symbol_aliasing.h
seismhan.o: /usr/include/sys/_posix_availability.h
seismhan.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
seismhan.o: /usr/include/_types.h /usr/include/sys/_types.h
seismhan.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
seismhan.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
seismhan.o: /usr/include/string.h /usr/include/strings.h
seismhan.o: /usr/include/secure/_string.h basecnst.h /usr/include/stdlib.h
seismhan.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
seismhan.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
seismhan.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
seismhan.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
seismhan.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
seismhan.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
seismhan.o: /usr/include/libkern/_OSByteOrder.h
seismhan.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
seismhan.o: /usr/include/machine/types.h /usr/include/i386/types.h sysbase.h
seismhan.o: seusrdef.h /usr/local/sh/sh/source/newgraph/gcusrdef.h shconst.h
seismhan.o: cpusrdef.h sherrors.h uierrors.h uiusrdef.h trusrdef.h erusrdef.h
seismhan.o: fctxmn0.h fctxdm.h seed_io/seedcfg.h seed_io/seed_lib.h
seismhan.o: globalparams.h sqliface.h causrdef.h shdirs.h
