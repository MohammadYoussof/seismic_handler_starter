
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

cmdargs.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
cmdargs.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
cmdargs.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
cmdargs.o: /usr/include/stdio_impl.h /usr/include/string.h
cmdargs.o: /usr/include/iso/string_iso.h /usr/include/ctype.h
cmdargs.o: /usr/include/iso/ctype_iso.h basecnst.h sysbase.h fousrdef.h
logplot.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
logplot.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
logplot.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
logplot.o: /usr/include/stdio_impl.h /usr/include/string.h
logplot.o: /usr/include/iso/string_iso.h /usr/include/math.h
logplot.o: /usr/include/iso/math_iso.h basecnst.h sysbase.h fousrdef.h
logplot.o: /programs/sol2/sh/source/newgraph/gcusrdef.h
logplot.o: /programs/sol2/sh/source/newgraph/graphbas.h shconst.h lgusrdef.h
recfiltr.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
recfiltr.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
recfiltr.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
recfiltr.o: /usr/include/stdio_impl.h /usr/include/string.h
recfiltr.o: /usr/include/iso/string_iso.h basecnst.h /usr/include/stdlib.h
recfiltr.o: /usr/include/iso/stdlib_iso.h sysbase.h fousrdef.h shconst.h
recfiltr.o: shvars.h erusrdef.h numres.h rfusrdef.h flerrors.h globalparams.h
shlib.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shlib.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shlib.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shlib.o: /usr/include/stdio_impl.h /usr/include/string.h
shlib.o: /usr/include/iso/string_iso.h /usr/include/ctype.h
shlib.o: /usr/include/iso/ctype_iso.h /usr/include/math.h
shlib.o: /usr/include/iso/math_iso.h basecnst.h sysbase.h fousrdef.h
shlib.o: shconst.h utusrdef.h cpusrdef.h ssusrdef.h erusrdef.h trusrdef.h
shlib.o: tcusrdef.h /programs/sol2/sh/source/newgraph/gcusrdef.h
shlib.o: /programs/sol2/sh/source/newgraph/graphbas.h fctxsl.h sserrors.h
shlib.o: sherrors.h globalparams.h seed_io/seedcfg.h sqliface.h
shmenuio.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shmenuio.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shmenuio.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shmenuio.o: /usr/include/stdio_impl.h /usr/include/string.h
shmenuio.o: /usr/include/iso/string_iso.h /usr/include/ctype.h
shmenuio.o: /usr/include/iso/ctype_iso.h basecnst.h /usr/include/stdlib.h
shmenuio.o: /usr/include/iso/stdlib_iso.h sysbase.h fousrdef.h
shmenuio.o: /programs/sol2/sh/source/newgraph/gcusrdef.h
shmenuio.o: /programs/sol2/sh/source/newgraph/graphbas.h shconst.h shvars.h
shmenuio.o: cpusrdef.h qfusrdef.h qiusrdef.h utusrdef.h erusrdef.h tcusrdef.h
shmenuio.o: glusrdef.h infoidx.h fctxml.h fctxsl.h
shmenuio.o: /programs/sol2/sh/source/foreign/shfrgn.h sherrors.h
shmenuio.o: seed_io/seedcfg.h seed_io/seed_lib.h seed_io/seed_cd.h
shmenuio.o: gcf/gcflib.h sqliface.h globalparams.h
timeconv.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
timeconv.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
timeconv.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
timeconv.o: /usr/include/stdio_impl.h /usr/include/ctype.h
timeconv.o: /usr/include/iso/ctype_iso.h /usr/include/string.h
timeconv.o: /usr/include/iso/string_iso.h basecnst.h sysbase.h fousrdef.h
timeconv.o: tcusrdef.h utusrdef.h tcerrors.h
cmdparse.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
cmdparse.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
cmdparse.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
cmdparse.o: /usr/include/stdio_impl.h /usr/include/string.h
cmdparse.o: /usr/include/iso/string_iso.h basecnst.h sysbase.h fousrdef.h
cmdparse.o: erusrdef.h cpusrdef.h cperrors.h uiusrdef.h
cmdparse.o: /programs/sol2/sh/source/newgraph/gcusrdef.h
cmdparse.o: /programs/sol2/sh/source/newgraph/graphbas.h
cmdparse.o: /programs/sol2/sh/source/newgraph/gcusrdef.h
matrixhd.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
matrixhd.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
matrixhd.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
matrixhd.o: /usr/include/stdio_impl.h /usr/include/math.h
matrixhd.o: /usr/include/iso/math_iso.h basecnst.h sysbase.h fousrdef.h
matrixhd.o: shconst.h mxusrdef.h
shmath.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shmath.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shmath.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shmath.o: /usr/include/stdio_impl.h /usr/include/string.h
shmath.o: /usr/include/iso/string_iso.h /usr/include/math.h
shmath.o: /usr/include/iso/math_iso.h basecnst.h /usr/include/stdlib.h
shmath.o: /usr/include/iso/stdlib_iso.h sysbase.h fousrdef.h tcusrdef.h
shmath.o: shconst.h shvars.h glusrdef.h fctxmt.h numres.h sherrors.h
shmenux.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shmenux.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shmenux.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shmenux.o: /usr/include/stdio_impl.h /usr/include/string.h
shmenux.o: /usr/include/iso/string_iso.h /usr/include/math.h
shmenux.o: /usr/include/iso/math_iso.h basecnst.h sysbase.h fousrdef.h
shmenux.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h shconst.h
shmenux.o: shvars.h cpusrdef.h glusrdef.h uiusrdef.h
shmenux.o: /programs/sol2/sh/source/newgraph/gcusrdef.h ssusrdef.h ttusrdef.h
shmenux.o: tcusrdef.h infoidx.h fctxsl.h fctxdm.h fctxml.h fctxcr.h fctxmt.h
shmenux.o: fctxpm.h residual.h sherrors.h
travtime.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
travtime.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
travtime.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
travtime.o: /usr/include/stdio_impl.h /usr/include/string.h
travtime.o: /usr/include/iso/string_iso.h /usr/include/math.h
travtime.o: /usr/include/iso/math_iso.h basecnst.h sysbase.h fousrdef.h
travtime.o: numres.h erusrdef.h ttusrdef.h tterrors.h
residual.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
residual.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
residual.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
residual.o: /usr/include/stdio_impl.h /usr/include/string.h
residual.o: /usr/include/iso/string_iso.h basecnst.h /usr/include/stdlib.h
residual.o: /usr/include/iso/stdlib_iso.h sysbase.h fousrdef.h erusrdef.h
residual.o: utusrdef.h residual.h
cpar.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
cpar.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
cpar.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
cpar.o: /usr/include/stdio_impl.h /usr/include/string.h
cpar.o: /usr/include/iso/string_iso.h basecnst.h sysbase.h fousrdef.h cpar.h
numres.o: /usr/include/math.h /usr/include/iso/math_iso.h
numres.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
numres.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
numres.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
numres.o: /usr/include/stdio_impl.h basecnst.h sysbase.h fousrdef.h
numres.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h numres.h
shcorr.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shcorr.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shcorr.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shcorr.o: /usr/include/stdio_impl.h /usr/include/math.h
shcorr.o: /usr/include/iso/math_iso.h basecnst.h sysbase.h fousrdef.h
shcorr.o: shconst.h mxusrdef.h scusrdef.h scerrors.h
shmenu0.o: /usr/include/string.h /usr/include/iso/string_iso.h
shmenu0.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shmenu0.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shmenu0.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shmenu0.o: /usr/include/stdio_impl.h /usr/include/math.h
shmenu0.o: /usr/include/iso/math_iso.h basecnst.h sysbase.h fousrdef.h
shmenu0.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
shmenu0.o: /programs/sol2/sh/source/newgraph/gcusrdef.h shconst.h shvars.h
shmenu0.o: cpusrdef.h uiusrdef.h utusrdef.h scusrdef.h ssusrdef.h glusrdef.h
shmenu0.o: qfusrdef.h ttusrdef.h ptusrdef.h itusrdef.h erusrdef.h tcusrdef.h
shmenu0.o: infoidx.h fctxmn0.h fctxmni.h fctxhl.h fctxsl.h fctxdm.h
shmenu0.o: sherrors.h sserrors.h earthloc.h globalparams.h seed_io/seedcfg.h
shmenu0.o: seed_io/seed_lib.h
shmidlev.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shmidlev.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shmidlev.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shmidlev.o: /usr/include/stdio_impl.h /usr/include/string.h
shmidlev.o: /usr/include/iso/string_iso.h /usr/include/ctype.h
shmidlev.o: /usr/include/iso/ctype_iso.h basecnst.h /usr/include/stdlib.h
shmidlev.o: /usr/include/iso/stdlib_iso.h sysbase.h fousrdef.h shconst.h
shmidlev.o: shvars.h cpusrdef.h utusrdef.h tcusrdef.h infoidx.h fctxsl.h
shmidlev.o: fctxcr.h fctxml.h fctxdm.h sherrors.h
usrinpw.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
usrinpw.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
usrinpw.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
usrinpw.o: /usr/include/stdio_impl.h /usr/include/string.h
usrinpw.o: /usr/include/iso/string_iso.h /usr/include/ctype.h
usrinpw.o: /usr/include/iso/ctype_iso.h basecnst.h sysbase.h fousrdef.h
usrinpw.o: /programs/sol2/sh/source/newgraph/gcusrdef.h erusrdef.h utusrdef.h
usrinpw.o: uiusrdef.h uierrors.h globalparams.h
earthloc.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
earthloc.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
earthloc.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
earthloc.o: /usr/include/stdio_impl.h /usr/include/string.h
earthloc.o: /usr/include/iso/string_iso.h /usr/include/math.h
earthloc.o: /usr/include/iso/math_iso.h basecnst.h sysbase.h fousrdef.h
earthloc.o: shconst.h earthloc.h
polfiltr.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
polfiltr.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
polfiltr.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
polfiltr.o: /usr/include/stdio_impl.h /usr/include/string.h
polfiltr.o: /usr/include/iso/string_iso.h /usr/include/math.h
polfiltr.o: /usr/include/iso/math_iso.h basecnst.h sysbase.h fousrdef.h
polfiltr.o: shconst.h mxusrdef.h
shcursor.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shcursor.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shcursor.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shcursor.o: /usr/include/stdio_impl.h basecnst.h sysbase.h fousrdef.h
shcursor.o: shconst.h shvars.h /programs/sol2/sh/source/newgraph/gcusrdef.h
shcursor.o: uiusrdef.h tcusrdef.h infoidx.h fctxcr.h fctxdm.h sherrors.h
shmenu1.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shmenu1.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shmenu1.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shmenu1.o: /usr/include/stdio_impl.h /usr/include/string.h
shmenu1.o: /usr/include/iso/string_iso.h /usr/include/math.h
shmenu1.o: /usr/include/iso/math_iso.h basecnst.h sysbase.h fousrdef.h
shmenu1.o: shconst.h shvars.h cpusrdef.h uiusrdef.h
shmenu1.o: /programs/sol2/sh/source/newgraph/gcusrdef.h qfusrdef.h qiusrdef.h
shmenu1.o: ssusrdef.h erusrdef.h tcusrdef.h infoidx.h fctxml.h fctxsl.h
shmenu1.o: fctxdm.h fctxmt.h fctxcr.h sherrors.h qferrors.h
shpm.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shpm.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shpm.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shpm.o: /usr/include/stdio_impl.h /usr/include/math.h
shpm.o: /usr/include/iso/math_iso.h basecnst.h sysbase.h fousrdef.h shconst.h
shpm.o: /programs/sol2/sh/source/newgraph/gcusrdef.h fctxsl.h fctxcr.h
shpm.o: fctxpm.h sherrors.h
utility.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
utility.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
utility.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
utility.o: /usr/include/stdio_impl.h /usr/include/ctype.h
utility.o: /usr/include/iso/ctype_iso.h /usr/include/string.h
utility.o: /usr/include/iso/string_iso.h basecnst.h sysbase.h fousrdef.h
utility.o: uterrors.h utusrdef.h
errmsg.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
errmsg.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
errmsg.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
errmsg.o: /usr/include/stdio_impl.h /usr/include/string.h
errmsg.o: /usr/include/iso/string_iso.h basecnst.h /usr/include/stdlib.h
errmsg.o: /usr/include/iso/stdlib_iso.h sysbase.h fousrdef.h erusrdef.h
errmsg.o: globalparams.h
ptravtim.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
ptravtim.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
ptravtim.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
ptravtim.o: /usr/include/stdio_impl.h /usr/include/string.h
ptravtim.o: /usr/include/iso/string_iso.h /usr/include/ctype.h
ptravtim.o: /usr/include/iso/ctype_iso.h basecnst.h sysbase.h fousrdef.h
ptravtim.o: erusrdef.h ptusrdef.h pterrors.h
shdataba.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shdataba.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shdataba.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shdataba.o: /usr/include/stdio_impl.h /usr/include/string.h
shdataba.o: /usr/include/iso/string_iso.h basecnst.h /usr/include/stdlib.h
shdataba.o: /usr/include/iso/stdlib_iso.h sysbase.h fousrdef.h shconst.h
shdataba.o: shvars.h qfusrdef.h qiusrdef.h tcusrdef.h
shdataba.o: /programs/sol2/sh/source/newgraph/gcusrdef.h infoidx.h sherrors.h
shmenu2.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shmenu2.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shmenu2.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shmenu2.o: /usr/include/stdio_impl.h /usr/include/string.h
shmenu2.o: /usr/include/iso/string_iso.h /usr/include/math.h
shmenu2.o: /usr/include/iso/math_iso.h basecnst.h /usr/include/stdlib.h
shmenu2.o: /usr/include/iso/stdlib_iso.h sysbase.h fousrdef.h shconst.h
shmenu2.o: shvars.h cpusrdef.h rfusrdef.h ffusrdef.h scusrdef.h ssusrdef.h
shmenu2.o: erusrdef.h qiusrdef.h qfusrdef.h uiusrdef.h
shmenu2.o: /programs/sol2/sh/source/newgraph/gcusrdef.h tcusrdef.h infoidx.h
shmenu2.o: fctxcr.h fctxdm.h fctxsl.h fctxml.h fctxmn2.h sherrors.h
shsymbol.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shsymbol.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shsymbol.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shsymbol.o: /usr/include/stdio_impl.h /usr/include/string.h
shsymbol.o: /usr/include/iso/string_iso.h /usr/include/ctype.h
shsymbol.o: /usr/include/iso/ctype_iso.h basecnst.h sysbase.h fousrdef.h
shsymbol.o: shconst.h shvars.h erusrdef.h cpusrdef.h sserrors.h ssusrdef.h
port_io.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
port_io.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
port_io.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
port_io.o: /usr/include/stdio_impl.h /usr/include/errno.h
port_io.o: /usr/include/sys/errno.h /usr/include/stdlib.h
port_io.o: /usr/include/iso/stdlib_iso.h /usr/include/unistd.h
port_io.o: /usr/include/sys/types.h /usr/include/sys/machtypes.h
port_io.o: /usr/include/sys/int_types.h /usr/include/sys/select.h
port_io.o: /usr/include/sys/time.h /usr/include/sys/time.h
port_io.o: /usr/include/sys/unistd.h /usr/include/sys/socket.h
port_io.o: /usr/include/sys/uio.h /usr/include/sys/netconfig.h
port_io.o: /usr/include/netinet/in.h /usr/include/sys/stream.h
port_io.o: /usr/include/sys/vnode.h /usr/include/sys/t_lock.h
port_io.o: /usr/include/sys/machlock.h /usr/include/sys/param.h
port_io.o: /usr/include/sys/mutex.h /usr/include/sys/rwlock.h
port_io.o: /usr/include/sys/semaphore.h /usr/include/sys/condvar.h
port_io.o: /usr/include/sys/time_impl.h /usr/include/sys/cred.h
port_io.o: /usr/include/sys/resource.h /usr/include/vm/seg_enum.h
port_io.o: /usr/include/sys/poll.h /usr/include/sys/strmdep.h
port_io.o: /usr/include/sys/model.h /usr/include/sys/strft.h
port_io.o: /usr/include/sys/byteorder.h /usr/include/sys/un.h
port_io.o: /usr/include/net/if_dl.h /usr/include/netdb.h port_io.h
fftfiltr.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
fftfiltr.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
fftfiltr.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
fftfiltr.o: /usr/include/stdio_impl.h /usr/include/string.h
fftfiltr.o: /usr/include/iso/string_iso.h /usr/include/math.h
fftfiltr.o: /usr/include/iso/math_iso.h basecnst.h sysbase.h fousrdef.h
fftfiltr.o: shconst.h shvars.h erusrdef.h ffusrdef.h fctxmt.h flerrors.h
fftfiltr.o: numres.h globalparams.h
qfileio.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
qfileio.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
qfileio.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
qfileio.o: /usr/include/stdio_impl.h /usr/include/string.h
qfileio.o: /usr/include/iso/string_iso.h basecnst.h sysbase.h fousrdef.h
qfileio.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
qfileio.o: /usr/include/unistd.h /usr/include/sys/types.h
qfileio.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
qfileio.o: /usr/include/sys/select.h /usr/include/sys/time.h
qfileio.o: /usr/include/sys/time.h /usr/include/sys/unistd.h qferrors.h
qfileio.o: qfusrdef.h
shdspmgr.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shdspmgr.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shdspmgr.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shdspmgr.o: /usr/include/stdio_impl.h /usr/include/string.h
shdspmgr.o: /usr/include/iso/string_iso.h basecnst.h /usr/include/stdlib.h
shdspmgr.o: /usr/include/iso/stdlib_iso.h /usr/include/math.h
shdspmgr.o: /usr/include/iso/math_iso.h sysbase.h fousrdef.h shconst.h
shdspmgr.o: shvars.h /programs/sol2/sh/source/newgraph/gcusrdef.h cpusrdef.h
shdspmgr.o: trusrdef.h tcusrdef.h infoidx.h fctxdm.h fctxsl.h sherrors.h
shmenu3.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shmenu3.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shmenu3.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shmenu3.o: /usr/include/stdio_impl.h /usr/include/string.h
shmenu3.o: /usr/include/iso/string_iso.h /usr/include/math.h
shmenu3.o: /usr/include/iso/math_iso.h basecnst.h /usr/include/stdlib.h
shmenu3.o: /usr/include/iso/stdlib_iso.h sysbase.h fousrdef.h shconst.h
shmenu3.o: shvars.h cpusrdef.h ttusrdef.h ptusrdef.h itusrdef.h utusrdef.h
shmenu3.o: erusrdef.h glusrdef.h ffusrdef.h
shmenu3.o: /programs/sol2/sh/source/newgraph/gcusrdef.h infoidx.h fctxml.h
shmenu3.o: fctxmt.h fctxsl.h fctxdm.h fctxmn3.h sherrors.h earthloc.h
shtransl.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shtransl.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shtransl.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shtransl.o: /usr/include/stdio_impl.h /usr/include/string.h
shtransl.o: /usr/include/iso/string_iso.h /usr/include/ctype.h
shtransl.o: /usr/include/iso/ctype_iso.h basecnst.h sysbase.h fousrdef.h
shtransl.o: shconst.h shvars.h cpusrdef.h erusrdef.h trusrdef.h ssusrdef.h
shtransl.o: tcusrdef.h infoidx.h fctxsl.h fctxml.h fctxdm.h trerrors.h
shtransl.o: utusrdef.h sserrors.h sherrors.h globalparams.h
sqliface.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
sqliface.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
sqliface.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
sqliface.o: /usr/include/stdio_impl.h /usr/include/string.h
sqliface.o: /usr/include/iso/string_iso.h basecnst.h /usr/include/stdlib.h
sqliface.o: /usr/include/iso/stdlib_iso.h sysbase.h fousrdef.h shconst.h
sqliface.o: tcusrdef.h seed_io/seedcfg.h sqliface.h globalparams.h
fileopen.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
fileopen.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
fileopen.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
fileopen.o: /usr/include/stdio_impl.h /usr/include/string.h
fileopen.o: /usr/include/iso/string_iso.h basecnst.h sysbase.h fousrdef.h
fileopen.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h foerrors.h
qfintrfc.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
qfintrfc.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
qfintrfc.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
qfintrfc.o: /usr/include/stdio_impl.h basecnst.h sysbase.h fousrdef.h
qfintrfc.o: shconst.h qfusrdef.h qiusrdef.h tcusrdef.h infoidx.h qierrors.h
shexec.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shexec.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shexec.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shexec.o: /usr/include/stdio_impl.h /usr/include/string.h
shexec.o: /usr/include/iso/string_iso.h basecnst.h sysbase.h fousrdef.h
shexec.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
shexec.o: /usr/include/unistd.h /usr/include/sys/types.h
shexec.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
shexec.o: /usr/include/sys/select.h /usr/include/sys/time.h
shexec.o: /usr/include/sys/time.h /usr/include/sys/unistd.h seusrdef.h
shexec.o: sysbase.h /programs/sol2/sh/source/newgraph/gcusrdef.h shconst.h
shexec.o: cpusrdef.h fctxmn0.h fctxmn1.h fctxmn2.h fctxmn3.h fctxmn4.h
shexec.o: fctxmn5.h fctxmni.h fctxmnx.h fctxdm.h fctxmt.h shvars.h erusrdef.h
shexec.o: ssusrdef.h uiusrdef.h causrdef.h qiusrdef.h qfusrdef.h earthloc.h
shexec.o: sherrors.h residual.h
shmenu4.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shmenu4.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shmenu4.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shmenu4.o: /usr/include/stdio_impl.h /usr/include/string.h
shmenu4.o: /usr/include/iso/string_iso.h /usr/include/math.h
shmenu4.o: /usr/include/iso/math_iso.h basecnst.h /usr/include/stdlib.h
shmenu4.o: /usr/include/iso/stdlib_iso.h sysbase.h shconst.h shvars.h
shmenu4.o: cpusrdef.h mxusrdef.h pfusrdef.h erusrdef.h ssusrdef.h uiusrdef.h
shmenu4.o: /programs/sol2/sh/source/newgraph/gcusrdef.h infoidx.h fctxml.h
shmenu4.o: fctxmt.h fctxsl.h fctxdm.h fctxmn4.h numres.h sherrors.h
shmenu4.o: globalparams.h
statloc.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
statloc.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
statloc.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
statloc.o: /usr/include/stdio_impl.h /usr/include/string.h
statloc.o: /usr/include/iso/string_iso.h basecnst.h /usr/include/stdlib.h
statloc.o: /usr/include/iso/stdlib_iso.h sysbase.h erusrdef.h glusrdef.h
statloc.o: globalparams.h
globalparams.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
globalparams.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
globalparams.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
globalparams.o: /usr/include/stdio_impl.h /usr/include/string.h
globalparams.o: /usr/include/iso/string_iso.h basecnst.h
globalparams.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
globalparams.o: utusrdef.h globalparams.h
incitheo.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
incitheo.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
incitheo.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
incitheo.o: /usr/include/stdio_impl.h /usr/include/string.h
incitheo.o: /usr/include/iso/string_iso.h basecnst.h sysbase.h erusrdef.h
incitheo.o: itusrdef.h
shhelp.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shhelp.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shhelp.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shhelp.o: /usr/include/stdio_impl.h /usr/include/string.h
shhelp.o: /usr/include/iso/string_iso.h basecnst.h ascii.h sysbase.h
shhelp.o: shconst.h /programs/sol2/sh/source/newgraph/gcusrdef.h fctxhl.h
shhelp.o: fctxsl.h utusrdef.h sherrors.h
shmenu5.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shmenu5.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shmenu5.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shmenu5.o: /usr/include/stdio_impl.h /usr/include/string.h
shmenu5.o: /usr/include/iso/string_iso.h /usr/include/math.h
shmenu5.o: /usr/include/iso/math_iso.h /usr/include/ctype.h
shmenu5.o: /usr/include/iso/ctype_iso.h basecnst.h sysbase.h
shmenu5.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h shconst.h
shmenu5.o: shvars.h cpusrdef.h utusrdef.h ffusrdef.h erusrdef.h
shmenu5.o: /programs/sol2/sh/source/newgraph/gcusrdef.h infoidx.h fctxml.h
shmenu5.o: fctxmt.h fctxsl.h fctxdm.h fctxmn5.h numres.h lgusrdef.h
shmenu5.o: sherrors.h
syscall.o: basecnst.h /usr/include/stdio.h /usr/include/iso/stdio_iso.h
syscall.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
syscall.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
syscall.o: /usr/include/stdio_impl.h /usr/include/string.h
syscall.o: /usr/include/iso/string_iso.h /usr/include/stdlib.h
syscall.o: /usr/include/iso/stdlib_iso.h /usr/include/sys/types.h
syscall.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
syscall.o: /usr/include/sys/select.h /usr/include/sys/time.h
syscall.o: /usr/include/sys/time.h /usr/include/sys/times.h sysbase.h
syscall.o: syerrors.h
callsh.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
callsh.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
callsh.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
callsh.o: /usr/include/stdio_impl.h /usr/include/string.h
callsh.o: /usr/include/iso/string_iso.h basecnst.h /usr/include/stdlib.h
callsh.o: /usr/include/iso/stdlib_iso.h sysbase.h shvars.h seusrdef.h
callsh.o: /programs/sol2/sh/source/newgraph/gcusrdef.h shconst.h cpusrdef.h
callsh.o: sherrors.h uierrors.h uiusrdef.h utusrdef.h trusrdef.h erusrdef.h
callsh.o: fctxmn0.h fctxdm.h
seismhan.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
seismhan.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
seismhan.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
seismhan.o: /usr/include/stdio_impl.h /usr/include/string.h
seismhan.o: /usr/include/iso/string_iso.h basecnst.h /usr/include/stdlib.h
seismhan.o: /usr/include/iso/stdlib_iso.h sysbase.h seusrdef.h
seismhan.o: /programs/sol2/sh/source/newgraph/gcusrdef.h shconst.h cpusrdef.h
seismhan.o: sherrors.h uierrors.h uiusrdef.h trusrdef.h erusrdef.h fctxmn0.h
seismhan.o: fctxdm.h seed_io/seedcfg.h seed_io/seed_lib.h globalparams.h
seismhan.o: sqliface.h causrdef.h shdirs.h
