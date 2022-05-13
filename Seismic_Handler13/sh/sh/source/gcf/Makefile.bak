# file Makefile
#      ========
#
# version 2, 21-Oct-2003
#
# Makefile of GCF/IO directory
# K. Stammler, 18-Oct-2003


CFLAGS      =	$(SH_CCOPTIONS) -DBASECNST=\"basecnst.h\" -I.. -I$(SH_UTIL)
CC          =	$(SH_COMPILER)
LD          =	$(SH_COMPILER)
LINK.c      =	$(SH_COMPILER)
RANLIB		=	$(SH_RANLIB)
LDFLAGS     =	-L$(SH_LIB) $(SH_LINKOPTIONS)
LIBS        =	-lgcf -lseed -lsource -lm $(SH_LINKLIBS)
OBJLIB		= ..
ALL_LOCAL_SRCS	= *.c
LIB_GCF_SRCS	= gcflib.c
LIB_GCF_OBJS	= $(LIB_GCF_SRCS:.c=.o)

LIB_GCF		=	$(SH_LIB)libgcf.a
LIB_SEED	=	$(SH_LIB)libseed.a
LIB_SOURCE	=	$(SH_LIB)libsource.a

ALL         =	gcfquickdump gcf2ascii gcf2mseed gfdline split_gcf gcftest

all:
		make $(LIB_GCF) $(ALL)

$(LIB_GCF):	$(LIB_GCF_OBJS)
	ar r $@ $(LIB_GCF_OBJS); $(RANLIB) $@

$(LIB_SOURCE):
	-@echo "libsource.a not up to date"

gcfquickdump: gcfquickdump.o $(LIB_SOURCE) $(LIB_GCF)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

gcf2ascii: gcf2ascii.o $(LIB_SOURCE) $(LIB_GCF)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

gcf2mseed: gcf2mseed.o $(LIB_SOURCE) $(LIB_GCF)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

gfdline: gfdline.o $(LIB_SOURCE) $(LIB_GCF)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

split_gcf: split_gcf.o $(LIB_SOURCE) $(LIB_GCF)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

gcftest: gcftest.o $(LIB_SOURCE) $(LIB_GCF)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)


clean:
	@rm *.o *% $(ALL)

depend:
	makedepend -s "# DO NOT DELETE" -- $(CFLAGS) -- $(ALL_LOCAL_SRCS)


# DO NOT DELETE

gcf2ascii.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
gcf2ascii.o: /usr/include/sys/_symbol_aliasing.h
gcf2ascii.o: /usr/include/sys/_posix_availability.h
gcf2ascii.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
gcf2ascii.o: /usr/include/_types.h /usr/include/sys/_types.h
gcf2ascii.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
gcf2ascii.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
gcf2ascii.o: /usr/include/string.h /usr/include/strings.h
gcf2ascii.o: /usr/include/secure/_string.h ../basecnst.h ../sysbase.h
gcf2ascii.o: ../fousrdef.h ../tcusrdef.h gcflib.h
gcf2mseed.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
gcf2mseed.o: /usr/include/sys/_symbol_aliasing.h
gcf2mseed.o: /usr/include/sys/_posix_availability.h
gcf2mseed.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
gcf2mseed.o: /usr/include/_types.h /usr/include/sys/_types.h
gcf2mseed.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
gcf2mseed.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
gcf2mseed.o: /usr/include/string.h /usr/include/strings.h
gcf2mseed.o: /usr/include/secure/_string.h ../basecnst.h
gcf2mseed.o: /usr/include/stdlib.h /usr/include/sys/wait.h
gcf2mseed.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
gcf2mseed.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
gcf2mseed.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
gcf2mseed.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
gcf2mseed.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
gcf2mseed.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
gcf2mseed.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
gcf2mseed.o: /usr/include/machine/types.h /usr/include/i386/types.h
gcf2mseed.o: ../sysbase.h ../fousrdef.h ../tcusrdef.h gcflib.h
gcf2mseed.o: ../seed_io/seedcfg.h ../seed_io/seed_lib.h
gcflib.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
gcflib.o: /usr/include/sys/_symbol_aliasing.h
gcflib.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
gcflib.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
gcflib.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
gcflib.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
gcflib.o: /usr/include/secure/_common.h /usr/include/string.h
gcflib.o: /usr/include/strings.h /usr/include/secure/_string.h ../basecnst.h
gcflib.o: /usr/include/stdlib.h /usr/include/sys/wait.h
gcflib.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
gcflib.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
gcflib.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
gcflib.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
gcflib.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
gcflib.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
gcflib.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
gcflib.o: /usr/include/machine/types.h /usr/include/i386/types.h ../sysbase.h
gcflib.o: ../fousrdef.h ../tcusrdef.h gcflib.h ../seed_io/seedcfg.h
gcflib.o: ../seed_io/seed_lib.h
gcfquickdump.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
gcfquickdump.o: /usr/include/sys/_symbol_aliasing.h
gcfquickdump.o: /usr/include/sys/_posix_availability.h
gcfquickdump.o: /usr/include/Availability.h
gcfquickdump.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
gcfquickdump.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
gcfquickdump.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
gcfquickdump.o: /usr/include/secure/_common.h /usr/include/string.h
gcfquickdump.o: /usr/include/strings.h /usr/include/secure/_string.h
gcfquickdump.o: ../basecnst.h ../sysbase.h ../fousrdef.h ../tcusrdef.h
gcfquickdump.o: gcflib.h
gcftest.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
gcftest.o: /usr/include/sys/_symbol_aliasing.h
gcftest.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
gcftest.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
gcftest.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
gcftest.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
gcftest.o: /usr/include/secure/_common.h /usr/include/string.h
gcftest.o: /usr/include/strings.h /usr/include/secure/_string.h ../basecnst.h
gcftest.o: ../sysbase.h ../fousrdef.h ../tcusrdef.h gcflib.h
gfdline.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
gfdline.o: /usr/include/sys/_symbol_aliasing.h
gfdline.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
gfdline.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
gfdline.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
gfdline.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
gfdline.o: /usr/include/secure/_common.h /usr/include/string.h
gfdline.o: /usr/include/strings.h /usr/include/secure/_string.h ../basecnst.h
gfdline.o: ../sysbase.h ../fousrdef.h ../cpar.h ../tcusrdef.h ../utusrdef.h
gfdline.o: ../erusrdef.h gcflib.h
split_gcf.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
split_gcf.o: /usr/include/sys/_symbol_aliasing.h
split_gcf.o: /usr/include/sys/_posix_availability.h
split_gcf.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
split_gcf.o: /usr/include/_types.h /usr/include/sys/_types.h
split_gcf.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
split_gcf.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
split_gcf.o: /usr/include/string.h /usr/include/strings.h
split_gcf.o: /usr/include/secure/_string.h ../basecnst.h ../sysbase.h
split_gcf.o: ../fousrdef.h ../cpar.h ../erusrdef.h ../tcusrdef.h gcflib.h
