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

gcf2ascii.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
gcf2ascii.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
gcf2ascii.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
gcf2ascii.o: /usr/include/stdio_impl.h /usr/include/string.h
gcf2ascii.o: /usr/include/iso/string_iso.h ../basecnst.h ../sysbase.h
gcf2ascii.o: ../fousrdef.h ../tcusrdef.h gcflib.h
gcf2mseed.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
gcf2mseed.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
gcf2mseed.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
gcf2mseed.o: /usr/include/stdio_impl.h /usr/include/string.h
gcf2mseed.o: /usr/include/iso/string_iso.h ../basecnst.h
gcf2mseed.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h ../sysbase.h
gcf2mseed.o: ../fousrdef.h ../tcusrdef.h gcflib.h ../seed_io/seedcfg.h
gcf2mseed.o: ../seed_io/seed_lib.h
gcflib.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
gcflib.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
gcflib.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
gcflib.o: /usr/include/stdio_impl.h /usr/include/string.h
gcflib.o: /usr/include/iso/string_iso.h ../basecnst.h /usr/include/stdlib.h
gcflib.o: /usr/include/iso/stdlib_iso.h ../sysbase.h ../fousrdef.h
gcflib.o: ../tcusrdef.h gcflib.h ../seed_io/seedcfg.h ../seed_io/seed_lib.h
gcfquickdump.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
gcfquickdump.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
gcfquickdump.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
gcfquickdump.o: /usr/include/stdio_impl.h /usr/include/string.h
gcfquickdump.o: /usr/include/iso/string_iso.h ../basecnst.h ../sysbase.h
gcfquickdump.o: ../fousrdef.h ../tcusrdef.h gcflib.h
gcftest.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
gcftest.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
gcftest.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
gcftest.o: /usr/include/stdio_impl.h /usr/include/string.h
gcftest.o: /usr/include/iso/string_iso.h ../basecnst.h ../sysbase.h
gcftest.o: ../fousrdef.h ../tcusrdef.h gcflib.h
gfdline.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
gfdline.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
gfdline.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
gfdline.o: /usr/include/stdio_impl.h /usr/include/string.h
gfdline.o: /usr/include/iso/string_iso.h ../basecnst.h ../sysbase.h
gfdline.o: ../fousrdef.h ../cpar.h ../tcusrdef.h ../utusrdef.h ../erusrdef.h
gfdline.o: gcflib.h
split_gcf.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
split_gcf.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
split_gcf.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
split_gcf.o: /usr/include/stdio_impl.h /usr/include/string.h
split_gcf.o: /usr/include/iso/string_iso.h ../basecnst.h ../sysbase.h
split_gcf.o: ../fousrdef.h ../cpar.h ../erusrdef.h ../tcusrdef.h gcflib.h
