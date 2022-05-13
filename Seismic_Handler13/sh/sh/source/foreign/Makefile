# file Makefile
#      ========
#
# version 8, 16-Feb-2000
#
# Makefile of directory $SH_SOURCE/foreign
# K. Stammler, 7-Mar-95

CFLAGS      = $(SH_CCOPTIONS) -I.. -DBASECNST=\"basecnst.h\" -Dsparc=1
CC          = $(SH_COMPILER)
LD          = $(SH_COMPILER)
RANLIB		= $(SH_RANLIB)
LDFLAGS     = -L/usr/local/X11/lib
LIBS        = -lm -lX

LIB_FRGN    = ${SH_LIB}libfrgn.a
LIB_FRGN_SRCS	= f2c.c	gsedata.c lineparse.c sh_ah.c sh_gse.c sh_gse2.c \
	sh_len.c sh_neic.c shfrgn.c

#LIB_FRGN_OBJS	= $(LIB_FRGN_SRCS:%.c=%.o)
LIB_FRGN_OBJS	= $(LIB_FRGN_SRCS:.c=.o)


all:
	@echo $(LIB_FRGN_OBJS); make $(LIB_FRGN)


$(LIB_FRGN): $(LIB_FRGN_OBJS)
	ar r $@ $(LIB_FRGN_OBJS); $(RANLIB) $@


clean:
	@rm $(LIB_FRGN_OBJS) *%


depend:
	makedepend -s "# DO NOT DELETE" -- $(CFLAGS) -- $(LIB_FRGN_SRCS)



# DO NOT DELETE

f2c.o: f2c.h
gsedata.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
gsedata.o: /usr/include/sys/_symbol_aliasing.h
gsedata.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
gsedata.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
gsedata.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
gsedata.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
gsedata.o: /usr/include/secure/_common.h /usr/include/string.h
gsedata.o: /usr/include/strings.h /usr/include/secure/_string.h ../basecnst.h
gsedata.o: /usr/include/stdlib.h /usr/include/sys/wait.h
gsedata.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
gsedata.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
gsedata.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
gsedata.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
gsedata.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
gsedata.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
gsedata.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
gsedata.o: /usr/include/machine/types.h /usr/include/i386/types.h
gsedata.o: ../sysbase.h ../fousrdef.h lineparse.h gsedata.h f2c.h
lineparse.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
lineparse.o: /usr/include/sys/_symbol_aliasing.h
lineparse.o: /usr/include/sys/_posix_availability.h
lineparse.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
lineparse.o: /usr/include/_types.h /usr/include/sys/_types.h
lineparse.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
lineparse.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
lineparse.o: /usr/include/string.h /usr/include/strings.h
lineparse.o: /usr/include/secure/_string.h ../basecnst.h ../sysbase.h
lineparse.o: ../fousrdef.h lineparse.h
sh_ah.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
sh_ah.o: /usr/include/sys/_symbol_aliasing.h
sh_ah.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
sh_ah.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
sh_ah.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
sh_ah.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
sh_ah.o: /usr/include/secure/_common.h /usr/include/string.h
sh_ah.o: /usr/include/strings.h /usr/include/secure/_string.h
sh_ah.o: /usr/include/ctype.h /usr/include/runetype.h /usr/include/math.h
sh_ah.o: /usr/include/architecture/i386/math.h ../basecnst.h ../sysbase.h
sh_ah.o: ../fousrdef.h ../tcusrdef.h sh_ah.h
sh_gse.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
sh_gse.o: /usr/include/sys/_symbol_aliasing.h
sh_gse.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
sh_gse.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
sh_gse.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
sh_gse.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
sh_gse.o: /usr/include/secure/_common.h /usr/include/string.h
sh_gse.o: /usr/include/strings.h /usr/include/secure/_string.h ../basecnst.h
sh_gse.o: /usr/include/stdlib.h /usr/include/sys/wait.h
sh_gse.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
sh_gse.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
sh_gse.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
sh_gse.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
sh_gse.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
sh_gse.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
sh_gse.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
sh_gse.o: /usr/include/machine/types.h /usr/include/i386/types.h ../sysbase.h
sh_gse.o: ../fousrdef.h ../tcusrdef.h sh_gse.h
sh_gse2.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
sh_gse2.o: /usr/include/sys/_symbol_aliasing.h
sh_gse2.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
sh_gse2.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
sh_gse2.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
sh_gse2.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
sh_gse2.o: /usr/include/secure/_common.h /usr/include/string.h
sh_gse2.o: /usr/include/strings.h /usr/include/secure/_string.h ../basecnst.h
sh_gse2.o: /usr/include/stdlib.h /usr/include/sys/wait.h
sh_gse2.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
sh_gse2.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
sh_gse2.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
sh_gse2.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
sh_gse2.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
sh_gse2.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
sh_gse2.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
sh_gse2.o: /usr/include/machine/types.h /usr/include/i386/types.h
sh_gse2.o: ../sysbase.h ../fousrdef.h ../tcusrdef.h gsedata.h sh_gse2.h
sh_len.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
sh_len.o: /usr/include/sys/_symbol_aliasing.h
sh_len.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
sh_len.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
sh_len.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
sh_len.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
sh_len.o: /usr/include/secure/_common.h /usr/include/string.h
sh_len.o: /usr/include/strings.h /usr/include/secure/_string.h
sh_len.o: /usr/include/stdlib.h /usr/include/sys/wait.h
sh_len.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
sh_len.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
sh_len.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
sh_len.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
sh_len.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
sh_len.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
sh_len.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
sh_len.o: /usr/include/machine/types.h /usr/include/i386/types.h
sh_len.o: /usr/include/ctype.h /usr/include/runetype.h ../basecnst.h
sh_len.o: ../sysbase.h ../fousrdef.h ../tcusrdef.h ../utusrdef.h sh_len.h
sh_neic.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
sh_neic.o: /usr/include/sys/_symbol_aliasing.h
sh_neic.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
sh_neic.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
sh_neic.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
sh_neic.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
sh_neic.o: /usr/include/secure/_common.h /usr/include/string.h
sh_neic.o: /usr/include/strings.h /usr/include/secure/_string.h
sh_neic.o: /usr/include/math.h /usr/include/architecture/i386/math.h
sh_neic.o: ../basecnst.h ../sysbase.h ../fousrdef.h ../tcusrdef.h
sh_neic.o: ../qfusrdef.h ../dfqentry.h sh_neic.h
shfrgn.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
shfrgn.o: /usr/include/sys/_symbol_aliasing.h
shfrgn.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
shfrgn.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
shfrgn.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
shfrgn.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
shfrgn.o: /usr/include/secure/_common.h ../basecnst.h ../sysbase.h
shfrgn.o: ../fousrdef.h ../shconst.h shfrgn.h sh_neic.h sh_gse.h sh_ah.h
shfrgn.o: sh_len.h sh_gse2.h
