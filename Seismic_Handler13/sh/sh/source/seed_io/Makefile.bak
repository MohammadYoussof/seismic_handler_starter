# file Makefile
#      ========
#
# version 33, 4-Nov-2006
#
# Makefile of SEED/IO directory
# K. Stammler, 6-Sep-93


CFLAGS      =	$(SH_CCOPTIONS) -DBASECNST=\"basecnst.h\" -I.. -I$(SH_UTIL)
CC          =	$(SH_COMPILER)
LD          =	$(SH_COMPILER)
LINK.c      =	$(SH_COMPILER)
RANLIB		=	$(SH_RANLIB)
LDFLAGS     =	-L$(SH_LIB) $(SH_LINKOPTIONS)
LIBS        =	-lseed -lsource -lm $(SH_LINKLIBS)
OBJLIB		= ..
FKDIR			= $(SH_UTIL)fk_src
ALL_LOCAL_SRCS	= *.c
LIB_SEED_SRCS	= seed_cd.c seed_lib.c seedbase.c steimalg.c tape_basics.c
#LIB_SEED_OBJS	= $(LIB_SEED_SRCS:%.c=%.o)
LIB_SEED_OBJS	= $(LIB_SEED_SRCS:.c=.o)

LIB_SEED		=	$(SH_LIB)libseed.a
LIB_SOURCE	=	$(SH_LIB)libsource.a

ALL         =	seedgaps \
					makesfd \
					sfdline \
					sfd2db \
					seed2sfd \
					seed512to4096 \
					seedquickdump \
					sc_logmsg \
					seedmerge \
					fixseed \
					write_steim1 \
					seed_tidy \
					sfd_extract_times \
					pcname \
					copy_recs \
					swap_vax_recs \
					split_seed \
					edit_rectime \
					demultiplex \
					inquire_time_span \
					inquire_avd \
					inquire_min_span \
					inquire_max_span \
					inquire_max_span_bh \
					lookup_cd \
					seed_tape \
					find_stream_calib \
					gse2_stream_calib \
					check_multi_tape \
					network_name \
					fir_resample
#					scan_streams

all:
		make $(LIB_SEED) $(ALL)

$(LIB_SEED):	$(LIB_SEED_OBJS)
	ar r $@ $(LIB_SEED_OBJS); $(RANLIB) $@

$(LIB_SOURCE):
	-@echo "libsource.a not up to date"

seedgaps: seedgaps.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

makesfd: makesfd.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

sfdline: sfdline.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

sfd2db: sfd2db.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

seed2sfd: seed2sfd.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

seed512to4096: seed512to4096.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

seedquickdump: seedquickdump.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

sc_logmsg: sc_logmsg.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

seedmerge: seedmerge.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

fixseed: fixseed.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

write_steim1: write_steim1.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

seed_tidy: seed_tidy.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

sfd_extract_times: sfd_extract_times.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

pcname: pcname.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

copy_recs: copy_recs.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

swap_vax_recs: swap_vax_recs.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

split_seed: split_seed.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

edit_rectime: edit_rectime.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

demultiplex: demultiplex.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

inquire_time_span: inquire_time_span.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

inquire_avd: inquire_avd.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

inquire_min_span: inquire_min_span.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

inquire_max_span: inquire_max_span.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

inquire_max_span_bh: inquire_max_span_bh.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

lookup_cd: lookup_cd.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

seed_tape: seed_tape.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

find_stream_calib: find_stream_calib.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

gse2_stream_calib: gse2_stream_calib.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

check_multi_tape: check_multi_tape.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

network_name: network_name.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

fir_resample: fir_resample.o $(LIB_SOURCE) $(LIB_SEED)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

tape_basics.o:
		$(SH_COMPILER) -c $(CFLAGS) tape_basics.c

clean:
	@rm *.o *% $(ALL)

depend:
	makedepend -s "# DO NOT DELETE" -- $(CFLAGS) -- $(ALL_LOCAL_SRCS)


# DO NOT DELETE

check_multi_tape.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
check_multi_tape.o: /usr/include/sys/_symbol_aliasing.h
check_multi_tape.o: /usr/include/sys/_posix_availability.h
check_multi_tape.o: /usr/include/Availability.h
check_multi_tape.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
check_multi_tape.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
check_multi_tape.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
check_multi_tape.o: /usr/include/secure/_common.h /usr/include/string.h
check_multi_tape.o: /usr/include/strings.h /usr/include/secure/_string.h
check_multi_tape.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
check_multi_tape.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
check_multi_tape.o: /usr/include/machine/types.h /usr/include/i386/types.h
check_multi_tape.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
check_multi_tape.o: /usr/include/sys/_endian.h
check_multi_tape.o: /usr/include/libkern/_OSByteOrder.h
check_multi_tape.o: /usr/include/libkern/i386/_OSByteOrder.h
check_multi_tape.o: /usr/include/sys/_structs.h /usr/include/sys/uio.h
check_multi_tape.o: ../basecnst.h /usr/include/stdlib.h
check_multi_tape.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
check_multi_tape.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
check_multi_tape.o: /usr/include/i386/_structs.h /usr/include/sys/resource.h
check_multi_tape.o: /usr/include/alloca.h ../sysbase.h ../fousrdef.h
check_multi_tape.o: ../cpar.h ../erusrdef.h ../tcusrdef.h ../utusrdef.h
check_multi_tape.o: seedcfg.h seed_lib.h tape_basics.h unix_io.h
copy_recs.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
copy_recs.o: /usr/include/sys/_symbol_aliasing.h
copy_recs.o: /usr/include/sys/_posix_availability.h
copy_recs.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
copy_recs.o: /usr/include/_types.h /usr/include/sys/_types.h
copy_recs.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
copy_recs.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
copy_recs.o: /usr/include/string.h /usr/include/strings.h
copy_recs.o: /usr/include/secure/_string.h ../basecnst.h ../sysbase.h
copy_recs.o: ../fousrdef.h ../cpar.h ../tcusrdef.h ../erusrdef.h seedcfg.h
copy_recs.o: seed_lib.h seed_cd.h
demultiplex.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
demultiplex.o: /usr/include/sys/_symbol_aliasing.h
demultiplex.o: /usr/include/sys/_posix_availability.h
demultiplex.o: /usr/include/Availability.h
demultiplex.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
demultiplex.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
demultiplex.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
demultiplex.o: /usr/include/secure/_common.h /usr/include/string.h
demultiplex.o: /usr/include/strings.h /usr/include/secure/_string.h
demultiplex.o: /usr/include/ctype.h /usr/include/runetype.h ../basecnst.h
demultiplex.o: ../sysbase.h ../fousrdef.h ../cpar.h ../erusrdef.h seedcfg.h
demultiplex.o: seed_lib.h
edit_rectime.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
edit_rectime.o: /usr/include/sys/_symbol_aliasing.h
edit_rectime.o: /usr/include/sys/_posix_availability.h
edit_rectime.o: /usr/include/Availability.h
edit_rectime.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
edit_rectime.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
edit_rectime.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
edit_rectime.o: /usr/include/secure/_common.h /usr/include/string.h
edit_rectime.o: /usr/include/strings.h /usr/include/secure/_string.h
edit_rectime.o: ../basecnst.h ../sysbase.h ../fousrdef.h ../cpar.h
edit_rectime.o: ../erusrdef.h seedcfg.h seed_lib.h
find_stream_calib.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
find_stream_calib.o: /usr/include/sys/_symbol_aliasing.h
find_stream_calib.o: /usr/include/sys/_posix_availability.h
find_stream_calib.o: /usr/include/Availability.h
find_stream_calib.o: /usr/include/AvailabilityInternal.h
find_stream_calib.o: /usr/include/_types.h /usr/include/sys/_types.h
find_stream_calib.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
find_stream_calib.o: /usr/include/secure/_stdio.h
find_stream_calib.o: /usr/include/secure/_common.h /usr/include/string.h
find_stream_calib.o: /usr/include/strings.h /usr/include/secure/_string.h
find_stream_calib.o: ../basecnst.h ../sysbase.h ../fousrdef.h ../cpar.h
find_stream_calib.o: ../erusrdef.h seedcfg.h seed_lib.h
fir_resample.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
fir_resample.o: /usr/include/sys/_symbol_aliasing.h
fir_resample.o: /usr/include/sys/_posix_availability.h
fir_resample.o: /usr/include/Availability.h
fir_resample.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
fir_resample.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
fir_resample.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
fir_resample.o: /usr/include/secure/_common.h /usr/include/string.h
fir_resample.o: /usr/include/strings.h /usr/include/secure/_string.h
fir_resample.o: ../basecnst.h /usr/include/stdlib.h /usr/include/sys/wait.h
fir_resample.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
fir_resample.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
fir_resample.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
fir_resample.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
fir_resample.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
fir_resample.o: /usr/include/libkern/_OSByteOrder.h
fir_resample.o: /usr/include/libkern/i386/_OSByteOrder.h
fir_resample.o: /usr/include/alloca.h /usr/include/machine/types.h
fir_resample.o: /usr/include/i386/types.h ../sysbase.h ../fousrdef.h
fir_resample.o: ../tcusrdef.h ../erusrdef.h ../utusrdef.h ../cpar.h seedcfg.h
fir_resample.o: seed_lib.h ../globalparams.h
fixseed.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
fixseed.o: /usr/include/sys/_symbol_aliasing.h
fixseed.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
fixseed.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
fixseed.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
fixseed.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
fixseed.o: /usr/include/secure/_common.h /usr/include/string.h
fixseed.o: /usr/include/strings.h /usr/include/secure/_string.h ../basecnst.h
fixseed.o: /usr/include/stdlib.h /usr/include/sys/wait.h
fixseed.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
fixseed.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
fixseed.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
fixseed.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
fixseed.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
fixseed.o: /usr/include/libkern/_OSByteOrder.h
fixseed.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
fixseed.o: /usr/include/machine/types.h /usr/include/i386/types.h
fixseed.o: ../sysbase.h ../fousrdef.h ../cpar.h
gse2_stream_calib.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
gse2_stream_calib.o: /usr/include/sys/_symbol_aliasing.h
gse2_stream_calib.o: /usr/include/sys/_posix_availability.h
gse2_stream_calib.o: /usr/include/Availability.h
gse2_stream_calib.o: /usr/include/AvailabilityInternal.h
gse2_stream_calib.o: /usr/include/_types.h /usr/include/sys/_types.h
gse2_stream_calib.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
gse2_stream_calib.o: /usr/include/secure/_stdio.h
gse2_stream_calib.o: /usr/include/secure/_common.h /usr/include/string.h
gse2_stream_calib.o: /usr/include/strings.h /usr/include/secure/_string.h
gse2_stream_calib.o: ../basecnst.h ../sysbase.h ../fousrdef.h
gse2_stream_calib.o: /usr/include/stdlib.h /usr/include/sys/wait.h
gse2_stream_calib.o: /usr/include/sys/signal.h
gse2_stream_calib.o: /usr/include/sys/appleapiopts.h
gse2_stream_calib.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
gse2_stream_calib.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
gse2_stream_calib.o: /usr/include/sys/resource.h
gse2_stream_calib.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
gse2_stream_calib.o: /usr/include/sys/_endian.h
gse2_stream_calib.o: /usr/include/libkern/_OSByteOrder.h
gse2_stream_calib.o: /usr/include/libkern/i386/_OSByteOrder.h
gse2_stream_calib.o: /usr/include/alloca.h /usr/include/machine/types.h
gse2_stream_calib.o: /usr/include/i386/types.h ../cpar.h ../erusrdef.h
gse2_stream_calib.o: ../utusrdef.h ../tcusrdef.h seedcfg.h seed_lib.h
inquire_avd.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
inquire_avd.o: /usr/include/sys/_symbol_aliasing.h
inquire_avd.o: /usr/include/sys/_posix_availability.h
inquire_avd.o: /usr/include/Availability.h
inquire_avd.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
inquire_avd.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
inquire_avd.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
inquire_avd.o: /usr/include/secure/_common.h /usr/include/string.h
inquire_avd.o: /usr/include/strings.h /usr/include/secure/_string.h
inquire_avd.o: ../basecnst.h ../sysbase.h ../fousrdef.h seedcfg.h seed_lib.h
inquire_max_span.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
inquire_max_span.o: /usr/include/sys/_symbol_aliasing.h
inquire_max_span.o: /usr/include/sys/_posix_availability.h
inquire_max_span.o: /usr/include/Availability.h
inquire_max_span.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
inquire_max_span.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
inquire_max_span.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
inquire_max_span.o: /usr/include/secure/_common.h /usr/include/string.h
inquire_max_span.o: /usr/include/strings.h /usr/include/secure/_string.h
inquire_max_span.o: ../basecnst.h ../sysbase.h ../fousrdef.h seedcfg.h
inquire_max_span.o: seed_lib.h ../cpar.h ../tcusrdef.h ../erusrdef.h
inquire_max_span_bh.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
inquire_max_span_bh.o: /usr/include/sys/_symbol_aliasing.h
inquire_max_span_bh.o: /usr/include/sys/_posix_availability.h
inquire_max_span_bh.o: /usr/include/Availability.h
inquire_max_span_bh.o: /usr/include/AvailabilityInternal.h
inquire_max_span_bh.o: /usr/include/_types.h /usr/include/sys/_types.h
inquire_max_span_bh.o: /usr/include/machine/_types.h
inquire_max_span_bh.o: /usr/include/i386/_types.h
inquire_max_span_bh.o: /usr/include/secure/_stdio.h
inquire_max_span_bh.o: /usr/include/secure/_common.h /usr/include/string.h
inquire_max_span_bh.o: /usr/include/strings.h /usr/include/secure/_string.h
inquire_max_span_bh.o: ../basecnst.h ../sysbase.h ../fousrdef.h seedcfg.h
inquire_max_span_bh.o: seed_lib.h ../cpar.h ../tcusrdef.h ../erusrdef.h
inquire_min_span.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
inquire_min_span.o: /usr/include/sys/_symbol_aliasing.h
inquire_min_span.o: /usr/include/sys/_posix_availability.h
inquire_min_span.o: /usr/include/Availability.h
inquire_min_span.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
inquire_min_span.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
inquire_min_span.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
inquire_min_span.o: /usr/include/secure/_common.h /usr/include/string.h
inquire_min_span.o: /usr/include/strings.h /usr/include/secure/_string.h
inquire_min_span.o: ../basecnst.h ../sysbase.h ../fousrdef.h seedcfg.h
inquire_min_span.o: seed_lib.h ../cpar.h ../tcusrdef.h ../erusrdef.h
inquire_time_span.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
inquire_time_span.o: /usr/include/sys/_symbol_aliasing.h
inquire_time_span.o: /usr/include/sys/_posix_availability.h
inquire_time_span.o: /usr/include/Availability.h
inquire_time_span.o: /usr/include/AvailabilityInternal.h
inquire_time_span.o: /usr/include/_types.h /usr/include/sys/_types.h
inquire_time_span.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
inquire_time_span.o: /usr/include/secure/_stdio.h
inquire_time_span.o: /usr/include/secure/_common.h /usr/include/string.h
inquire_time_span.o: /usr/include/strings.h /usr/include/secure/_string.h
inquire_time_span.o: ../basecnst.h ../sysbase.h ../fousrdef.h seedcfg.h
inquire_time_span.o: seed_lib.h
lookup_cd.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
lookup_cd.o: /usr/include/sys/_symbol_aliasing.h
lookup_cd.o: /usr/include/sys/_posix_availability.h
lookup_cd.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
lookup_cd.o: /usr/include/_types.h /usr/include/sys/_types.h
lookup_cd.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
lookup_cd.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
lookup_cd.o: /usr/include/string.h /usr/include/strings.h
lookup_cd.o: /usr/include/secure/_string.h ../basecnst.h ../sysbase.h
lookup_cd.o: ../fousrdef.h ../erusrdef.h ../cpar.h seed_cd.h
makesfd.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
makesfd.o: /usr/include/sys/_symbol_aliasing.h
makesfd.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
makesfd.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
makesfd.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
makesfd.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
makesfd.o: /usr/include/secure/_common.h /usr/include/string.h
makesfd.o: /usr/include/strings.h /usr/include/secure/_string.h ../basecnst.h
makesfd.o: /usr/include/stdlib.h /usr/include/sys/wait.h
makesfd.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
makesfd.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
makesfd.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
makesfd.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
makesfd.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
makesfd.o: /usr/include/libkern/_OSByteOrder.h
makesfd.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
makesfd.o: /usr/include/machine/types.h /usr/include/i386/types.h
makesfd.o: ../sysbase.h ../fousrdef.h ../cpar.h seedcfg.h
network_name.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
network_name.o: /usr/include/sys/_symbol_aliasing.h
network_name.o: /usr/include/sys/_posix_availability.h
network_name.o: /usr/include/Availability.h
network_name.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
network_name.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
network_name.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
network_name.o: /usr/include/secure/_common.h /usr/include/string.h
network_name.o: /usr/include/strings.h /usr/include/secure/_string.h
pcname.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
pcname.o: /usr/include/sys/_symbol_aliasing.h
pcname.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
pcname.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
pcname.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
pcname.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
pcname.o: /usr/include/secure/_common.h /usr/include/string.h
pcname.o: /usr/include/strings.h /usr/include/secure/_string.h ../basecnst.h
pcname.o: ../sysbase.h ../fousrdef.h ../tcusrdef.h
sc_logmsg.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
sc_logmsg.o: /usr/include/sys/_symbol_aliasing.h
sc_logmsg.o: /usr/include/sys/_posix_availability.h
sc_logmsg.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
sc_logmsg.o: /usr/include/_types.h /usr/include/sys/_types.h
sc_logmsg.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
sc_logmsg.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
sc_logmsg.o: /usr/include/string.h /usr/include/strings.h
sc_logmsg.o: /usr/include/secure/_string.h ../basecnst.h ../sysbase.h
sc_logmsg.o: ../fousrdef.h ../cpar.h ../tcusrdef.h ../utusrdef.h
sc_logmsg.o: ../erusrdef.h seedcfg.h seed_lib.h
seed2sfd.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
seed2sfd.o: /usr/include/sys/_symbol_aliasing.h
seed2sfd.o: /usr/include/sys/_posix_availability.h
seed2sfd.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
seed2sfd.o: /usr/include/_types.h /usr/include/sys/_types.h
seed2sfd.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
seed2sfd.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
seed2sfd.o: /usr/include/string.h /usr/include/strings.h
seed2sfd.o: /usr/include/secure/_string.h ../basecnst.h ../sysbase.h
seed2sfd.o: ../fousrdef.h ../cpar.h ../tcusrdef.h ../utusrdef.h ../erusrdef.h
seed2sfd.o: seedcfg.h seed_lib.h
seed512to4096.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
seed512to4096.o: /usr/include/sys/_symbol_aliasing.h
seed512to4096.o: /usr/include/sys/_posix_availability.h
seed512to4096.o: /usr/include/Availability.h
seed512to4096.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
seed512to4096.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
seed512to4096.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
seed512to4096.o: /usr/include/secure/_common.h /usr/include/string.h
seed512to4096.o: /usr/include/strings.h /usr/include/secure/_string.h
seed512to4096.o: ../basecnst.h /usr/include/stdlib.h /usr/include/sys/wait.h
seed512to4096.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
seed512to4096.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
seed512to4096.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
seed512to4096.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
seed512to4096.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
seed512to4096.o: /usr/include/libkern/_OSByteOrder.h
seed512to4096.o: /usr/include/libkern/i386/_OSByteOrder.h
seed512to4096.o: /usr/include/alloca.h /usr/include/machine/types.h
seed512to4096.o: /usr/include/i386/types.h ../sysbase.h ../fousrdef.h
seed512to4096.o: ../erusrdef.h ../tcusrdef.h ../cpar.h seedcfg.h seed_lib.h
seed_cd.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
seed_cd.o: /usr/include/sys/_symbol_aliasing.h
seed_cd.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
seed_cd.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
seed_cd.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
seed_cd.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
seed_cd.o: /usr/include/secure/_common.h /usr/include/string.h
seed_cd.o: /usr/include/strings.h /usr/include/secure/_string.h ../basecnst.h
seed_cd.o: /usr/include/stdlib.h /usr/include/sys/wait.h
seed_cd.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
seed_cd.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
seed_cd.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
seed_cd.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
seed_cd.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
seed_cd.o: /usr/include/libkern/_OSByteOrder.h
seed_cd.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
seed_cd.o: /usr/include/machine/types.h /usr/include/i386/types.h
seed_cd.o: ../sysbase.h ../fousrdef.h ../utusrdef.h ../tcusrdef.h
seed_cd.o: ../erusrdef.h seedcfg.h seed_lib.h seed_cd.h ../stations.h
seed_lib.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
seed_lib.o: /usr/include/sys/_symbol_aliasing.h
seed_lib.o: /usr/include/sys/_posix_availability.h
seed_lib.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
seed_lib.o: /usr/include/_types.h /usr/include/sys/_types.h
seed_lib.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
seed_lib.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
seed_lib.o: /usr/include/string.h /usr/include/strings.h
seed_lib.o: /usr/include/secure/_string.h /usr/include/math.h
seed_lib.o: /usr/include/architecture/i386/math.h ../basecnst.h
seed_lib.o: /usr/include/unistd.h /usr/include/sys/unistd.h
seed_lib.o: /usr/include/sys/select.h /usr/include/sys/appleapiopts.h
seed_lib.o: /usr/include/sys/_structs.h /usr/include/sys/_select.h
seed_lib.o: /usr/include/stdlib.h /usr/include/sys/wait.h
seed_lib.o: /usr/include/sys/signal.h /usr/include/machine/signal.h
seed_lib.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
seed_lib.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
seed_lib.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
seed_lib.o: /usr/include/libkern/_OSByteOrder.h
seed_lib.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
seed_lib.o: /usr/include/machine/types.h /usr/include/i386/types.h
seed_lib.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
seed_lib.o: /usr/include/sys/types.h ../sysbase.h ../fousrdef.h ../tcusrdef.h
seed_lib.o: ../erusrdef.h ../utusrdef.h ../globalparams.h seedcfg.h
seed_lib.o: seed_lib.h steim1.h
seed_tape.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
seed_tape.o: /usr/include/sys/_symbol_aliasing.h
seed_tape.o: /usr/include/sys/_posix_availability.h
seed_tape.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
seed_tape.o: /usr/include/_types.h /usr/include/sys/_types.h
seed_tape.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
seed_tape.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
seed_tape.o: /usr/include/string.h /usr/include/strings.h
seed_tape.o: /usr/include/secure/_string.h /usr/include/stdlib.h
seed_tape.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
seed_tape.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
seed_tape.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
seed_tape.o: /usr/include/sys/_structs.h /usr/include/sys/resource.h
seed_tape.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
seed_tape.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
seed_tape.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
seed_tape.o: /usr/include/machine/types.h /usr/include/i386/types.h
seed_tape.o: /usr/include/ctype.h /usr/include/runetype.h
seed_tape.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
seed_tape.o: /usr/include/sys/types.h /usr/include/sys/uio.h ../basecnst.h
seed_tape.o: ../sysbase.h ../fousrdef.h ../cpar.h ../tcusrdef.h ../utusrdef.h
seed_tape.o: ../erusrdef.h seedcfg.h tape_basics.h unix_io.h
seed_tidy.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
seed_tidy.o: /usr/include/sys/_symbol_aliasing.h
seed_tidy.o: /usr/include/sys/_posix_availability.h
seed_tidy.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
seed_tidy.o: /usr/include/_types.h /usr/include/sys/_types.h
seed_tidy.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
seed_tidy.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
seed_tidy.o: /usr/include/string.h /usr/include/strings.h
seed_tidy.o: /usr/include/secure/_string.h ../basecnst.h
seed_tidy.o: /usr/include/stdlib.h /usr/include/sys/wait.h
seed_tidy.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
seed_tidy.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
seed_tidy.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
seed_tidy.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
seed_tidy.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
seed_tidy.o: /usr/include/libkern/_OSByteOrder.h
seed_tidy.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
seed_tidy.o: /usr/include/machine/types.h /usr/include/i386/types.h
seed_tidy.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
seed_tidy.o: /usr/include/sys/types.h /usr/include/sys/uio.h ../sysbase.h
seed_tidy.o: ../fousrdef.h ../erusrdef.h ../tcusrdef.h ../utusrdef.h
seed_tidy.o: ../cpar.h seedcfg.h seed_lib.h unix_io.h
seedbase.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
seedbase.o: /usr/include/sys/_symbol_aliasing.h
seedbase.o: /usr/include/sys/_posix_availability.h
seedbase.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
seedbase.o: /usr/include/_types.h /usr/include/sys/_types.h
seedbase.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
seedbase.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
seedbase.o: /usr/include/string.h /usr/include/strings.h
seedbase.o: /usr/include/secure/_string.h ../basecnst.h /usr/include/stdlib.h
seedbase.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
seedbase.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
seedbase.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
seedbase.o: /usr/include/sys/_structs.h /usr/include/sys/resource.h
seedbase.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
seedbase.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
seedbase.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
seedbase.o: /usr/include/machine/types.h /usr/include/i386/types.h
seedbase.o: ../sysbase.h ../fousrdef.h ../cpar.h ../tcusrdef.h ../utusrdef.h
seedbase.o: ../erusrdef.h seedcfg.h
seedgaps.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
seedgaps.o: /usr/include/sys/_symbol_aliasing.h
seedgaps.o: /usr/include/sys/_posix_availability.h
seedgaps.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
seedgaps.o: /usr/include/_types.h /usr/include/sys/_types.h
seedgaps.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
seedgaps.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
seedgaps.o: /usr/include/string.h /usr/include/strings.h
seedgaps.o: /usr/include/secure/_string.h ../basecnst.h /usr/include/stdlib.h
seedgaps.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
seedgaps.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
seedgaps.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
seedgaps.o: /usr/include/sys/_structs.h /usr/include/sys/resource.h
seedgaps.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
seedgaps.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
seedgaps.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
seedgaps.o: /usr/include/machine/types.h /usr/include/i386/types.h
seedgaps.o: ../sysbase.h ../fousrdef.h ../cpar.h ../erusrdef.h ../tcusrdef.h
seedgaps.o: seedcfg.h seed_lib.h
seedmerge.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
seedmerge.o: /usr/include/sys/_symbol_aliasing.h
seedmerge.o: /usr/include/sys/_posix_availability.h
seedmerge.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
seedmerge.o: /usr/include/_types.h /usr/include/sys/_types.h
seedmerge.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
seedmerge.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
seedmerge.o: /usr/include/string.h /usr/include/strings.h
seedmerge.o: /usr/include/secure/_string.h ../basecnst.h
seedmerge.o: /usr/include/stdlib.h /usr/include/sys/wait.h
seedmerge.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
seedmerge.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
seedmerge.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
seedmerge.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
seedmerge.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
seedmerge.o: /usr/include/libkern/_OSByteOrder.h
seedmerge.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
seedmerge.o: /usr/include/machine/types.h /usr/include/i386/types.h
seedmerge.o: /usr/include/unistd.h /usr/include/sys/unistd.h
seedmerge.o: /usr/include/sys/select.h /usr/include/sys/_select.h
seedmerge.o: ../sysbase.h ../fousrdef.h ../cpar.h ../erusrdef.h ../tcusrdef.h
seedmerge.o: seedcfg.h seed_lib.h
seedquickdump.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
seedquickdump.o: /usr/include/sys/_symbol_aliasing.h
seedquickdump.o: /usr/include/sys/_posix_availability.h
seedquickdump.o: /usr/include/Availability.h
seedquickdump.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
seedquickdump.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
seedquickdump.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
seedquickdump.o: /usr/include/secure/_common.h /usr/include/string.h
seedquickdump.o: /usr/include/strings.h /usr/include/secure/_string.h
seedquickdump.o: ../basecnst.h ../sysbase.h ../fousrdef.h ../cpar.h
seedquickdump.o: ../tcusrdef.h ../utusrdef.h ../erusrdef.h seedcfg.h
seedquickdump.o: seed_lib.h
sfd2db-datetime.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
sfd2db-datetime.o: /usr/include/sys/_symbol_aliasing.h
sfd2db-datetime.o: /usr/include/sys/_posix_availability.h
sfd2db-datetime.o: /usr/include/Availability.h
sfd2db-datetime.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
sfd2db-datetime.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
sfd2db-datetime.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
sfd2db-datetime.o: /usr/include/secure/_common.h /usr/include/string.h
sfd2db-datetime.o: /usr/include/strings.h /usr/include/secure/_string.h
sfd2db-datetime.o: ../basecnst.h ../sysbase.h ../fousrdef.h ../cpar.h
sfd2db-datetime.o: ../tcusrdef.h seedcfg.h seed_lib.h
sfd2db.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
sfd2db.o: /usr/include/sys/_symbol_aliasing.h
sfd2db.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
sfd2db.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
sfd2db.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
sfd2db.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
sfd2db.o: /usr/include/secure/_common.h /usr/include/string.h
sfd2db.o: /usr/include/strings.h /usr/include/secure/_string.h
sfd2db.o: /usr/include/stdlib.h /usr/include/sys/wait.h
sfd2db.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
sfd2db.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
sfd2db.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
sfd2db.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
sfd2db.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
sfd2db.o: /usr/include/libkern/_OSByteOrder.h
sfd2db.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
sfd2db.o: /usr/include/machine/types.h /usr/include/i386/types.h
sfd2db.o: ../basecnst.h ../sysbase.h ../fousrdef.h ../cpar.h ../tcusrdef.h
sfd2db.o: seedcfg.h seed_lib.h
sfd_extract_times.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
sfd_extract_times.o: /usr/include/sys/_symbol_aliasing.h
sfd_extract_times.o: /usr/include/sys/_posix_availability.h
sfd_extract_times.o: /usr/include/Availability.h
sfd_extract_times.o: /usr/include/AvailabilityInternal.h
sfd_extract_times.o: /usr/include/_types.h /usr/include/sys/_types.h
sfd_extract_times.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
sfd_extract_times.o: /usr/include/secure/_stdio.h
sfd_extract_times.o: /usr/include/secure/_common.h /usr/include/string.h
sfd_extract_times.o: /usr/include/strings.h /usr/include/secure/_string.h
sfd_extract_times.o: ../basecnst.h ../sysbase.h ../fousrdef.h seedcfg.h
sfd_extract_times.o: seed_lib.h
sfdline.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
sfdline.o: /usr/include/sys/_symbol_aliasing.h
sfdline.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
sfdline.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
sfdline.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
sfdline.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
sfdline.o: /usr/include/secure/_common.h /usr/include/string.h
sfdline.o: /usr/include/strings.h /usr/include/secure/_string.h ../basecnst.h
sfdline.o: ../sysbase.h ../fousrdef.h ../cpar.h ../tcusrdef.h ../utusrdef.h
sfdline.o: ../erusrdef.h seedcfg.h seed_lib.h
split_seed.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
split_seed.o: /usr/include/sys/_symbol_aliasing.h
split_seed.o: /usr/include/sys/_posix_availability.h
split_seed.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
split_seed.o: /usr/include/_types.h /usr/include/sys/_types.h
split_seed.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
split_seed.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
split_seed.o: /usr/include/string.h /usr/include/strings.h
split_seed.o: /usr/include/secure/_string.h ../basecnst.h ../sysbase.h
split_seed.o: ../fousrdef.h ../cpar.h ../erusrdef.h seedcfg.h seed_lib.h
splitd.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
splitd.o: /usr/include/sys/_symbol_aliasing.h
splitd.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
splitd.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
splitd.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
splitd.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
splitd.o: /usr/include/secure/_common.h /usr/include/string.h
splitd.o: /usr/include/strings.h /usr/include/secure/_string.h
splitd.o: /usr/include/unistd.h /usr/include/sys/unistd.h
splitd.o: /usr/include/sys/select.h /usr/include/sys/appleapiopts.h
splitd.o: /usr/include/sys/_structs.h /usr/include/sys/_select.h
steimalg.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
steimalg.o: /usr/include/sys/_symbol_aliasing.h
steimalg.o: /usr/include/sys/_posix_availability.h
steimalg.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
steimalg.o: /usr/include/_types.h /usr/include/sys/_types.h
steimalg.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
steimalg.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
steimalg.o: /usr/include/string.h /usr/include/strings.h
steimalg.o: /usr/include/secure/_string.h steim1.h
swap_vax_recs.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
swap_vax_recs.o: /usr/include/sys/_symbol_aliasing.h
swap_vax_recs.o: /usr/include/sys/_posix_availability.h
swap_vax_recs.o: /usr/include/Availability.h
swap_vax_recs.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
swap_vax_recs.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
swap_vax_recs.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
swap_vax_recs.o: /usr/include/secure/_common.h /usr/include/string.h
swap_vax_recs.o: /usr/include/strings.h /usr/include/secure/_string.h
swap_vax_recs.o: ../basecnst.h ../sysbase.h ../fousrdef.h ../cpar.h
swap_vax_recs.o: ../tcusrdef.h ../erusrdef.h seedcfg.h seed_lib.h
tape_basics.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
tape_basics.o: /usr/include/sys/_symbol_aliasing.h
tape_basics.o: /usr/include/sys/_posix_availability.h
tape_basics.o: /usr/include/Availability.h
tape_basics.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
tape_basics.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
tape_basics.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
tape_basics.o: /usr/include/secure/_common.h /usr/include/string.h
tape_basics.o: /usr/include/strings.h /usr/include/secure/_string.h
tape_basics.o: /usr/include/stdlib.h /usr/include/sys/wait.h
tape_basics.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
tape_basics.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
tape_basics.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
tape_basics.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
tape_basics.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
tape_basics.o: /usr/include/libkern/_OSByteOrder.h
tape_basics.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
tape_basics.o: /usr/include/machine/types.h /usr/include/i386/types.h
tape_basics.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
tape_basics.o: /usr/include/sys/types.h /usr/include/sys/uio.h ../basecnst.h
tape_basics.o: /usr/include/sys/ioccom.h /usr/include/sys/mtio.h
write_steim1.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
write_steim1.o: /usr/include/sys/_symbol_aliasing.h
write_steim1.o: /usr/include/sys/_posix_availability.h
write_steim1.o: /usr/include/Availability.h
write_steim1.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
write_steim1.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
write_steim1.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
write_steim1.o: /usr/include/secure/_common.h /usr/include/string.h
write_steim1.o: /usr/include/strings.h /usr/include/secure/_string.h
write_steim1.o: ../basecnst.h /usr/include/stdlib.h /usr/include/sys/wait.h
write_steim1.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
write_steim1.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
write_steim1.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
write_steim1.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
write_steim1.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
write_steim1.o: /usr/include/libkern/_OSByteOrder.h
write_steim1.o: /usr/include/libkern/i386/_OSByteOrder.h
write_steim1.o: /usr/include/alloca.h /usr/include/machine/types.h
write_steim1.o: /usr/include/i386/types.h ../sysbase.h ../fousrdef.h
write_steim1.o: ../tcusrdef.h ../erusrdef.h ../cpar.h seedcfg.h seed_lib.h
