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

check_multi_tape.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
check_multi_tape.o: /usr/include/sys/feature_tests.h
check_multi_tape.o: /usr/include/sys/isa_defs.h /usr/include/sys/va_list.h
check_multi_tape.o: /usr/include/stdio_tag.h /usr/include/stdio_impl.h
check_multi_tape.o: /usr/include/string.h /usr/include/iso/string_iso.h
check_multi_tape.o: /usr/include/fcntl.h /usr/include/sys/types.h
check_multi_tape.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
check_multi_tape.o: /usr/include/sys/select.h /usr/include/sys/time.h
check_multi_tape.o: /usr/include/sys/time.h /usr/include/sys/fcntl.h
check_multi_tape.o: /usr/include/sys/uio.h ../basecnst.h
check_multi_tape.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
check_multi_tape.o: ../sysbase.h ../fousrdef.h ../cpar.h ../erusrdef.h
check_multi_tape.o: ../tcusrdef.h ../utusrdef.h seedcfg.h seed_lib.h
check_multi_tape.o: tape_basics.h unix_io.h
copy_recs.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
copy_recs.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
copy_recs.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
copy_recs.o: /usr/include/stdio_impl.h /usr/include/string.h
copy_recs.o: /usr/include/iso/string_iso.h ../basecnst.h ../sysbase.h
copy_recs.o: ../fousrdef.h ../cpar.h ../tcusrdef.h ../erusrdef.h seedcfg.h
copy_recs.o: seed_lib.h seed_cd.h
demultiplex.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
demultiplex.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
demultiplex.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
demultiplex.o: /usr/include/stdio_impl.h /usr/include/string.h
demultiplex.o: /usr/include/iso/string_iso.h /usr/include/ctype.h
demultiplex.o: /usr/include/iso/ctype_iso.h ../basecnst.h ../sysbase.h
demultiplex.o: ../fousrdef.h ../cpar.h ../erusrdef.h seedcfg.h seed_lib.h
edit_rectime.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
edit_rectime.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
edit_rectime.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
edit_rectime.o: /usr/include/stdio_impl.h /usr/include/string.h
edit_rectime.o: /usr/include/iso/string_iso.h ../basecnst.h ../sysbase.h
edit_rectime.o: ../fousrdef.h ../cpar.h ../erusrdef.h seedcfg.h seed_lib.h
find_stream_calib.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
find_stream_calib.o: /usr/include/sys/feature_tests.h
find_stream_calib.o: /usr/include/sys/isa_defs.h /usr/include/sys/va_list.h
find_stream_calib.o: /usr/include/stdio_tag.h /usr/include/stdio_impl.h
find_stream_calib.o: /usr/include/string.h /usr/include/iso/string_iso.h
find_stream_calib.o: ../basecnst.h ../sysbase.h ../fousrdef.h ../cpar.h
find_stream_calib.o: ../erusrdef.h seedcfg.h seed_lib.h
fir_resample.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
fir_resample.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
fir_resample.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
fir_resample.o: /usr/include/stdio_impl.h /usr/include/string.h
fir_resample.o: /usr/include/iso/string_iso.h ../basecnst.h
fir_resample.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
fir_resample.o: ../sysbase.h ../fousrdef.h ../tcusrdef.h ../erusrdef.h
fir_resample.o: ../utusrdef.h ../cpar.h seedcfg.h seed_lib.h
fir_resample.o: ../globalparams.h
fixseed.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
fixseed.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
fixseed.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
fixseed.o: /usr/include/stdio_impl.h /usr/include/string.h
fixseed.o: /usr/include/iso/string_iso.h ../basecnst.h /usr/include/stdlib.h
fixseed.o: /usr/include/iso/stdlib_iso.h ../sysbase.h ../fousrdef.h ../cpar.h
gse2_stream_calib.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
gse2_stream_calib.o: /usr/include/sys/feature_tests.h
gse2_stream_calib.o: /usr/include/sys/isa_defs.h /usr/include/sys/va_list.h
gse2_stream_calib.o: /usr/include/stdio_tag.h /usr/include/stdio_impl.h
gse2_stream_calib.o: /usr/include/string.h /usr/include/iso/string_iso.h
gse2_stream_calib.o: ../basecnst.h ../sysbase.h ../fousrdef.h
gse2_stream_calib.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
gse2_stream_calib.o: ../cpar.h ../erusrdef.h ../utusrdef.h ../tcusrdef.h
gse2_stream_calib.o: seedcfg.h seed_lib.h
inquire_avd.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
inquire_avd.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
inquire_avd.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
inquire_avd.o: /usr/include/stdio_impl.h /usr/include/string.h
inquire_avd.o: /usr/include/iso/string_iso.h ../basecnst.h ../sysbase.h
inquire_avd.o: ../fousrdef.h seedcfg.h seed_lib.h
inquire_max_span.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
inquire_max_span.o: /usr/include/sys/feature_tests.h
inquire_max_span.o: /usr/include/sys/isa_defs.h /usr/include/sys/va_list.h
inquire_max_span.o: /usr/include/stdio_tag.h /usr/include/stdio_impl.h
inquire_max_span.o: /usr/include/string.h /usr/include/iso/string_iso.h
inquire_max_span.o: ../basecnst.h ../sysbase.h ../fousrdef.h seedcfg.h
inquire_max_span.o: seed_lib.h ../cpar.h ../tcusrdef.h ../erusrdef.h
inquire_max_span_bh.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
inquire_max_span_bh.o: /usr/include/sys/feature_tests.h
inquire_max_span_bh.o: /usr/include/sys/isa_defs.h /usr/include/sys/va_list.h
inquire_max_span_bh.o: /usr/include/stdio_tag.h /usr/include/stdio_impl.h
inquire_max_span_bh.o: /usr/include/string.h /usr/include/iso/string_iso.h
inquire_max_span_bh.o: ../basecnst.h ../sysbase.h ../fousrdef.h seedcfg.h
inquire_max_span_bh.o: seed_lib.h ../cpar.h ../tcusrdef.h ../erusrdef.h
inquire_min_span.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
inquire_min_span.o: /usr/include/sys/feature_tests.h
inquire_min_span.o: /usr/include/sys/isa_defs.h /usr/include/sys/va_list.h
inquire_min_span.o: /usr/include/stdio_tag.h /usr/include/stdio_impl.h
inquire_min_span.o: /usr/include/string.h /usr/include/iso/string_iso.h
inquire_min_span.o: ../basecnst.h ../sysbase.h ../fousrdef.h seedcfg.h
inquire_min_span.o: seed_lib.h ../cpar.h ../tcusrdef.h ../erusrdef.h
inquire_time_span.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
inquire_time_span.o: /usr/include/sys/feature_tests.h
inquire_time_span.o: /usr/include/sys/isa_defs.h /usr/include/sys/va_list.h
inquire_time_span.o: /usr/include/stdio_tag.h /usr/include/stdio_impl.h
inquire_time_span.o: /usr/include/string.h /usr/include/iso/string_iso.h
inquire_time_span.o: ../basecnst.h ../sysbase.h ../fousrdef.h seedcfg.h
inquire_time_span.o: seed_lib.h
lookup_cd.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
lookup_cd.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
lookup_cd.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
lookup_cd.o: /usr/include/stdio_impl.h /usr/include/string.h
lookup_cd.o: /usr/include/iso/string_iso.h ../basecnst.h ../sysbase.h
lookup_cd.o: ../fousrdef.h ../erusrdef.h ../cpar.h seed_cd.h
makesfd.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
makesfd.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
makesfd.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
makesfd.o: /usr/include/stdio_impl.h /usr/include/string.h
makesfd.o: /usr/include/iso/string_iso.h ../basecnst.h /usr/include/stdlib.h
makesfd.o: /usr/include/iso/stdlib_iso.h ../sysbase.h ../fousrdef.h ../cpar.h
makesfd.o: seedcfg.h
network_name.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
network_name.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
network_name.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
network_name.o: /usr/include/stdio_impl.h /usr/include/string.h
network_name.o: /usr/include/iso/string_iso.h
pcname.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
pcname.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
pcname.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
pcname.o: /usr/include/stdio_impl.h /usr/include/string.h
pcname.o: /usr/include/iso/string_iso.h ../basecnst.h ../sysbase.h
pcname.o: ../fousrdef.h ../tcusrdef.h
sc_logmsg.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
sc_logmsg.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
sc_logmsg.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
sc_logmsg.o: /usr/include/stdio_impl.h /usr/include/string.h
sc_logmsg.o: /usr/include/iso/string_iso.h ../basecnst.h ../sysbase.h
sc_logmsg.o: ../fousrdef.h ../cpar.h ../tcusrdef.h ../utusrdef.h
sc_logmsg.o: ../erusrdef.h seedcfg.h seed_lib.h
seed2sfd.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
seed2sfd.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
seed2sfd.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
seed2sfd.o: /usr/include/stdio_impl.h /usr/include/string.h
seed2sfd.o: /usr/include/iso/string_iso.h ../basecnst.h ../sysbase.h
seed2sfd.o: ../fousrdef.h ../cpar.h ../tcusrdef.h ../utusrdef.h ../erusrdef.h
seed2sfd.o: seedcfg.h seed_lib.h
seed512to4096.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
seed512to4096.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
seed512to4096.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
seed512to4096.o: /usr/include/stdio_impl.h /usr/include/string.h
seed512to4096.o: /usr/include/iso/string_iso.h ../basecnst.h
seed512to4096.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
seed512to4096.o: ../sysbase.h ../fousrdef.h ../erusrdef.h ../tcusrdef.h
seed512to4096.o: ../cpar.h seedcfg.h seed_lib.h
seed_cd.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
seed_cd.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
seed_cd.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
seed_cd.o: /usr/include/stdio_impl.h /usr/include/string.h
seed_cd.o: /usr/include/iso/string_iso.h ../basecnst.h /usr/include/stdlib.h
seed_cd.o: /usr/include/iso/stdlib_iso.h ../sysbase.h ../fousrdef.h
seed_cd.o: ../utusrdef.h ../tcusrdef.h ../erusrdef.h seedcfg.h seed_lib.h
seed_cd.o: seed_cd.h ../stations.h
seed_lib.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
seed_lib.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
seed_lib.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
seed_lib.o: /usr/include/stdio_impl.h /usr/include/string.h
seed_lib.o: /usr/include/iso/string_iso.h /usr/include/math.h
seed_lib.o: /usr/include/iso/math_iso.h ../basecnst.h /usr/include/unistd.h
seed_lib.o: /usr/include/sys/types.h /usr/include/sys/machtypes.h
seed_lib.o: /usr/include/sys/int_types.h /usr/include/sys/select.h
seed_lib.o: /usr/include/sys/time.h /usr/include/sys/time.h
seed_lib.o: /usr/include/sys/unistd.h /usr/include/stdlib.h
seed_lib.o: /usr/include/iso/stdlib_iso.h /usr/include/fcntl.h
seed_lib.o: /usr/include/sys/fcntl.h ../sysbase.h ../fousrdef.h ../tcusrdef.h
seed_lib.o: ../erusrdef.h ../utusrdef.h ../globalparams.h seedcfg.h
seed_lib.o: seed_lib.h steim1.h
seed_tape.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
seed_tape.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
seed_tape.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
seed_tape.o: /usr/include/stdio_impl.h /usr/include/string.h
seed_tape.o: /usr/include/iso/string_iso.h /usr/include/stdlib.h
seed_tape.o: /usr/include/iso/stdlib_iso.h /usr/include/ctype.h
seed_tape.o: /usr/include/iso/ctype_iso.h /usr/include/fcntl.h
seed_tape.o: /usr/include/sys/types.h /usr/include/sys/machtypes.h
seed_tape.o: /usr/include/sys/int_types.h /usr/include/sys/select.h
seed_tape.o: /usr/include/sys/time.h /usr/include/sys/time.h
seed_tape.o: /usr/include/sys/fcntl.h /usr/include/sys/uio.h ../basecnst.h
seed_tape.o: ../sysbase.h ../fousrdef.h ../cpar.h ../tcusrdef.h ../utusrdef.h
seed_tape.o: ../erusrdef.h seedcfg.h tape_basics.h unix_io.h
seed_tidy.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
seed_tidy.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
seed_tidy.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
seed_tidy.o: /usr/include/stdio_impl.h /usr/include/string.h
seed_tidy.o: /usr/include/iso/string_iso.h ../basecnst.h
seed_tidy.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
seed_tidy.o: /usr/include/fcntl.h /usr/include/sys/types.h
seed_tidy.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
seed_tidy.o: /usr/include/sys/select.h /usr/include/sys/time.h
seed_tidy.o: /usr/include/sys/time.h /usr/include/sys/fcntl.h
seed_tidy.o: /usr/include/sys/uio.h ../sysbase.h ../fousrdef.h ../erusrdef.h
seed_tidy.o: ../tcusrdef.h ../utusrdef.h ../cpar.h seedcfg.h seed_lib.h
seed_tidy.o: unix_io.h
seedbase.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
seedbase.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
seedbase.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
seedbase.o: /usr/include/stdio_impl.h /usr/include/string.h
seedbase.o: /usr/include/iso/string_iso.h ../basecnst.h /usr/include/stdlib.h
seedbase.o: /usr/include/iso/stdlib_iso.h ../sysbase.h ../fousrdef.h
seedbase.o: ../cpar.h ../tcusrdef.h ../utusrdef.h ../erusrdef.h seedcfg.h
seedgaps.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
seedgaps.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
seedgaps.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
seedgaps.o: /usr/include/stdio_impl.h /usr/include/string.h
seedgaps.o: /usr/include/iso/string_iso.h ../basecnst.h /usr/include/stdlib.h
seedgaps.o: /usr/include/iso/stdlib_iso.h ../sysbase.h ../fousrdef.h
seedgaps.o: ../cpar.h ../erusrdef.h ../tcusrdef.h seedcfg.h seed_lib.h
seedmerge.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
seedmerge.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
seedmerge.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
seedmerge.o: /usr/include/stdio_impl.h /usr/include/string.h
seedmerge.o: /usr/include/iso/string_iso.h ../basecnst.h
seedmerge.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
seedmerge.o: /usr/include/unistd.h /usr/include/sys/types.h
seedmerge.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
seedmerge.o: /usr/include/sys/select.h /usr/include/sys/time.h
seedmerge.o: /usr/include/sys/time.h /usr/include/sys/unistd.h ../sysbase.h
seedmerge.o: ../fousrdef.h ../cpar.h ../erusrdef.h ../tcusrdef.h seedcfg.h
seedmerge.o: seed_lib.h
seedquickdump.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
seedquickdump.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
seedquickdump.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
seedquickdump.o: /usr/include/stdio_impl.h /usr/include/string.h
seedquickdump.o: /usr/include/iso/string_iso.h ../basecnst.h ../sysbase.h
seedquickdump.o: ../fousrdef.h ../cpar.h ../tcusrdef.h ../utusrdef.h
seedquickdump.o: ../erusrdef.h seedcfg.h seed_lib.h
sfd2db-datetime.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
sfd2db-datetime.o: /usr/include/sys/feature_tests.h
sfd2db-datetime.o: /usr/include/sys/isa_defs.h /usr/include/sys/va_list.h
sfd2db-datetime.o: /usr/include/stdio_tag.h /usr/include/stdio_impl.h
sfd2db-datetime.o: /usr/include/string.h /usr/include/iso/string_iso.h
sfd2db-datetime.o: ../basecnst.h ../sysbase.h ../fousrdef.h ../cpar.h
sfd2db-datetime.o: ../tcusrdef.h seedcfg.h seed_lib.h
sfd2db.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
sfd2db.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
sfd2db.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
sfd2db.o: /usr/include/stdio_impl.h /usr/include/string.h
sfd2db.o: /usr/include/iso/string_iso.h /usr/include/stdlib.h
sfd2db.o: /usr/include/iso/stdlib_iso.h ../basecnst.h ../sysbase.h
sfd2db.o: ../fousrdef.h ../cpar.h ../tcusrdef.h seedcfg.h seed_lib.h
sfd_extract_times.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
sfd_extract_times.o: /usr/include/sys/feature_tests.h
sfd_extract_times.o: /usr/include/sys/isa_defs.h /usr/include/sys/va_list.h
sfd_extract_times.o: /usr/include/stdio_tag.h /usr/include/stdio_impl.h
sfd_extract_times.o: /usr/include/string.h /usr/include/iso/string_iso.h
sfd_extract_times.o: ../basecnst.h ../sysbase.h ../fousrdef.h seedcfg.h
sfd_extract_times.o: seed_lib.h
sfdline.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
sfdline.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
sfdline.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
sfdline.o: /usr/include/stdio_impl.h /usr/include/string.h
sfdline.o: /usr/include/iso/string_iso.h ../basecnst.h ../sysbase.h
sfdline.o: ../fousrdef.h ../cpar.h ../tcusrdef.h ../utusrdef.h ../erusrdef.h
sfdline.o: seedcfg.h seed_lib.h
split_seed.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
split_seed.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
split_seed.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
split_seed.o: /usr/include/stdio_impl.h /usr/include/string.h
split_seed.o: /usr/include/iso/string_iso.h ../basecnst.h ../sysbase.h
split_seed.o: ../fousrdef.h ../cpar.h ../erusrdef.h seedcfg.h seed_lib.h
splitd.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
splitd.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
splitd.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
splitd.o: /usr/include/stdio_impl.h /usr/include/string.h
splitd.o: /usr/include/iso/string_iso.h /usr/include/unistd.h
splitd.o: /usr/include/sys/types.h /usr/include/sys/machtypes.h
splitd.o: /usr/include/sys/int_types.h /usr/include/sys/select.h
splitd.o: /usr/include/sys/time.h /usr/include/sys/time.h
splitd.o: /usr/include/sys/unistd.h
steimalg.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
steimalg.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
steimalg.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
steimalg.o: /usr/include/stdio_impl.h /usr/include/string.h
steimalg.o: /usr/include/iso/string_iso.h steim1.h
swap_vax_recs.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
swap_vax_recs.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
swap_vax_recs.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
swap_vax_recs.o: /usr/include/stdio_impl.h /usr/include/string.h
swap_vax_recs.o: /usr/include/iso/string_iso.h ../basecnst.h ../sysbase.h
swap_vax_recs.o: ../fousrdef.h ../cpar.h ../tcusrdef.h ../erusrdef.h
swap_vax_recs.o: seedcfg.h seed_lib.h
tape_basics.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
tape_basics.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
tape_basics.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
tape_basics.o: /usr/include/stdio_impl.h /usr/include/string.h
tape_basics.o: /usr/include/iso/string_iso.h /usr/include/stdlib.h
tape_basics.o: /usr/include/iso/stdlib_iso.h /usr/include/fcntl.h
tape_basics.o: /usr/include/sys/types.h /usr/include/sys/machtypes.h
tape_basics.o: /usr/include/sys/int_types.h /usr/include/sys/select.h
tape_basics.o: /usr/include/sys/time.h /usr/include/sys/time.h
tape_basics.o: /usr/include/sys/fcntl.h /usr/include/sys/uio.h ../basecnst.h
tape_basics.o: /usr/include/sys/ioccom.h /usr/include/sys/mtio.h
write_steim1.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
write_steim1.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
write_steim1.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
write_steim1.o: /usr/include/stdio_impl.h /usr/include/string.h
write_steim1.o: /usr/include/iso/string_iso.h ../basecnst.h
write_steim1.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
write_steim1.o: ../sysbase.h ../fousrdef.h ../tcusrdef.h ../erusrdef.h
write_steim1.o: ../cpar.h seedcfg.h seed_lib.h
