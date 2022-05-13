# File Makefile
#      ========
#
# version 41, 28-Dec-2006
#
# Makefile of directory utility
# K. Stammler, 6-Sep-93

CFLAGS      =	$(SH_CCOPTIONS) -I$(SH_SOURCE) -I$(SH_SOURCE)/foreign -DBASECNST=\"basecnst.h\"
CC          = $(SH_COMPILER)
LD          = $(SH_COMPILER)
LINK.c      = $(SH_COMPILER)
RANLIB		= $(SH_RANLIB)
LDFLAGS     = -L$(SH_LIB) $(SH_LINKOPTIONS)
LIBS        =	-lutil -lsource -lm $(SH_LINKLIBS)
LIB_SOURCE	=	${SH_LIB}libsource.a
LIB_UTIL		=	${SH_LIB}libutil.a
LIB_UTIL_SRCS	=	eventdsc.c evtelex.c evhypo.c evhypout.c timelist.c evbullet.c
#LIB_UTIL_OBJS	=	$(LIB_UTIL_SRCS:%.c=%.o)
LIB_UTIL_OBJS	=	$(LIB_UTIL_SRCS:.c=.o)
LOCAL_SRCS	=	*.c


ALL         =	butfreq \
		butrec \
		fconcat \
		fereg \
		findevnt \
		flf2flt \
		evalflf \
		catflf \
		locadd \
		locdiff \
		find_next_loc \
		restfreq \
		restrec \
		simfreq \
		simrec \
		q2gse \
		hypo_stlist \
		ev2telex \
		ev2hypo \
		ev2hyposat \
		ev2hypocenter \
		ev4hypocenter \
		ev4hypou \
		ev2view \
		ev2bull \
		autoloc2evt \
		evt2reltimes \
		evt2relonset \
		evt2phasetimes \
		relonset2slo \
		onset_convert \
		check_evt \
		reformat_locsat_out \
		archevent_shc \
		timename \
		evt_copy_and_split \
		evt_time_sort \
		evt_correct_aniso \
		evt_extract_ms \
		qed_ms_make_evt \
		vrs_read_diff \
		traveltime \
		slo2distance \
		statinf \
		fit_travel \
		fit_travel2 \
		crusttrav \
		fit_to_line \
		getresid \
		floatop \
		weather2ascii \
		beamloc \
		stddev \
		find_topo30_id \
		stringop



all:
		make $(LIB_UTIL) $(ALL)

$(LIB_UTIL):	$(LIB_UTIL_OBJS)
		ar r $@ $(LIB_UTIL_OBJS); $(RANLIB) $@

butfreq:	butfreq.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

butrec: butrec.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

fconcat: fconcat.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

fereg: fereg.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

findevnt: findevnt.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

flf2flt: flf2flt.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

evalflf: evalflf.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

catflf: catflf.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

locadd: locadd.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

locdiff: locdiff.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

find_next_loc: find_next_loc.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

restfreq: restfreq.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

restrec: restrec.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

simfreq: simfreq.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

simrec: simrec.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

q2gse: q2gse.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

hypo_stlist: hypo_stlist.o $(LIB_SOURCE) $(LIB_UTIL)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

ev2view: ev2view.o $(LIB_SOURCE) $(LIB_UTIL)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

ev2bull: ev2bull.o $(LIB_SOURCE) $(LIB_UTIL)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

autoloc2evt: autoloc2evt.o $(LIB_SOURCE) $(LIB_UTIL)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

ev2telex: ev2telex.o $(LIB_SOURCE) $(LIB_UTIL)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

ev2hypo: ev2hypo.o $(LIB_SOURCE) $(LIB_UTIL)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

ev2hyposat: ev2hyposat.o $(LIB_SOURCE) $(LIB_UTIL)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

ev2hypocenter: ev2hypocenter.o $(LIB_SOURCE) $(LIB_UTIL)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

ev4hypocenter: ev4hypocenter.o $(LIB_SOURCE) $(LIB_UTIL)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

ev4hypou: ev4hypou.o $(LIB_SOURCE) $(LIB_UTIL)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

onset_convert: onset_convert.o $(LIB_SOURCE) $(LIB_UTIL)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

check_evt: check_evt.o $(LIB_SOURCE) $(LIB_UTIL)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

reformat_locsat_out: reformat_locsat_out.o $(LIB_SOURCE) $(LIB_UTIL)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

archevent_shc: archevent_shc.o $(LIB_SOURCE) $(LIB_UTIL)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

timename: timename.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

evt_copy_and_split: evt_copy_and_split.o $(LIB_SOURCE) $(LIB_UTIL)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

evt_time_sort: evt_time_sort.o $(LIB_SOURCE) $(LIB_UTIL)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

evt_correct_aniso: evt_correct_aniso.o $(LIB_SOURCE) $(LIB_UTIL)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

evt_extract_ms: evt_extract_ms.o $(LIB_SOURCE) $(LIB_UTIL)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

qed_ms_make_evt: qed_ms_make_evt.o $(LIB_SOURCE) $(LIB_UTIL)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

vrs_read_diff: vrs_read_diff.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

traveltime: traveltime.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

slo2distance: slo2distance.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

statinf: statinf.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

fit_travel: fit_travel.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

fit_travel2: fit_travel2.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

crusttrav: crusttrav.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

fit_to_line: fit_to_line.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

evt2reltimes: evt2reltimes.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

evt2relonset: evt2relonset.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

evt2phasetimes: evt2phasetimes.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

relonset2slo: relonset2slo.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

getresid:     getresid.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

floatop:      floatop.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

weather2ascii:     weather2ascii.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

beamloc:     beamloc.o $(LIB_SOURCE)
		$(LINK.c) -o $@ $@.o $(LDFLAGS) $(LIBS)

stddev: 	stddev.o
		$(LINK.c) -o $@ $@.o -lm



clean:
	@rm $(ALL) *.o *%




depend:
	makedepend -s "# DO NOT DELETE" -- $(CFLAGS) -- $(LOCAL_SRCS)


# DO NOT DELETE

archevent_shc.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
archevent_shc.o: /usr/include/sys/_symbol_aliasing.h
archevent_shc.o: /usr/include/sys/_posix_availability.h
archevent_shc.o: /usr/include/Availability.h
archevent_shc.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
archevent_shc.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
archevent_shc.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
archevent_shc.o: /usr/include/secure/_common.h /usr/include/string.h
archevent_shc.o: /usr/include/strings.h /usr/include/secure/_string.h
archevent_shc.o: /usr/local/sh/sh/source/basecnst.h
archevent_shc.o: /usr/local/sh/sh/source/sysbase.h
archevent_shc.o: /usr/local/sh/sh/source/fousrdef.h
archevent_shc.o: /usr/local/sh/sh/source/tcusrdef.h eventdsc.h
autoloc2evt.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
autoloc2evt.o: /usr/include/sys/_symbol_aliasing.h
autoloc2evt.o: /usr/include/sys/_posix_availability.h
autoloc2evt.o: /usr/include/Availability.h
autoloc2evt.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
autoloc2evt.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
autoloc2evt.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
autoloc2evt.o: /usr/include/secure/_common.h /usr/include/string.h
autoloc2evt.o: /usr/include/strings.h /usr/include/secure/_string.h
autoloc2evt.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
autoloc2evt.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
autoloc2evt.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
autoloc2evt.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
autoloc2evt.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
autoloc2evt.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
autoloc2evt.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
autoloc2evt.o: /usr/include/libkern/_OSByteOrder.h
autoloc2evt.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
autoloc2evt.o: /usr/include/machine/types.h /usr/include/i386/types.h
autoloc2evt.o: /usr/local/sh/sh/source/sysbase.h
autoloc2evt.o: /usr/local/sh/sh/source/fousrdef.h
autoloc2evt.o: /usr/local/sh/sh/source/tcusrdef.h eventdsc.h
beamloc.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
beamloc.o: /usr/include/sys/_symbol_aliasing.h
beamloc.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
beamloc.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
beamloc.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
beamloc.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
beamloc.o: /usr/include/secure/_common.h /usr/include/string.h
beamloc.o: /usr/include/strings.h /usr/include/secure/_string.h
beamloc.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
beamloc.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
beamloc.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
beamloc.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
beamloc.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
beamloc.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
beamloc.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
beamloc.o: /usr/include/libkern/_OSByteOrder.h
beamloc.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
beamloc.o: /usr/include/machine/types.h /usr/include/i386/types.h
beamloc.o: /usr/local/sh/sh/source/sysbase.h
beamloc.o: /usr/local/sh/sh/source/fousrdef.h
beamloc.o: /usr/local/sh/sh/source/utusrdef.h
beamloc.o: /usr/local/sh/sh/source/glusrdef.h
beamloc.o: /usr/local/sh/sh/source/earthloc.h
beamloc.o: /usr/local/sh/sh/source/ptusrdef.h
beamloc.o: /usr/local/sh/sh/source/numres.h
butfreq.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
butfreq.o: /usr/include/sys/_symbol_aliasing.h
butfreq.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
butfreq.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
butfreq.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
butfreq.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
butfreq.o: /usr/include/secure/_common.h /usr/include/string.h
butfreq.o: /usr/include/strings.h /usr/include/secure/_string.h
butfreq.o: /usr/include/math.h /usr/include/architecture/i386/math.h
butfreq.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
butfreq.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
butfreq.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
butfreq.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
butfreq.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
butfreq.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
butfreq.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
butfreq.o: /usr/include/libkern/_OSByteOrder.h
butfreq.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
butfreq.o: /usr/include/machine/types.h /usr/include/i386/types.h
butfreq.o: /usr/local/sh/sh/source/sysbase.h
butfreq.o: /usr/local/sh/sh/source/fousrdef.h /usr/local/sh/sh/source/cpar.h
butrec.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
butrec.o: /usr/include/sys/_symbol_aliasing.h
butrec.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
butrec.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
butrec.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
butrec.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
butrec.o: /usr/include/secure/_common.h /usr/include/string.h
butrec.o: /usr/include/strings.h /usr/include/secure/_string.h
butrec.o: /usr/include/math.h /usr/include/architecture/i386/math.h
butrec.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
butrec.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
butrec.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
butrec.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
butrec.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
butrec.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
butrec.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
butrec.o: /usr/include/libkern/_OSByteOrder.h
butrec.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
butrec.o: /usr/include/machine/types.h /usr/include/i386/types.h
butrec.o: /usr/local/sh/sh/source/sysbase.h
butrec.o: /usr/local/sh/sh/source/fousrdef.h /usr/local/sh/sh/source/cpar.h
catflf.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
catflf.o: /usr/include/sys/_symbol_aliasing.h
catflf.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
catflf.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
catflf.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
catflf.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
catflf.o: /usr/include/secure/_common.h /usr/include/string.h
catflf.o: /usr/include/strings.h /usr/include/secure/_string.h
catflf.o: /usr/include/math.h /usr/include/architecture/i386/math.h
catflf.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
catflf.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
catflf.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
catflf.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
catflf.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
catflf.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
catflf.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
catflf.o: /usr/include/libkern/_OSByteOrder.h
catflf.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
catflf.o: /usr/include/machine/types.h /usr/include/i386/types.h
catflf.o: /usr/local/sh/sh/source/sysbase.h
catflf.o: /usr/local/sh/sh/source/fousrdef.h
catflf.o: /usr/local/sh/sh/source/ffusrdef.h
catflf.o: /usr/local/sh/sh/source/shconst.h
catflf.o: /usr/local/sh/sh/source/erusrdef.h
catflf.o: /usr/local/sh/sh/source/globalparams.h
check_evt.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
check_evt.o: /usr/include/sys/_symbol_aliasing.h
check_evt.o: /usr/include/sys/_posix_availability.h
check_evt.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
check_evt.o: /usr/include/_types.h /usr/include/sys/_types.h
check_evt.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
check_evt.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
check_evt.o: /usr/include/string.h /usr/include/strings.h
check_evt.o: /usr/include/secure/_string.h /usr/local/sh/sh/source/basecnst.h
check_evt.o: /usr/local/sh/sh/source/sysbase.h
check_evt.o: /usr/local/sh/sh/source/fousrdef.h eventdsc.h
comment_of_evt.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
comment_of_evt.o: /usr/include/sys/_symbol_aliasing.h
comment_of_evt.o: /usr/include/sys/_posix_availability.h
comment_of_evt.o: /usr/include/Availability.h
comment_of_evt.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
comment_of_evt.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
comment_of_evt.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
comment_of_evt.o: /usr/include/secure/_common.h /usr/include/string.h
comment_of_evt.o: /usr/include/strings.h /usr/include/secure/_string.h
compare_locs.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
compare_locs.o: /usr/include/sys/_symbol_aliasing.h
compare_locs.o: /usr/include/sys/_posix_availability.h
compare_locs.o: /usr/include/Availability.h
compare_locs.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
compare_locs.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
compare_locs.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
compare_locs.o: /usr/include/secure/_common.h /usr/include/string.h
compare_locs.o: /usr/include/strings.h /usr/include/secure/_string.h
compare_locs.o: /usr/local/sh/sh/source/basecnst.h
compare_locs.o: /usr/local/sh/sh/source/sysbase.h
compare_locs.o: /usr/local/sh/sh/source/fousrdef.h
compare_locs.o: /usr/local/sh/sh/source/erusrdef.h
compare_locs.o: /usr/local/sh/sh/source/cpar.h
compare_locs.o: /usr/local/sh/sh/source/tcusrdef.h
compare_locs.o: /usr/local/sh/sh/source/earthloc.h
condense_grfdet.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
condense_grfdet.o: /usr/include/sys/_symbol_aliasing.h
condense_grfdet.o: /usr/include/sys/_posix_availability.h
condense_grfdet.o: /usr/include/Availability.h
condense_grfdet.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
condense_grfdet.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
condense_grfdet.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
condense_grfdet.o: /usr/include/secure/_common.h /usr/include/strings.h
condense_grfdet.o: /usr/include/string.h /usr/include/secure/_string.h
condense_grfdet.o: /usr/local/sh/sh/source/basecnst.h
condense_grfdet.o: /usr/local/sh/sh/source/sysbase.h
condense_grfdet.o: /usr/local/sh/sh/source/fousrdef.h
condense_grfdet.o: /usr/local/sh/sh/source/tcusrdef.h
crusttrav.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
crusttrav.o: /usr/include/sys/_symbol_aliasing.h
crusttrav.o: /usr/include/sys/_posix_availability.h
crusttrav.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
crusttrav.o: /usr/include/_types.h /usr/include/sys/_types.h
crusttrav.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
crusttrav.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
crusttrav.o: /usr/include/string.h /usr/include/strings.h
crusttrav.o: /usr/include/secure/_string.h /usr/include/math.h
crusttrav.o: /usr/include/architecture/i386/math.h
crusttrav.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
crusttrav.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
crusttrav.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
crusttrav.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
crusttrav.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
crusttrav.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
crusttrav.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
crusttrav.o: /usr/include/libkern/_OSByteOrder.h
crusttrav.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
crusttrav.o: /usr/include/machine/types.h /usr/include/i386/types.h
crusttrav.o: /usr/local/sh/sh/source/sysbase.h
crusttrav.o: /usr/local/sh/sh/source/fousrdef.h
crusttrav.o: /usr/local/sh/sh/source/shconst.h /usr/local/sh/sh/source/cpar.h
crusttrav.o: /usr/local/sh/sh/source/tcusrdef.h
crusttrav.o: /usr/local/sh/sh/source/glusrdef.h eventdsc.h
crusttrav.o: /usr/local/sh/sh/source/numres.h
deg2dms.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
deg2dms.o: /usr/include/sys/_symbol_aliasing.h
deg2dms.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
deg2dms.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
deg2dms.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
deg2dms.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
deg2dms.o: /usr/include/secure/_common.h /usr/include/string.h
deg2dms.o: /usr/include/strings.h /usr/include/secure/_string.h
emscev2oed.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
emscev2oed.o: /usr/include/sys/_symbol_aliasing.h
emscev2oed.o: /usr/include/sys/_posix_availability.h
emscev2oed.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
emscev2oed.o: /usr/include/_types.h /usr/include/sys/_types.h
emscev2oed.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
emscev2oed.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
emscev2oed.o: /usr/include/string.h /usr/include/strings.h
emscev2oed.o: /usr/include/secure/_string.h
ev2bull.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
ev2bull.o: /usr/include/sys/_symbol_aliasing.h
ev2bull.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
ev2bull.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
ev2bull.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
ev2bull.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
ev2bull.o: /usr/include/secure/_common.h /usr/include/string.h
ev2bull.o: /usr/include/strings.h /usr/include/secure/_string.h
ev2bull.o: /usr/local/sh/sh/source/basecnst.h
ev2bull.o: /usr/local/sh/sh/source/sysbase.h
ev2bull.o: /usr/local/sh/sh/source/fousrdef.h eventdsc.h evbullet.h
ev2hypo.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
ev2hypo.o: /usr/include/sys/_symbol_aliasing.h
ev2hypo.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
ev2hypo.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
ev2hypo.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
ev2hypo.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
ev2hypo.o: /usr/include/secure/_common.h /usr/include/string.h
ev2hypo.o: /usr/include/strings.h /usr/include/secure/_string.h
ev2hypo.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
ev2hypo.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
ev2hypo.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
ev2hypo.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
ev2hypo.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
ev2hypo.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
ev2hypo.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
ev2hypo.o: /usr/include/libkern/_OSByteOrder.h
ev2hypo.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
ev2hypo.o: /usr/include/machine/types.h /usr/include/i386/types.h
ev2hypo.o: /usr/local/sh/sh/source/sysbase.h
ev2hypo.o: /usr/local/sh/sh/source/fousrdef.h /usr/local/sh/sh/source/cpar.h
ev2hypo.o: eventdsc.h evhypo.h
ev2hypocenter.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
ev2hypocenter.o: /usr/include/sys/_symbol_aliasing.h
ev2hypocenter.o: /usr/include/sys/_posix_availability.h
ev2hypocenter.o: /usr/include/Availability.h
ev2hypocenter.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
ev2hypocenter.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
ev2hypocenter.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
ev2hypocenter.o: /usr/include/secure/_common.h /usr/include/string.h
ev2hypocenter.o: /usr/include/strings.h /usr/include/secure/_string.h
ev2hypocenter.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
ev2hypocenter.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
ev2hypocenter.o: /usr/include/sys/appleapiopts.h
ev2hypocenter.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
ev2hypocenter.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
ev2hypocenter.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
ev2hypocenter.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
ev2hypocenter.o: /usr/include/sys/_endian.h
ev2hypocenter.o: /usr/include/libkern/_OSByteOrder.h
ev2hypocenter.o: /usr/include/libkern/i386/_OSByteOrder.h
ev2hypocenter.o: /usr/include/alloca.h /usr/include/machine/types.h
ev2hypocenter.o: /usr/include/i386/types.h /usr/local/sh/sh/source/sysbase.h
ev2hypocenter.o: /usr/local/sh/sh/source/fousrdef.h
ev2hypocenter.o: /usr/local/sh/sh/source/tcusrdef.h
ev2hypocenter.o: /usr/local/sh/sh/source/cpar.h eventdsc.h
ev2hyposat.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
ev2hyposat.o: /usr/include/sys/_symbol_aliasing.h
ev2hyposat.o: /usr/include/sys/_posix_availability.h
ev2hyposat.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
ev2hyposat.o: /usr/include/_types.h /usr/include/sys/_types.h
ev2hyposat.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
ev2hyposat.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
ev2hyposat.o: /usr/include/string.h /usr/include/strings.h
ev2hyposat.o: /usr/include/secure/_string.h
ev2hyposat.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
ev2hyposat.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
ev2hyposat.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
ev2hyposat.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
ev2hyposat.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
ev2hyposat.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
ev2hyposat.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
ev2hyposat.o: /usr/include/libkern/_OSByteOrder.h
ev2hyposat.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
ev2hyposat.o: /usr/include/machine/types.h /usr/include/i386/types.h
ev2hyposat.o: /usr/local/sh/sh/source/sysbase.h
ev2hyposat.o: /usr/local/sh/sh/source/fousrdef.h
ev2hyposat.o: /usr/local/sh/sh/source/tcusrdef.h
ev2hyposat.o: /usr/local/sh/sh/source/cpar.h eventdsc.h
ev2telex.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
ev2telex.o: /usr/include/sys/_symbol_aliasing.h
ev2telex.o: /usr/include/sys/_posix_availability.h
ev2telex.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
ev2telex.o: /usr/include/_types.h /usr/include/sys/_types.h
ev2telex.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
ev2telex.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
ev2telex.o: /usr/include/string.h /usr/include/strings.h
ev2telex.o: /usr/include/secure/_string.h /usr/local/sh/sh/source/basecnst.h
ev2telex.o: /usr/local/sh/sh/source/sysbase.h
ev2telex.o: /usr/local/sh/sh/source/fousrdef.h eventdsc.h evtelex.h
ev2telex.o: /usr/local/sh/sh/source/cpar.h
ev2view.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
ev2view.o: /usr/include/sys/_symbol_aliasing.h
ev2view.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
ev2view.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
ev2view.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
ev2view.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
ev2view.o: /usr/include/secure/_common.h /usr/include/string.h
ev2view.o: /usr/include/strings.h /usr/include/secure/_string.h
ev2view.o: /usr/local/sh/sh/source/basecnst.h
ev2view.o: /usr/local/sh/sh/source/sysbase.h
ev2view.o: /usr/local/sh/sh/source/fousrdef.h eventdsc.h
ev4hypocenter.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
ev4hypocenter.o: /usr/include/sys/_symbol_aliasing.h
ev4hypocenter.o: /usr/include/sys/_posix_availability.h
ev4hypocenter.o: /usr/include/Availability.h
ev4hypocenter.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
ev4hypocenter.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
ev4hypocenter.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
ev4hypocenter.o: /usr/include/secure/_common.h /usr/include/string.h
ev4hypocenter.o: /usr/include/strings.h /usr/include/secure/_string.h
ev4hypocenter.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
ev4hypocenter.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
ev4hypocenter.o: /usr/include/sys/appleapiopts.h
ev4hypocenter.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
ev4hypocenter.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
ev4hypocenter.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
ev4hypocenter.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
ev4hypocenter.o: /usr/include/sys/_endian.h
ev4hypocenter.o: /usr/include/libkern/_OSByteOrder.h
ev4hypocenter.o: /usr/include/libkern/i386/_OSByteOrder.h
ev4hypocenter.o: /usr/include/alloca.h /usr/include/machine/types.h
ev4hypocenter.o: /usr/include/i386/types.h /usr/local/sh/sh/source/sysbase.h
ev4hypocenter.o: /usr/local/sh/sh/source/fousrdef.h
ev4hypocenter.o: /usr/local/sh/sh/source/tcusrdef.h eventdsc.h
ev4hypou.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
ev4hypou.o: /usr/include/sys/_symbol_aliasing.h
ev4hypou.o: /usr/include/sys/_posix_availability.h
ev4hypou.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
ev4hypou.o: /usr/include/_types.h /usr/include/sys/_types.h
ev4hypou.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
ev4hypou.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
ev4hypou.o: /usr/include/string.h /usr/include/strings.h
ev4hypou.o: /usr/include/secure/_string.h /usr/local/sh/sh/source/basecnst.h
ev4hypou.o: /usr/local/sh/sh/source/sysbase.h
ev4hypou.o: /usr/local/sh/sh/source/fousrdef.h eventdsc.h evhypout.h
evalflf.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
evalflf.o: /usr/include/sys/_symbol_aliasing.h
evalflf.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
evalflf.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
evalflf.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
evalflf.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
evalflf.o: /usr/include/secure/_common.h /usr/include/string.h
evalflf.o: /usr/include/strings.h /usr/include/secure/_string.h
evalflf.o: /usr/include/math.h /usr/include/architecture/i386/math.h
evalflf.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
evalflf.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
evalflf.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
evalflf.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
evalflf.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
evalflf.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
evalflf.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
evalflf.o: /usr/include/libkern/_OSByteOrder.h
evalflf.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
evalflf.o: /usr/include/machine/types.h /usr/include/i386/types.h
evalflf.o: /usr/local/sh/sh/source/sysbase.h
evalflf.o: /usr/local/sh/sh/source/fousrdef.h /usr/local/sh/sh/source/cpar.h
evalflf.o: /usr/local/sh/sh/source/flerrors.h
evalflf.o: /usr/local/sh/sh/source/erusrdef.h
evbullet.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
evbullet.o: /usr/include/sys/_symbol_aliasing.h
evbullet.o: /usr/include/sys/_posix_availability.h
evbullet.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
evbullet.o: /usr/include/_types.h /usr/include/sys/_types.h
evbullet.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
evbullet.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
evbullet.o: /usr/include/string.h /usr/include/strings.h
evbullet.o: /usr/include/secure/_string.h /usr/local/sh/sh/source/basecnst.h
evbullet.o: /usr/local/sh/sh/source/sysbase.h
evbullet.o: /usr/local/sh/sh/source/fousrdef.h
evbullet.o: /usr/local/sh/sh/source/tcusrdef.h
evbullet.o: /usr/local/sh/sh/source/utusrdef.h eventdsc.h evtelex.h
evbullet.o: evbullet.h
evcreate.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
evcreate.o: /usr/include/sys/_symbol_aliasing.h
evcreate.o: /usr/include/sys/_posix_availability.h
evcreate.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
evcreate.o: /usr/include/_types.h /usr/include/sys/_types.h
evcreate.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
evcreate.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
evcreate.o: /usr/include/string.h /usr/include/strings.h
evcreate.o: /usr/include/secure/_string.h /usr/local/sh/sh/source/basecnst.h
evcreate.o: /usr/local/sh/sh/source/sysbase.h
evcreate.o: /usr/local/sh/sh/source/fousrdef.h eventdsc.h
eventdsc.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
eventdsc.o: /usr/include/sys/_symbol_aliasing.h
eventdsc.o: /usr/include/sys/_posix_availability.h
eventdsc.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
eventdsc.o: /usr/include/_types.h /usr/include/sys/_types.h
eventdsc.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
eventdsc.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
eventdsc.o: /usr/include/string.h /usr/include/strings.h
eventdsc.o: /usr/include/secure/_string.h /usr/local/sh/sh/source/basecnst.h
eventdsc.o: /usr/include/stdlib.h /usr/include/sys/wait.h
eventdsc.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
eventdsc.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
eventdsc.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
eventdsc.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
eventdsc.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
eventdsc.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
eventdsc.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
eventdsc.o: /usr/include/machine/types.h /usr/include/i386/types.h
eventdsc.o: /usr/local/sh/sh/source/sysbase.h
eventdsc.o: /usr/local/sh/sh/source/fousrdef.h
eventdsc.o: /usr/local/sh/sh/source/tcusrdef.h eventdsc.h
evhypo.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
evhypo.o: /usr/include/sys/_symbol_aliasing.h
evhypo.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
evhypo.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
evhypo.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
evhypo.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
evhypo.o: /usr/include/secure/_common.h /usr/include/string.h
evhypo.o: /usr/include/strings.h /usr/include/secure/_string.h
evhypo.o: /usr/local/sh/sh/source/basecnst.h
evhypo.o: /usr/local/sh/sh/source/sysbase.h
evhypo.o: /usr/local/sh/sh/source/fousrdef.h
evhypo.o: /usr/local/sh/sh/source/tcusrdef.h eventdsc.h evhypo.h
evhypout.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
evhypout.o: /usr/include/sys/_symbol_aliasing.h
evhypout.o: /usr/include/sys/_posix_availability.h
evhypout.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
evhypout.o: /usr/include/_types.h /usr/include/sys/_types.h
evhypout.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
evhypout.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
evhypout.o: /usr/include/string.h /usr/include/strings.h
evhypout.o: /usr/include/secure/_string.h /usr/local/sh/sh/source/basecnst.h
evhypout.o: /usr/local/sh/sh/source/sysbase.h
evhypout.o: /usr/local/sh/sh/source/fousrdef.h
evhypout.o: /usr/local/sh/sh/source/tcusrdef.h eventdsc.h evhypout.h
evshow.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
evshow.o: /usr/include/sys/_symbol_aliasing.h
evshow.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
evshow.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
evshow.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
evshow.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
evshow.o: /usr/include/secure/_common.h /usr/include/string.h
evshow.o: /usr/include/strings.h /usr/include/secure/_string.h
evshow.o: /usr/local/sh/sh/source/basecnst.h
evshow.o: /usr/local/sh/sh/source/sysbase.h
evshow.o: /usr/local/sh/sh/source/fousrdef.h eventdsc.h
evt2phasetimes.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
evt2phasetimes.o: /usr/include/sys/_symbol_aliasing.h
evt2phasetimes.o: /usr/include/sys/_posix_availability.h
evt2phasetimes.o: /usr/include/Availability.h
evt2phasetimes.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
evt2phasetimes.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
evt2phasetimes.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
evt2phasetimes.o: /usr/include/secure/_common.h /usr/include/string.h
evt2phasetimes.o: /usr/include/strings.h /usr/include/secure/_string.h
evt2phasetimes.o: /usr/include/math.h /usr/include/architecture/i386/math.h
evt2phasetimes.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
evt2phasetimes.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
evt2phasetimes.o: /usr/include/sys/appleapiopts.h
evt2phasetimes.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
evt2phasetimes.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
evt2phasetimes.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
evt2phasetimes.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
evt2phasetimes.o: /usr/include/sys/_endian.h
evt2phasetimes.o: /usr/include/libkern/_OSByteOrder.h
evt2phasetimes.o: /usr/include/libkern/i386/_OSByteOrder.h
evt2phasetimes.o: /usr/include/alloca.h /usr/include/machine/types.h
evt2phasetimes.o: /usr/include/i386/types.h /usr/local/sh/sh/source/sysbase.h
evt2phasetimes.o: /usr/local/sh/sh/source/fousrdef.h
evt2phasetimes.o: /usr/local/sh/sh/source/cpar.h
evt2phasetimes.o: /usr/local/sh/sh/source/earthloc.h
evt2phasetimes.o: /usr/local/sh/sh/source/glusrdef.h
evt2phasetimes.o: /usr/local/sh/sh/source/erusrdef.h
evt2phasetimes.o: /usr/local/sh/sh/source/tcusrdef.h eventdsc.h
evt2relonset.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
evt2relonset.o: /usr/include/sys/_symbol_aliasing.h
evt2relonset.o: /usr/include/sys/_posix_availability.h
evt2relonset.o: /usr/include/Availability.h
evt2relonset.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
evt2relonset.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
evt2relonset.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
evt2relonset.o: /usr/include/secure/_common.h /usr/include/string.h
evt2relonset.o: /usr/include/strings.h /usr/include/secure/_string.h
evt2relonset.o: /usr/local/sh/sh/source/basecnst.h
evt2relonset.o: /usr/local/sh/sh/source/sysbase.h
evt2relonset.o: /usr/local/sh/sh/source/fousrdef.h eventdsc.h
evt2relonset.o: /usr/local/sh/sh/source/tcusrdef.h
evt2reltimes.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
evt2reltimes.o: /usr/include/sys/_symbol_aliasing.h
evt2reltimes.o: /usr/include/sys/_posix_availability.h
evt2reltimes.o: /usr/include/Availability.h
evt2reltimes.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
evt2reltimes.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
evt2reltimes.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
evt2reltimes.o: /usr/include/secure/_common.h /usr/include/string.h
evt2reltimes.o: /usr/include/strings.h /usr/include/secure/_string.h
evt2reltimes.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
evt2reltimes.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
evt2reltimes.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
evt2reltimes.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
evt2reltimes.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
evt2reltimes.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
evt2reltimes.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
evt2reltimes.o: /usr/include/libkern/_OSByteOrder.h
evt2reltimes.o: /usr/include/libkern/i386/_OSByteOrder.h
evt2reltimes.o: /usr/include/alloca.h /usr/include/machine/types.h
evt2reltimes.o: /usr/include/i386/types.h /usr/local/sh/sh/source/sysbase.h
evt2reltimes.o: /usr/local/sh/sh/source/fousrdef.h eventdsc.h
evt2reltimes.o: /usr/local/sh/sh/source/tcusrdef.h
evt_copy_and_split.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
evt_copy_and_split.o: /usr/include/sys/_symbol_aliasing.h
evt_copy_and_split.o: /usr/include/sys/_posix_availability.h
evt_copy_and_split.o: /usr/include/Availability.h
evt_copy_and_split.o: /usr/include/AvailabilityInternal.h
evt_copy_and_split.o: /usr/include/_types.h /usr/include/sys/_types.h
evt_copy_and_split.o: /usr/include/machine/_types.h
evt_copy_and_split.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
evt_copy_and_split.o: /usr/include/secure/_common.h /usr/include/string.h
evt_copy_and_split.o: /usr/include/strings.h /usr/include/secure/_string.h
evt_copy_and_split.o: /usr/local/sh/sh/source/basecnst.h
evt_copy_and_split.o: /usr/local/sh/sh/source/sysbase.h
evt_copy_and_split.o: /usr/local/sh/sh/source/fousrdef.h
evt_copy_and_split.o: /usr/local/sh/sh/source/cpar.h
evt_copy_and_split.o: /usr/local/sh/sh/source/utusrdef.h eventdsc.h
evt_copy_and_split.o: /usr/local/sh/sh/source/erusrdef.h
evt_correct_aniso.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
evt_correct_aniso.o: /usr/include/sys/_symbol_aliasing.h
evt_correct_aniso.o: /usr/include/sys/_posix_availability.h
evt_correct_aniso.o: /usr/include/Availability.h
evt_correct_aniso.o: /usr/include/AvailabilityInternal.h
evt_correct_aniso.o: /usr/include/_types.h /usr/include/sys/_types.h
evt_correct_aniso.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
evt_correct_aniso.o: /usr/include/secure/_stdio.h
evt_correct_aniso.o: /usr/include/secure/_common.h /usr/include/string.h
evt_correct_aniso.o: /usr/include/strings.h /usr/include/secure/_string.h
evt_correct_aniso.o: /usr/include/stdlib.h /usr/include/sys/wait.h
evt_correct_aniso.o: /usr/include/sys/signal.h
evt_correct_aniso.o: /usr/include/sys/appleapiopts.h
evt_correct_aniso.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
evt_correct_aniso.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
evt_correct_aniso.o: /usr/include/machine/_structs.h
evt_correct_aniso.o: /usr/include/sys/resource.h
evt_correct_aniso.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
evt_correct_aniso.o: /usr/include/sys/_endian.h
evt_correct_aniso.o: /usr/include/libkern/_OSByteOrder.h
evt_correct_aniso.o: /usr/include/libkern/i386/_OSByteOrder.h
evt_correct_aniso.o: /usr/include/alloca.h /usr/include/machine/types.h
evt_correct_aniso.o: /usr/include/i386/types.h /usr/include/math.h
evt_correct_aniso.o: /usr/include/architecture/i386/math.h
evt_correct_aniso.o: /usr/local/sh/sh/source/basecnst.h
evt_correct_aniso.o: /usr/local/sh/sh/source/sysbase.h
evt_correct_aniso.o: /usr/local/sh/sh/source/fousrdef.h
evt_correct_aniso.o: /usr/local/sh/sh/source/tcusrdef.h
evt_correct_aniso.o: /usr/local/sh/sh/source/earthloc.h
evt_correct_aniso.o: /usr/local/sh/sh/source/glusrdef.h
evt_correct_aniso.o: /usr/local/sh/sh/source/cpar.h eventdsc.h timelist.h
evt_extract_ms.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
evt_extract_ms.o: /usr/include/sys/_symbol_aliasing.h
evt_extract_ms.o: /usr/include/sys/_posix_availability.h
evt_extract_ms.o: /usr/include/Availability.h
evt_extract_ms.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
evt_extract_ms.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
evt_extract_ms.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
evt_extract_ms.o: /usr/include/secure/_common.h /usr/include/string.h
evt_extract_ms.o: /usr/include/strings.h /usr/include/secure/_string.h
evt_extract_ms.o: /usr/local/sh/sh/source/basecnst.h
evt_extract_ms.o: /usr/local/sh/sh/source/sysbase.h
evt_extract_ms.o: /usr/local/sh/sh/source/fousrdef.h
evt_extract_ms.o: /usr/local/sh/sh/source/tcusrdef.h
evt_extract_ms.o: /usr/local/sh/sh/source/cpar.h eventdsc.h
evt_extract_ms.o: /usr/local/sh/sh/source/erusrdef.h
evt_time_sort.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
evt_time_sort.o: /usr/include/sys/_symbol_aliasing.h
evt_time_sort.o: /usr/include/sys/_posix_availability.h
evt_time_sort.o: /usr/include/Availability.h
evt_time_sort.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
evt_time_sort.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
evt_time_sort.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
evt_time_sort.o: /usr/include/secure/_common.h /usr/include/string.h
evt_time_sort.o: /usr/include/strings.h /usr/include/secure/_string.h
evt_time_sort.o: /usr/local/sh/sh/source/basecnst.h
evt_time_sort.o: /usr/local/sh/sh/source/sysbase.h
evt_time_sort.o: /usr/local/sh/sh/source/fousrdef.h
evt_time_sort.o: /usr/local/sh/sh/source/tcusrdef.h
evt_time_sort.o: /usr/local/sh/sh/source/cpar.h eventdsc.h timelist.h
evtelex.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
evtelex.o: /usr/include/sys/_symbol_aliasing.h
evtelex.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
evtelex.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
evtelex.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
evtelex.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
evtelex.o: /usr/include/secure/_common.h /usr/include/string.h
evtelex.o: /usr/include/strings.h /usr/include/secure/_string.h
evtelex.o: /usr/local/sh/sh/source/basecnst.h
evtelex.o: /usr/local/sh/sh/source/sysbase.h
evtelex.o: /usr/local/sh/sh/source/fousrdef.h
evtelex.o: /usr/local/sh/sh/source/tcusrdef.h
evtelex.o: /usr/local/sh/sh/source/utusrdef.h eventdsc.h evtelex.h
fconcat.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
fconcat.o: /usr/include/sys/_symbol_aliasing.h
fconcat.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
fconcat.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
fconcat.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
fconcat.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
fconcat.o: /usr/include/secure/_common.h /usr/include/string.h
fconcat.o: /usr/include/strings.h /usr/include/secure/_string.h
fconcat.o: /usr/include/math.h /usr/include/architecture/i386/math.h
fconcat.o: /usr/local/sh/sh/source/basecnst.h
fconcat.o: /usr/local/sh/sh/source/sysbase.h
fconcat.o: /usr/local/sh/sh/source/fousrdef.h /usr/local/sh/sh/source/cpar.h
fereg.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
fereg.o: /usr/include/sys/_symbol_aliasing.h
fereg.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
fereg.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
fereg.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
fereg.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
fereg.o: /usr/include/secure/_common.h /usr/include/string.h
fereg.o: /usr/include/strings.h /usr/include/secure/_string.h
fereg.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
fereg.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
fereg.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
fereg.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
fereg.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
fereg.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
fereg.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
fereg.o: /usr/include/libkern/_OSByteOrder.h
fereg.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
fereg.o: /usr/include/machine/types.h /usr/include/i386/types.h
fereg.o: /usr/local/sh/sh/source/sysbase.h /usr/local/sh/sh/source/fousrdef.h
fereg.o: /usr/local/sh/sh/source/cpar.h /usr/local/sh/sh/source/earthloc.h
fereg.o: inpfiles.h
find_next_loc.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
find_next_loc.o: /usr/include/sys/_symbol_aliasing.h
find_next_loc.o: /usr/include/sys/_posix_availability.h
find_next_loc.o: /usr/include/Availability.h
find_next_loc.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
find_next_loc.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
find_next_loc.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
find_next_loc.o: /usr/include/secure/_common.h /usr/include/string.h
find_next_loc.o: /usr/include/strings.h /usr/include/secure/_string.h
find_next_loc.o: /usr/local/sh/sh/source/basecnst.h
find_next_loc.o: /usr/local/sh/sh/source/sysbase.h
find_next_loc.o: /usr/local/sh/sh/source/fousrdef.h
find_next_loc.o: /usr/local/sh/sh/source/utusrdef.h
find_next_loc.o: /usr/local/sh/sh/source/glusrdef.h
find_next_loc.o: /usr/local/sh/sh/source/earthloc.h
find_next_loc.o: /usr/local/sh/sh/source/ptusrdef.h
find_topo30_id.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
find_topo30_id.o: /usr/include/sys/_symbol_aliasing.h
find_topo30_id.o: /usr/include/sys/_posix_availability.h
find_topo30_id.o: /usr/include/Availability.h
find_topo30_id.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
find_topo30_id.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
find_topo30_id.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
find_topo30_id.o: /usr/include/secure/_common.h /usr/include/string.h
find_topo30_id.o: /usr/include/strings.h /usr/include/secure/_string.h
findevnt.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
findevnt.o: /usr/include/sys/_symbol_aliasing.h
findevnt.o: /usr/include/sys/_posix_availability.h
findevnt.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
findevnt.o: /usr/include/_types.h /usr/include/sys/_types.h
findevnt.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
findevnt.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
findevnt.o: /usr/include/string.h /usr/include/strings.h
findevnt.o: /usr/include/secure/_string.h /usr/local/sh/sh/source/basecnst.h
findevnt.o: /usr/include/stdlib.h /usr/include/sys/wait.h
findevnt.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
findevnt.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
findevnt.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
findevnt.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
findevnt.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
findevnt.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
findevnt.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
findevnt.o: /usr/include/machine/types.h /usr/include/i386/types.h
findevnt.o: /usr/local/sh/sh/source/sysbase.h
findevnt.o: /usr/local/sh/sh/source/fousrdef.h /usr/local/sh/sh/source/cpar.h
findevnt.o: /usr/local/sh/sh/source/earthloc.h
findevnt.o: /usr/local/sh/sh/source/tcusrdef.h ../source/ptusrdef.h
findevnt.o: /usr/local/sh/sh/source/utusrdef.h inpfiles.h
fit_to_line.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
fit_to_line.o: /usr/include/sys/_symbol_aliasing.h
fit_to_line.o: /usr/include/sys/_posix_availability.h
fit_to_line.o: /usr/include/Availability.h
fit_to_line.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
fit_to_line.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
fit_to_line.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
fit_to_line.o: /usr/include/secure/_common.h /usr/include/string.h
fit_to_line.o: /usr/include/strings.h /usr/include/secure/_string.h
fit_to_line.o: /usr/include/math.h /usr/include/architecture/i386/math.h
fit_to_line.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
fit_to_line.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
fit_to_line.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
fit_to_line.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
fit_to_line.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
fit_to_line.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
fit_to_line.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
fit_to_line.o: /usr/include/libkern/_OSByteOrder.h
fit_to_line.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
fit_to_line.o: /usr/include/machine/types.h /usr/include/i386/types.h
fit_to_line.o: /usr/local/sh/sh/source/sysbase.h
fit_to_line.o: /usr/local/sh/sh/source/fousrdef.h
fit_to_line.o: /usr/local/sh/sh/source/numres.h
fit_travel.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
fit_travel.o: /usr/include/sys/_symbol_aliasing.h
fit_travel.o: /usr/include/sys/_posix_availability.h
fit_travel.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
fit_travel.o: /usr/include/_types.h /usr/include/sys/_types.h
fit_travel.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
fit_travel.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
fit_travel.o: /usr/include/string.h /usr/include/strings.h
fit_travel.o: /usr/include/secure/_string.h /usr/include/math.h
fit_travel.o: /usr/include/architecture/i386/math.h
fit_travel.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
fit_travel.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
fit_travel.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
fit_travel.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
fit_travel.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
fit_travel.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
fit_travel.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
fit_travel.o: /usr/include/libkern/_OSByteOrder.h
fit_travel.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
fit_travel.o: /usr/include/machine/types.h /usr/include/i386/types.h
fit_travel.o: /usr/local/sh/sh/source/sysbase.h
fit_travel.o: /usr/local/sh/sh/source/fousrdef.h
fit_travel.o: /usr/local/sh/sh/source/cpar.h
fit_travel.o: /usr/local/sh/sh/source/ptusrdef.h
fit_travel.o: /usr/local/sh/sh/source/earthloc.h
fit_travel.o: /usr/local/sh/sh/source/glusrdef.h
fit_travel.o: /usr/local/sh/sh/source/erusrdef.h
fit_travel.o: /usr/local/sh/sh/source/tcusrdef.h
fit_travel2.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
fit_travel2.o: /usr/include/sys/_symbol_aliasing.h
fit_travel2.o: /usr/include/sys/_posix_availability.h
fit_travel2.o: /usr/include/Availability.h
fit_travel2.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
fit_travel2.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
fit_travel2.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
fit_travel2.o: /usr/include/secure/_common.h /usr/include/string.h
fit_travel2.o: /usr/include/strings.h /usr/include/secure/_string.h
fit_travel2.o: /usr/include/math.h /usr/include/architecture/i386/math.h
fit_travel2.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
fit_travel2.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
fit_travel2.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
fit_travel2.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
fit_travel2.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
fit_travel2.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
fit_travel2.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
fit_travel2.o: /usr/include/libkern/_OSByteOrder.h
fit_travel2.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
fit_travel2.o: /usr/include/machine/types.h /usr/include/i386/types.h
fit_travel2.o: /usr/local/sh/sh/source/sysbase.h
fit_travel2.o: /usr/local/sh/sh/source/fousrdef.h
fit_travel2.o: /usr/local/sh/sh/source/cpar.h
fit_travel2.o: /usr/local/sh/sh/source/ptusrdef.h
fit_travel2.o: /usr/local/sh/sh/source/earthloc.h
fit_travel2.o: /usr/local/sh/sh/source/numres.h
fit_travel2.o: /usr/local/sh/sh/source/glusrdef.h
fit_travel2.o: /usr/local/sh/sh/source/erusrdef.h
fit_travel2.o: /usr/local/sh/sh/source/tcusrdef.h
fit_travel2.o: /usr/local/sh/sh/source/globalparams.h
flf2flt.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
flf2flt.o: /usr/include/sys/_symbol_aliasing.h
flf2flt.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
flf2flt.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
flf2flt.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
flf2flt.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
flf2flt.o: /usr/include/secure/_common.h /usr/include/string.h
flf2flt.o: /usr/include/strings.h /usr/include/secure/_string.h
flf2flt.o: /usr/include/math.h /usr/include/architecture/i386/math.h
flf2flt.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
flf2flt.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
flf2flt.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
flf2flt.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
flf2flt.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
flf2flt.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
flf2flt.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
flf2flt.o: /usr/include/libkern/_OSByteOrder.h
flf2flt.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
flf2flt.o: /usr/include/machine/types.h /usr/include/i386/types.h
flf2flt.o: /usr/local/sh/sh/source/sysbase.h
flf2flt.o: /usr/local/sh/sh/source/fousrdef.h /usr/local/sh/sh/source/cpar.h
flf2flt.o: /usr/local/sh/sh/source/flerrors.h
flf2flt.o: /usr/local/sh/sh/source/erusrdef.h
floatop.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
floatop.o: /usr/include/sys/_symbol_aliasing.h
floatop.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
floatop.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
floatop.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
floatop.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
floatop.o: /usr/include/secure/_common.h /usr/include/string.h
floatop.o: /usr/include/strings.h /usr/include/secure/_string.h
floatop.o: /usr/include/math.h /usr/include/architecture/i386/math.h
floatop.o: /usr/local/sh/sh/source/basecnst.h
floatop.o: /usr/local/sh/sh/source/sysbase.h
floatop.o: /usr/local/sh/sh/source/fousrdef.h /usr/local/sh/sh/source/cpar.h
getresid.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
getresid.o: /usr/include/sys/_symbol_aliasing.h
getresid.o: /usr/include/sys/_posix_availability.h
getresid.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
getresid.o: /usr/include/_types.h /usr/include/sys/_types.h
getresid.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
getresid.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
getresid.o: /usr/include/string.h /usr/include/strings.h
getresid.o: /usr/include/secure/_string.h /usr/local/sh/sh/source/basecnst.h
getresid.o: /usr/local/sh/sh/source/sysbase.h
getresid.o: /usr/local/sh/sh/source/fousrdef.h
getresid.o: /usr/local/sh/sh/source/erusrdef.h
getresid.o: /usr/local/sh/sh/source/residual.h
gsetest.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
gsetest.o: /usr/include/sys/_symbol_aliasing.h
gsetest.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
gsetest.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
gsetest.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
gsetest.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
gsetest.o: /usr/include/secure/_common.h /usr/include/string.h
gsetest.o: /usr/include/strings.h /usr/include/secure/_string.h
gsetest.o: /usr/local/sh/sh/source/basecnst.h
gsetest.o: /usr/local/sh/sh/source/sysbase.h
gsetest.o: /usr/local/sh/sh/source/fousrdef.h
gsetest.o: /usr/local/sh/sh/source/foreign/gsedata.h
ht2pz.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
ht2pz.o: /usr/include/sys/_symbol_aliasing.h
ht2pz.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
ht2pz.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
ht2pz.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
ht2pz.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
ht2pz.o: /usr/include/secure/_common.h /usr/include/string.h
ht2pz.o: /usr/include/strings.h /usr/include/secure/_string.h
ht2pz.o: /usr/include/math.h /usr/include/architecture/i386/math.h
hypo_stlist.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
hypo_stlist.o: /usr/include/sys/_symbol_aliasing.h
hypo_stlist.o: /usr/include/sys/_posix_availability.h
hypo_stlist.o: /usr/include/Availability.h
hypo_stlist.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
hypo_stlist.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
hypo_stlist.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
hypo_stlist.o: /usr/include/secure/_common.h /usr/include/string.h
hypo_stlist.o: /usr/include/strings.h /usr/include/secure/_string.h
hypo_stlist.o: /usr/include/math.h /usr/include/architecture/i386/math.h
hypo_stlist.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
hypo_stlist.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
hypo_stlist.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
hypo_stlist.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
hypo_stlist.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
hypo_stlist.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
hypo_stlist.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
hypo_stlist.o: /usr/include/libkern/_OSByteOrder.h
hypo_stlist.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
hypo_stlist.o: /usr/include/machine/types.h /usr/include/i386/types.h
hypo_stlist.o: /usr/local/sh/sh/source/sysbase.h
hypo_stlist.o: /usr/local/sh/sh/source/fousrdef.h
hypo_stlist.o: /usr/local/sh/sh/source/glusrdef.h
hypo_stlist.o: /usr/local/sh/sh/source/cpar.h
hypo_stlist.o: /usr/local/sh/sh/source/utusrdef.h inpfiles.h
lineparse.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
lineparse.o: /usr/include/sys/_symbol_aliasing.h
lineparse.o: /usr/include/sys/_posix_availability.h
lineparse.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
lineparse.o: /usr/include/_types.h /usr/include/sys/_types.h
lineparse.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
lineparse.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
lineparse.o: /usr/include/string.h /usr/include/strings.h
lineparse.o: /usr/include/secure/_string.h /usr/local/sh/sh/source/basecnst.h
lineparse.o: /usr/local/sh/sh/source/sysbase.h
lineparse.o: /usr/local/sh/sh/source/fousrdef.h lineparse.h
locadd.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
locadd.o: /usr/include/sys/_symbol_aliasing.h
locadd.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
locadd.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
locadd.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
locadd.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
locadd.o: /usr/include/secure/_common.h /usr/include/string.h
locadd.o: /usr/include/strings.h /usr/include/secure/_string.h
locadd.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
locadd.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
locadd.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
locadd.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
locadd.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
locadd.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
locadd.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
locadd.o: /usr/include/libkern/_OSByteOrder.h
locadd.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
locadd.o: /usr/include/machine/types.h /usr/include/i386/types.h
locadd.o: /usr/local/sh/sh/source/sysbase.h
locadd.o: /usr/local/sh/sh/source/fousrdef.h /usr/local/sh/sh/source/cpar.h
locadd.o: /usr/local/sh/sh/source/utusrdef.h
locadd.o: /usr/local/sh/sh/source/glusrdef.h
locadd.o: /usr/local/sh/sh/source/earthloc.h inpfiles.h
locdiff.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
locdiff.o: /usr/include/sys/_symbol_aliasing.h
locdiff.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
locdiff.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
locdiff.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
locdiff.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
locdiff.o: /usr/include/secure/_common.h /usr/include/string.h
locdiff.o: /usr/include/strings.h /usr/include/secure/_string.h
locdiff.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
locdiff.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
locdiff.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
locdiff.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
locdiff.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
locdiff.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
locdiff.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
locdiff.o: /usr/include/libkern/_OSByteOrder.h
locdiff.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
locdiff.o: /usr/include/machine/types.h /usr/include/i386/types.h
locdiff.o: /usr/local/sh/sh/source/sysbase.h
locdiff.o: /usr/local/sh/sh/source/fousrdef.h /usr/local/sh/sh/source/cpar.h
locdiff.o: /usr/local/sh/sh/source/utusrdef.h
locdiff.o: /usr/local/sh/sh/source/glusrdef.h
locdiff.o: /usr/local/sh/sh/source/earthloc.h
locdiff.o: /usr/local/sh/sh/source/ptusrdef.h inpfiles.h
logseries.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
logseries.o: /usr/include/sys/_symbol_aliasing.h
logseries.o: /usr/include/sys/_posix_availability.h
logseries.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
logseries.o: /usr/include/_types.h /usr/include/sys/_types.h
logseries.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
logseries.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
logseries.o: /usr/include/string.h /usr/include/strings.h
logseries.o: /usr/include/secure/_string.h
lynx_reformat.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
lynx_reformat.o: /usr/include/sys/_symbol_aliasing.h
lynx_reformat.o: /usr/include/sys/_posix_availability.h
lynx_reformat.o: /usr/include/Availability.h
lynx_reformat.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
lynx_reformat.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
lynx_reformat.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
lynx_reformat.o: /usr/include/secure/_common.h /usr/include/string.h
lynx_reformat.o: /usr/include/strings.h /usr/include/secure/_string.h
onset_convert.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
onset_convert.o: /usr/include/sys/_symbol_aliasing.h
onset_convert.o: /usr/include/sys/_posix_availability.h
onset_convert.o: /usr/include/Availability.h
onset_convert.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
onset_convert.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
onset_convert.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
onset_convert.o: /usr/include/secure/_common.h /usr/include/string.h
onset_convert.o: /usr/include/strings.h /usr/include/secure/_string.h
onset_convert.o: /usr/local/sh/sh/source/basecnst.h
onset_convert.o: /usr/local/sh/sh/source/sysbase.h
onset_convert.o: /usr/local/sh/sh/source/fousrdef.h
onset_convert.o: /usr/local/sh/sh/source/tcusrdef.h
q2gse.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
q2gse.o: /usr/include/sys/_symbol_aliasing.h
q2gse.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
q2gse.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
q2gse.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
q2gse.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
q2gse.o: /usr/include/secure/_common.h /usr/include/string.h
q2gse.o: /usr/include/strings.h /usr/include/secure/_string.h
q2gse.o: /usr/include/math.h /usr/include/architecture/i386/math.h
q2gse.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
q2gse.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
q2gse.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
q2gse.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
q2gse.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
q2gse.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
q2gse.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
q2gse.o: /usr/include/libkern/_OSByteOrder.h
q2gse.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
q2gse.o: /usr/include/machine/types.h /usr/include/i386/types.h
q2gse.o: /usr/local/sh/sh/source/sysbase.h /usr/local/sh/sh/source/fousrdef.h
q2gse.o: /usr/local/sh/sh/source/cpar.h /usr/local/sh/sh/source/qfusrdef.h
q2gse.o: /usr/local/sh/sh/source/qferrors.h
q2gse.o: /usr/local/sh/sh/source/dfqentry.h
q2gse.o: /usr/local/sh/sh/source/tcusrdef.h
q2gse.o: /usr/local/sh/sh/source/erusrdef.h
q2gse.o: /usr/local/sh/sh/source/glusrdef.h inpfiles.h
qed_ms_make_evt.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
qed_ms_make_evt.o: /usr/include/sys/_symbol_aliasing.h
qed_ms_make_evt.o: /usr/include/sys/_posix_availability.h
qed_ms_make_evt.o: /usr/include/Availability.h
qed_ms_make_evt.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
qed_ms_make_evt.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
qed_ms_make_evt.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
qed_ms_make_evt.o: /usr/include/secure/_common.h /usr/include/string.h
qed_ms_make_evt.o: /usr/include/strings.h /usr/include/secure/_string.h
qed_ms_make_evt.o: /usr/local/sh/sh/source/basecnst.h
qed_ms_make_evt.o: /usr/local/sh/sh/source/sysbase.h
qed_ms_make_evt.o: /usr/local/sh/sh/source/fousrdef.h eventdsc.h
qed_ms_make_evt.o: /usr/local/sh/sh/source/tcusrdef.h
reformat_hypoe_out.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
reformat_hypoe_out.o: /usr/include/sys/_symbol_aliasing.h
reformat_hypoe_out.o: /usr/include/sys/_posix_availability.h
reformat_hypoe_out.o: /usr/include/Availability.h
reformat_hypoe_out.o: /usr/include/AvailabilityInternal.h
reformat_hypoe_out.o: /usr/include/_types.h /usr/include/sys/_types.h
reformat_hypoe_out.o: /usr/include/machine/_types.h
reformat_hypoe_out.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
reformat_hypoe_out.o: /usr/include/secure/_common.h /usr/include/string.h
reformat_hypoe_out.o: /usr/include/strings.h /usr/include/secure/_string.h
reformat_locsat_out.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
reformat_locsat_out.o: /usr/include/sys/_symbol_aliasing.h
reformat_locsat_out.o: /usr/include/sys/_posix_availability.h
reformat_locsat_out.o: /usr/include/Availability.h
reformat_locsat_out.o: /usr/include/AvailabilityInternal.h
reformat_locsat_out.o: /usr/include/_types.h /usr/include/sys/_types.h
reformat_locsat_out.o: /usr/include/machine/_types.h
reformat_locsat_out.o: /usr/include/i386/_types.h
reformat_locsat_out.o: /usr/include/secure/_stdio.h
reformat_locsat_out.o: /usr/include/secure/_common.h /usr/include/string.h
reformat_locsat_out.o: /usr/include/strings.h /usr/include/secure/_string.h
reformat_locsat_out.o: /usr/local/sh/sh/source/basecnst.h
reformat_locsat_out.o: /usr/local/sh/sh/source/sysbase.h
reformat_locsat_out.o: /usr/local/sh/sh/source/fousrdef.h
reformat_locsat_out.o: /usr/local/sh/sh/source/tcusrdef.h
relonset2slo.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
relonset2slo.o: /usr/include/sys/_symbol_aliasing.h
relonset2slo.o: /usr/include/sys/_posix_availability.h
relonset2slo.o: /usr/include/Availability.h
relonset2slo.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
relonset2slo.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
relonset2slo.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
relonset2slo.o: /usr/include/secure/_common.h /usr/include/string.h
relonset2slo.o: /usr/include/strings.h /usr/include/secure/_string.h
relonset2slo.o: /usr/include/math.h /usr/include/architecture/i386/math.h
relonset2slo.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
relonset2slo.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
relonset2slo.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
relonset2slo.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
relonset2slo.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
relonset2slo.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
relonset2slo.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
relonset2slo.o: /usr/include/libkern/_OSByteOrder.h
relonset2slo.o: /usr/include/libkern/i386/_OSByteOrder.h
relonset2slo.o: /usr/include/alloca.h /usr/include/machine/types.h
relonset2slo.o: /usr/include/i386/types.h /usr/local/sh/sh/source/sysbase.h
relonset2slo.o: /usr/local/sh/sh/source/fousrdef.h
relonset2slo.o: /usr/local/sh/sh/source/cpar.h
relonset2slo.o: /usr/local/sh/sh/source/erusrdef.h
relonset2slo.o: /usr/local/sh/sh/source/glusrdef.h
relonset2slo.o: /usr/local/sh/sh/source/earthloc.h
relonset2slo.o: /usr/local/sh/sh/source/fctxmt.h
relonset2slo.o: /usr/local/sh/sh/source/shconst.h
restfreq.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
restfreq.o: /usr/include/sys/_symbol_aliasing.h
restfreq.o: /usr/include/sys/_posix_availability.h
restfreq.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
restfreq.o: /usr/include/_types.h /usr/include/sys/_types.h
restfreq.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
restfreq.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
restfreq.o: /usr/include/string.h /usr/include/strings.h
restfreq.o: /usr/include/secure/_string.h /usr/include/math.h
restfreq.o: /usr/include/architecture/i386/math.h
restfreq.o: /usr/local/sh/sh/source/basecnst.h
restfreq.o: /usr/local/sh/sh/source/sysbase.h
restfreq.o: /usr/local/sh/sh/source/fousrdef.h /usr/include/stdlib.h
restfreq.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
restfreq.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
restfreq.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
restfreq.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
restfreq.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
restfreq.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
restfreq.o: /usr/include/libkern/_OSByteOrder.h
restfreq.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
restfreq.o: /usr/include/machine/types.h /usr/include/i386/types.h
restfreq.o: /usr/local/sh/sh/source/cpar.h
restrec.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
restrec.o: /usr/include/sys/_symbol_aliasing.h
restrec.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
restrec.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
restrec.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
restrec.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
restrec.o: /usr/include/secure/_common.h /usr/include/string.h
restrec.o: /usr/include/strings.h /usr/include/secure/_string.h
restrec.o: /usr/include/math.h /usr/include/architecture/i386/math.h
restrec.o: /usr/local/sh/sh/source/basecnst.h
restrec.o: /usr/local/sh/sh/source/sysbase.h
restrec.o: /usr/local/sh/sh/source/fousrdef.h
seisan2gse.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
seisan2gse.o: /usr/include/sys/_symbol_aliasing.h
seisan2gse.o: /usr/include/sys/_posix_availability.h
seisan2gse.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
seisan2gse.o: /usr/include/_types.h /usr/include/sys/_types.h
seisan2gse.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
seisan2gse.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
seisan2gse.o: /usr/include/string.h /usr/include/strings.h
seisan2gse.o: /usr/include/secure/_string.h
simfreq.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
simfreq.o: /usr/include/sys/_symbol_aliasing.h
simfreq.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
simfreq.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
simfreq.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
simfreq.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
simfreq.o: /usr/include/secure/_common.h /usr/include/string.h
simfreq.o: /usr/include/strings.h /usr/include/secure/_string.h
simfreq.o: /usr/include/math.h /usr/include/architecture/i386/math.h
simfreq.o: /usr/local/sh/sh/source/basecnst.h
simfreq.o: /usr/local/sh/sh/source/sysbase.h
simfreq.o: /usr/local/sh/sh/source/fousrdef.h /usr/local/sh/sh/source/cpar.h
simrec.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
simrec.o: /usr/include/sys/_symbol_aliasing.h
simrec.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
simrec.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
simrec.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
simrec.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
simrec.o: /usr/include/secure/_common.h /usr/include/string.h
simrec.o: /usr/include/strings.h /usr/include/secure/_string.h
simrec.o: /usr/include/math.h /usr/include/architecture/i386/math.h
simrec.o: /usr/local/sh/sh/source/basecnst.h
simrec.o: /usr/local/sh/sh/source/sysbase.h
simrec.o: /usr/local/sh/sh/source/fousrdef.h
slo2distance.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
slo2distance.o: /usr/include/sys/_symbol_aliasing.h
slo2distance.o: /usr/include/sys/_posix_availability.h
slo2distance.o: /usr/include/Availability.h
slo2distance.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
slo2distance.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
slo2distance.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
slo2distance.o: /usr/include/secure/_common.h /usr/include/string.h
slo2distance.o: /usr/include/strings.h /usr/include/secure/_string.h
slo2distance.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
slo2distance.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
slo2distance.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
slo2distance.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
slo2distance.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
slo2distance.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
slo2distance.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
slo2distance.o: /usr/include/libkern/_OSByteOrder.h
slo2distance.o: /usr/include/libkern/i386/_OSByteOrder.h
slo2distance.o: /usr/include/alloca.h /usr/include/machine/types.h
slo2distance.o: /usr/include/i386/types.h /usr/local/sh/sh/source/sysbase.h
slo2distance.o: /usr/local/sh/sh/source/fousrdef.h
slo2distance.o: /usr/local/sh/sh/source/ptusrdef.h
slo2distance.o: /usr/local/sh/sh/source/erusrdef.h
slo2distance.o: /usr/local/sh/sh/source/cpar.h
statinf.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
statinf.o: /usr/include/sys/_symbol_aliasing.h
statinf.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
statinf.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
statinf.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
statinf.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
statinf.o: /usr/include/secure/_common.h /usr/include/string.h
statinf.o: /usr/include/strings.h /usr/include/secure/_string.h
statinf.o: /usr/include/math.h /usr/include/architecture/i386/math.h
statinf.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
statinf.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
statinf.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
statinf.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
statinf.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
statinf.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
statinf.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
statinf.o: /usr/include/libkern/_OSByteOrder.h
statinf.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
statinf.o: /usr/include/machine/types.h /usr/include/i386/types.h
statinf.o: /usr/local/sh/sh/source/sysbase.h
statinf.o: /usr/local/sh/sh/source/fousrdef.h /usr/local/sh/sh/source/cpar.h
statinf.o: /usr/local/sh/sh/source/glusrdef.h
stddev.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
stddev.o: /usr/include/sys/_symbol_aliasing.h
stddev.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
stddev.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
stddev.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
stddev.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
stddev.o: /usr/include/secure/_common.h /usr/include/string.h
stddev.o: /usr/include/strings.h /usr/include/secure/_string.h
stddev.o: /usr/include/math.h /usr/include/architecture/i386/math.h
stringop.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
stringop.o: /usr/include/sys/_symbol_aliasing.h
stringop.o: /usr/include/sys/_posix_availability.h
stringop.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
stringop.o: /usr/include/_types.h /usr/include/sys/_types.h
stringop.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
stringop.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
stringop.o: /usr/include/string.h /usr/include/strings.h
stringop.o: /usr/include/secure/_string.h
timelist.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
timelist.o: /usr/include/sys/_symbol_aliasing.h
timelist.o: /usr/include/sys/_posix_availability.h
timelist.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
timelist.o: /usr/include/_types.h /usr/include/sys/_types.h
timelist.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
timelist.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
timelist.o: /usr/include/string.h /usr/include/strings.h
timelist.o: /usr/include/secure/_string.h /usr/local/sh/sh/source/basecnst.h
timelist.o: /usr/include/stdlib.h /usr/include/sys/wait.h
timelist.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
timelist.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
timelist.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
timelist.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
timelist.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
timelist.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
timelist.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
timelist.o: /usr/include/machine/types.h /usr/include/i386/types.h
timelist.o: /usr/local/sh/sh/source/sysbase.h
timelist.o: /usr/local/sh/sh/source/fousrdef.h timelist.h
timelist.o: /usr/local/sh/sh/source/tcusrdef.h
timename.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
timename.o: /usr/include/sys/_symbol_aliasing.h
timename.o: /usr/include/sys/_posix_availability.h
timename.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
timename.o: /usr/include/_types.h /usr/include/sys/_types.h
timename.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
timename.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
timename.o: /usr/include/string.h /usr/include/strings.h
timename.o: /usr/include/secure/_string.h /usr/local/sh/sh/source/basecnst.h
timename.o: /usr/local/sh/sh/source/sysbase.h
timename.o: /usr/local/sh/sh/source/fousrdef.h
timename.o: /usr/local/sh/sh/source/tcusrdef.h /usr/local/sh/sh/source/cpar.h
traveltime.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
traveltime.o: /usr/include/sys/_symbol_aliasing.h
traveltime.o: /usr/include/sys/_posix_availability.h
traveltime.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
traveltime.o: /usr/include/_types.h /usr/include/sys/_types.h
traveltime.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
traveltime.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
traveltime.o: /usr/include/string.h /usr/include/strings.h
traveltime.o: /usr/include/secure/_string.h
traveltime.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
traveltime.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
traveltime.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
traveltime.o: /usr/include/i386/signal.h /usr/include/i386/_structs.h
traveltime.o: /usr/include/sys/_structs.h /usr/include/machine/_structs.h
traveltime.o: /usr/include/sys/resource.h /usr/include/machine/endian.h
traveltime.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
traveltime.o: /usr/include/libkern/_OSByteOrder.h
traveltime.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
traveltime.o: /usr/include/machine/types.h /usr/include/i386/types.h
traveltime.o: /usr/local/sh/sh/source/sysbase.h
traveltime.o: /usr/local/sh/sh/source/fousrdef.h
traveltime.o: /usr/local/sh/sh/source/ptusrdef.h
traveltime.o: /usr/local/sh/sh/source/erusrdef.h
traveltime.o: /usr/local/sh/sh/source/cpar.h
vrs_read_diff.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
vrs_read_diff.o: /usr/include/sys/_symbol_aliasing.h
vrs_read_diff.o: /usr/include/sys/_posix_availability.h
vrs_read_diff.o: /usr/include/Availability.h
vrs_read_diff.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
vrs_read_diff.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
vrs_read_diff.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
vrs_read_diff.o: /usr/include/secure/_common.h /usr/include/string.h
vrs_read_diff.o: /usr/include/strings.h /usr/include/secure/_string.h
vrs_read_diff.o: /usr/local/sh/sh/source/basecnst.h
vrs_read_diff.o: /usr/local/sh/sh/source/sysbase.h
vrs_read_diff.o: /usr/local/sh/sh/source/fousrdef.h
weather2ascii.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
weather2ascii.o: /usr/include/sys/_symbol_aliasing.h
weather2ascii.o: /usr/include/sys/_posix_availability.h
weather2ascii.o: /usr/include/Availability.h
weather2ascii.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
weather2ascii.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
weather2ascii.o: /usr/include/i386/_types.h /usr/include/secure/_stdio.h
weather2ascii.o: /usr/include/secure/_common.h /usr/include/string.h
weather2ascii.o: /usr/include/strings.h /usr/include/secure/_string.h
weather2ascii.o: /usr/include/math.h /usr/include/architecture/i386/math.h
weather2ascii.o: /usr/local/sh/sh/source/basecnst.h /usr/include/stdlib.h
weather2ascii.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
weather2ascii.o: /usr/include/sys/appleapiopts.h
weather2ascii.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
weather2ascii.o: /usr/include/i386/_structs.h /usr/include/sys/_structs.h
weather2ascii.o: /usr/include/machine/_structs.h /usr/include/sys/resource.h
weather2ascii.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
weather2ascii.o: /usr/include/sys/_endian.h
weather2ascii.o: /usr/include/libkern/_OSByteOrder.h
weather2ascii.o: /usr/include/libkern/i386/_OSByteOrder.h
weather2ascii.o: /usr/include/alloca.h /usr/include/machine/types.h
weather2ascii.o: /usr/include/i386/types.h /usr/local/sh/sh/source/sysbase.h
weather2ascii.o: /usr/local/sh/sh/source/fousrdef.h
weather2ascii.o: /usr/local/sh/sh/source/cpar.h
weather2ascii.o: /usr/local/sh/sh/source/erusrdef.h
weather2ascii.o: /usr/local/sh/sh/source/tcusrdef.h
