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

archevent_shc.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
archevent_shc.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
archevent_shc.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
archevent_shc.o: /usr/include/stdio_impl.h /usr/include/string.h
archevent_shc.o: /usr/include/iso/string_iso.h
archevent_shc.o: /programs/sol2/sh/source/basecnst.h
archevent_shc.o: /programs/sol2/sh/source/sysbase.h
archevent_shc.o: /programs/sol2/sh/source/fousrdef.h
archevent_shc.o: /programs/sol2/sh/source/tcusrdef.h eventdsc.h
autoloc2evt.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
autoloc2evt.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
autoloc2evt.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
autoloc2evt.o: /usr/include/stdio_impl.h /usr/include/string.h
autoloc2evt.o: /usr/include/iso/string_iso.h
autoloc2evt.o: /programs/sol2/sh/source/basecnst.h /usr/include/stdlib.h
autoloc2evt.o: /usr/include/iso/stdlib_iso.h
autoloc2evt.o: /programs/sol2/sh/source/sysbase.h
autoloc2evt.o: /programs/sol2/sh/source/fousrdef.h
autoloc2evt.o: /programs/sol2/sh/source/tcusrdef.h eventdsc.h
beamloc.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
beamloc.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
beamloc.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
beamloc.o: /usr/include/stdio_impl.h /usr/include/string.h
beamloc.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
beamloc.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
beamloc.o: /programs/sol2/sh/source/sysbase.h
beamloc.o: /programs/sol2/sh/source/fousrdef.h
beamloc.o: /programs/sol2/sh/source/utusrdef.h
beamloc.o: /programs/sol2/sh/source/glusrdef.h
beamloc.o: /programs/sol2/sh/source/earthloc.h
beamloc.o: /programs/sol2/sh/source/ptusrdef.h
beamloc.o: /programs/sol2/sh/source/numres.h
butfreq.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
butfreq.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
butfreq.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
butfreq.o: /usr/include/stdio_impl.h /usr/include/string.h
butfreq.o: /usr/include/iso/string_iso.h /usr/include/math.h
butfreq.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
butfreq.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
butfreq.o: /programs/sol2/sh/source/sysbase.h
butfreq.o: /programs/sol2/sh/source/fousrdef.h
butfreq.o: /programs/sol2/sh/source/cpar.h
butrec.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
butrec.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
butrec.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
butrec.o: /usr/include/stdio_impl.h /usr/include/string.h
butrec.o: /usr/include/iso/string_iso.h /usr/include/math.h
butrec.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
butrec.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
butrec.o: /programs/sol2/sh/source/sysbase.h
butrec.o: /programs/sol2/sh/source/fousrdef.h /programs/sol2/sh/source/cpar.h
catflf.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
catflf.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
catflf.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
catflf.o: /usr/include/stdio_impl.h /usr/include/string.h
catflf.o: /usr/include/iso/string_iso.h /usr/include/math.h
catflf.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
catflf.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
catflf.o: /programs/sol2/sh/source/sysbase.h
catflf.o: /programs/sol2/sh/source/fousrdef.h
catflf.o: /programs/sol2/sh/source/ffusrdef.h
catflf.o: /programs/sol2/sh/source/shconst.h
catflf.o: /programs/sol2/sh/source/erusrdef.h
check_evt.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
check_evt.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
check_evt.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
check_evt.o: /usr/include/stdio_impl.h /usr/include/string.h
check_evt.o: /usr/include/iso/string_iso.h
check_evt.o: /programs/sol2/sh/source/basecnst.h
check_evt.o: /programs/sol2/sh/source/sysbase.h
check_evt.o: /programs/sol2/sh/source/fousrdef.h eventdsc.h
comment_of_evt.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
comment_of_evt.o: /usr/include/sys/feature_tests.h
comment_of_evt.o: /usr/include/sys/isa_defs.h /usr/include/sys/va_list.h
comment_of_evt.o: /usr/include/stdio_tag.h /usr/include/stdio_impl.h
comment_of_evt.o: /usr/include/string.h /usr/include/iso/string_iso.h
compare_locs.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
compare_locs.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
compare_locs.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
compare_locs.o: /usr/include/stdio_impl.h /usr/include/string.h
compare_locs.o: /usr/include/iso/string_iso.h
compare_locs.o: /programs/sol2/sh/source/basecnst.h
compare_locs.o: /programs/sol2/sh/source/sysbase.h
compare_locs.o: /programs/sol2/sh/source/fousrdef.h
compare_locs.o: /programs/sol2/sh/source/erusrdef.h
compare_locs.o: /programs/sol2/sh/source/cpar.h
compare_locs.o: /programs/sol2/sh/source/tcusrdef.h
compare_locs.o: /programs/sol2/sh/source/earthloc.h
condense_grfdet.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
condense_grfdet.o: /usr/include/sys/feature_tests.h
condense_grfdet.o: /usr/include/sys/isa_defs.h /usr/include/sys/va_list.h
condense_grfdet.o: /usr/include/stdio_tag.h /usr/include/stdio_impl.h
condense_grfdet.o: /usr/include/strings.h /usr/include/sys/types.h
condense_grfdet.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
condense_grfdet.o: /usr/include/sys/select.h /usr/include/sys/time.h
condense_grfdet.o: /usr/include/sys/time.h /usr/include/string.h
condense_grfdet.o: /usr/include/iso/string_iso.h
condense_grfdet.o: /programs/sol2/sh/source/basecnst.h
condense_grfdet.o: /programs/sol2/sh/source/sysbase.h
condense_grfdet.o: /programs/sol2/sh/source/fousrdef.h
condense_grfdet.o: /programs/sol2/sh/source/tcusrdef.h
crusttrav.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
crusttrav.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
crusttrav.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
crusttrav.o: /usr/include/stdio_impl.h /usr/include/string.h
crusttrav.o: /usr/include/iso/string_iso.h /usr/include/math.h
crusttrav.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
crusttrav.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
crusttrav.o: /programs/sol2/sh/source/sysbase.h
crusttrav.o: /programs/sol2/sh/source/fousrdef.h
crusttrav.o: /programs/sol2/sh/source/shconst.h
crusttrav.o: /programs/sol2/sh/source/cpar.h
crusttrav.o: /programs/sol2/sh/source/tcusrdef.h
crusttrav.o: /programs/sol2/sh/source/glusrdef.h eventdsc.h
crusttrav.o: /programs/sol2/sh/source/numres.h
deg2dms.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
deg2dms.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
deg2dms.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
deg2dms.o: /usr/include/stdio_impl.h /usr/include/string.h
deg2dms.o: /usr/include/iso/string_iso.h
emscev2oed.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
emscev2oed.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
emscev2oed.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
emscev2oed.o: /usr/include/stdio_impl.h /usr/include/string.h
emscev2oed.o: /usr/include/iso/string_iso.h
ev2bull.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
ev2bull.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
ev2bull.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
ev2bull.o: /usr/include/stdio_impl.h /usr/include/string.h
ev2bull.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
ev2bull.o: /programs/sol2/sh/source/sysbase.h
ev2bull.o: /programs/sol2/sh/source/fousrdef.h eventdsc.h evbullet.h
ev2hypo.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
ev2hypo.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
ev2hypo.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
ev2hypo.o: /usr/include/stdio_impl.h /usr/include/string.h
ev2hypo.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
ev2hypo.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
ev2hypo.o: /programs/sol2/sh/source/sysbase.h
ev2hypo.o: /programs/sol2/sh/source/fousrdef.h
ev2hypo.o: /programs/sol2/sh/source/cpar.h eventdsc.h evhypo.h
ev2hypocenter.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
ev2hypocenter.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
ev2hypocenter.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
ev2hypocenter.o: /usr/include/stdio_impl.h /usr/include/string.h
ev2hypocenter.o: /usr/include/iso/string_iso.h
ev2hypocenter.o: /programs/sol2/sh/source/basecnst.h /usr/include/stdlib.h
ev2hypocenter.o: /usr/include/iso/stdlib_iso.h
ev2hypocenter.o: /programs/sol2/sh/source/sysbase.h
ev2hypocenter.o: /programs/sol2/sh/source/fousrdef.h
ev2hypocenter.o: /programs/sol2/sh/source/tcusrdef.h
ev2hypocenter.o: /programs/sol2/sh/source/cpar.h eventdsc.h
ev2hyposat.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
ev2hyposat.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
ev2hyposat.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
ev2hyposat.o: /usr/include/stdio_impl.h /usr/include/string.h
ev2hyposat.o: /usr/include/iso/string_iso.h
ev2hyposat.o: /programs/sol2/sh/source/basecnst.h /usr/include/stdlib.h
ev2hyposat.o: /usr/include/iso/stdlib_iso.h
ev2hyposat.o: /programs/sol2/sh/source/sysbase.h
ev2hyposat.o: /programs/sol2/sh/source/fousrdef.h
ev2hyposat.o: /programs/sol2/sh/source/tcusrdef.h
ev2hyposat.o: /programs/sol2/sh/source/cpar.h eventdsc.h
ev2telex.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
ev2telex.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
ev2telex.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
ev2telex.o: /usr/include/stdio_impl.h /usr/include/string.h
ev2telex.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
ev2telex.o: /programs/sol2/sh/source/sysbase.h
ev2telex.o: /programs/sol2/sh/source/fousrdef.h eventdsc.h evtelex.h
ev2telex.o: /programs/sol2/sh/source/cpar.h
ev2view.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
ev2view.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
ev2view.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
ev2view.o: /usr/include/stdio_impl.h /usr/include/string.h
ev2view.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
ev2view.o: /programs/sol2/sh/source/sysbase.h
ev2view.o: /programs/sol2/sh/source/fousrdef.h eventdsc.h
ev4hypocenter.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
ev4hypocenter.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
ev4hypocenter.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
ev4hypocenter.o: /usr/include/stdio_impl.h /usr/include/string.h
ev4hypocenter.o: /usr/include/iso/string_iso.h
ev4hypocenter.o: /programs/sol2/sh/source/basecnst.h /usr/include/stdlib.h
ev4hypocenter.o: /usr/include/iso/stdlib_iso.h
ev4hypocenter.o: /programs/sol2/sh/source/sysbase.h
ev4hypocenter.o: /programs/sol2/sh/source/fousrdef.h
ev4hypocenter.o: /programs/sol2/sh/source/tcusrdef.h eventdsc.h
ev4hypou.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
ev4hypou.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
ev4hypou.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
ev4hypou.o: /usr/include/stdio_impl.h /usr/include/string.h
ev4hypou.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
ev4hypou.o: /programs/sol2/sh/source/sysbase.h
ev4hypou.o: /programs/sol2/sh/source/fousrdef.h eventdsc.h evhypout.h
evalflf.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
evalflf.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
evalflf.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
evalflf.o: /usr/include/stdio_impl.h /usr/include/string.h
evalflf.o: /usr/include/iso/string_iso.h /usr/include/math.h
evalflf.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
evalflf.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
evalflf.o: /programs/sol2/sh/source/sysbase.h
evalflf.o: /programs/sol2/sh/source/fousrdef.h
evalflf.o: /programs/sol2/sh/source/cpar.h
evalflf.o: /programs/sol2/sh/source/flerrors.h
evalflf.o: /programs/sol2/sh/source/erusrdef.h
evbullet.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
evbullet.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
evbullet.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
evbullet.o: /usr/include/stdio_impl.h /usr/include/string.h
evbullet.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
evbullet.o: /programs/sol2/sh/source/sysbase.h
evbullet.o: /programs/sol2/sh/source/fousrdef.h
evbullet.o: /programs/sol2/sh/source/tcusrdef.h
evbullet.o: /programs/sol2/sh/source/utusrdef.h eventdsc.h evtelex.h
evbullet.o: evbullet.h
evcreate.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
evcreate.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
evcreate.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
evcreate.o: /usr/include/stdio_impl.h /usr/include/string.h
evcreate.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
evcreate.o: /programs/sol2/sh/source/sysbase.h
evcreate.o: /programs/sol2/sh/source/fousrdef.h eventdsc.h
eventdsc.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
eventdsc.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
eventdsc.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
eventdsc.o: /usr/include/stdio_impl.h /usr/include/string.h
eventdsc.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
eventdsc.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
eventdsc.o: /programs/sol2/sh/source/sysbase.h
eventdsc.o: /programs/sol2/sh/source/fousrdef.h
eventdsc.o: /programs/sol2/sh/source/tcusrdef.h eventdsc.h
evhypo.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
evhypo.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
evhypo.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
evhypo.o: /usr/include/stdio_impl.h /usr/include/string.h
evhypo.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
evhypo.o: /programs/sol2/sh/source/sysbase.h
evhypo.o: /programs/sol2/sh/source/fousrdef.h
evhypo.o: /programs/sol2/sh/source/tcusrdef.h eventdsc.h evhypo.h
evhypout.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
evhypout.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
evhypout.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
evhypout.o: /usr/include/stdio_impl.h /usr/include/string.h
evhypout.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
evhypout.o: /programs/sol2/sh/source/sysbase.h
evhypout.o: /programs/sol2/sh/source/fousrdef.h
evhypout.o: /programs/sol2/sh/source/tcusrdef.h eventdsc.h evhypout.h
evshow.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
evshow.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
evshow.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
evshow.o: /usr/include/stdio_impl.h /usr/include/string.h
evshow.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
evshow.o: /programs/sol2/sh/source/sysbase.h
evshow.o: /programs/sol2/sh/source/fousrdef.h eventdsc.h
evt2phasetimes.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
evt2phasetimes.o: /usr/include/sys/feature_tests.h
evt2phasetimes.o: /usr/include/sys/isa_defs.h /usr/include/sys/va_list.h
evt2phasetimes.o: /usr/include/stdio_tag.h /usr/include/stdio_impl.h
evt2phasetimes.o: /usr/include/string.h /usr/include/iso/string_iso.h
evt2phasetimes.o: /usr/include/math.h /usr/include/iso/math_iso.h
evt2phasetimes.o: /programs/sol2/sh/source/basecnst.h /usr/include/stdlib.h
evt2phasetimes.o: /usr/include/iso/stdlib_iso.h
evt2phasetimes.o: /programs/sol2/sh/source/sysbase.h
evt2phasetimes.o: /programs/sol2/sh/source/fousrdef.h
evt2phasetimes.o: /programs/sol2/sh/source/cpar.h
evt2phasetimes.o: /programs/sol2/sh/source/earthloc.h
evt2phasetimes.o: /programs/sol2/sh/source/glusrdef.h
evt2phasetimes.o: /programs/sol2/sh/source/erusrdef.h
evt2phasetimes.o: /programs/sol2/sh/source/tcusrdef.h eventdsc.h
evt2relonset.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
evt2relonset.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
evt2relonset.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
evt2relonset.o: /usr/include/stdio_impl.h /usr/include/string.h
evt2relonset.o: /usr/include/iso/string_iso.h
evt2relonset.o: /programs/sol2/sh/source/basecnst.h
evt2relonset.o: /programs/sol2/sh/source/sysbase.h
evt2relonset.o: /programs/sol2/sh/source/fousrdef.h eventdsc.h
evt2relonset.o: /programs/sol2/sh/source/tcusrdef.h
evt2reltimes.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
evt2reltimes.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
evt2reltimes.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
evt2reltimes.o: /usr/include/stdio_impl.h /usr/include/string.h
evt2reltimes.o: /usr/include/iso/string_iso.h
evt2reltimes.o: /programs/sol2/sh/source/basecnst.h /usr/include/stdlib.h
evt2reltimes.o: /usr/include/iso/stdlib_iso.h
evt2reltimes.o: /programs/sol2/sh/source/sysbase.h
evt2reltimes.o: /programs/sol2/sh/source/fousrdef.h eventdsc.h
evt2reltimes.o: /programs/sol2/sh/source/tcusrdef.h
evt_copy_and_split.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
evt_copy_and_split.o: /usr/include/sys/feature_tests.h
evt_copy_and_split.o: /usr/include/sys/isa_defs.h /usr/include/sys/va_list.h
evt_copy_and_split.o: /usr/include/stdio_tag.h /usr/include/stdio_impl.h
evt_copy_and_split.o: /usr/include/string.h /usr/include/iso/string_iso.h
evt_copy_and_split.o: /programs/sol2/sh/source/basecnst.h
evt_copy_and_split.o: /programs/sol2/sh/source/sysbase.h
evt_copy_and_split.o: /programs/sol2/sh/source/fousrdef.h
evt_copy_and_split.o: /programs/sol2/sh/source/cpar.h
evt_copy_and_split.o: /programs/sol2/sh/source/utusrdef.h eventdsc.h
evt_copy_and_split.o: /programs/sol2/sh/source/erusrdef.h
evt_correct_aniso.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
evt_correct_aniso.o: /usr/include/sys/feature_tests.h
evt_correct_aniso.o: /usr/include/sys/isa_defs.h /usr/include/sys/va_list.h
evt_correct_aniso.o: /usr/include/stdio_tag.h /usr/include/stdio_impl.h
evt_correct_aniso.o: /usr/include/string.h /usr/include/iso/string_iso.h
evt_correct_aniso.o: /usr/include/math.h /usr/include/iso/math_iso.h
evt_correct_aniso.o: /programs/sol2/sh/source/basecnst.h
evt_correct_aniso.o: /programs/sol2/sh/source/sysbase.h
evt_correct_aniso.o: /programs/sol2/sh/source/fousrdef.h
evt_correct_aniso.o: /programs/sol2/sh/source/tcusrdef.h
evt_correct_aniso.o: /programs/sol2/sh/source/earthloc.h
evt_correct_aniso.o: /programs/sol2/sh/source/glusrdef.h
evt_correct_aniso.o: /programs/sol2/sh/source/cpar.h eventdsc.h timelist.h
evt_extract_ms.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
evt_extract_ms.o: /usr/include/sys/feature_tests.h
evt_extract_ms.o: /usr/include/sys/isa_defs.h /usr/include/sys/va_list.h
evt_extract_ms.o: /usr/include/stdio_tag.h /usr/include/stdio_impl.h
evt_extract_ms.o: /usr/include/string.h /usr/include/iso/string_iso.h
evt_extract_ms.o: /programs/sol2/sh/source/basecnst.h
evt_extract_ms.o: /programs/sol2/sh/source/sysbase.h
evt_extract_ms.o: /programs/sol2/sh/source/fousrdef.h
evt_extract_ms.o: /programs/sol2/sh/source/tcusrdef.h
evt_extract_ms.o: /programs/sol2/sh/source/cpar.h eventdsc.h
evt_extract_ms.o: /programs/sol2/sh/source/erusrdef.h
evt_time_sort.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
evt_time_sort.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
evt_time_sort.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
evt_time_sort.o: /usr/include/stdio_impl.h /usr/include/string.h
evt_time_sort.o: /usr/include/iso/string_iso.h
evt_time_sort.o: /programs/sol2/sh/source/basecnst.h
evt_time_sort.o: /programs/sol2/sh/source/sysbase.h
evt_time_sort.o: /programs/sol2/sh/source/fousrdef.h
evt_time_sort.o: /programs/sol2/sh/source/tcusrdef.h
evt_time_sort.o: /programs/sol2/sh/source/cpar.h eventdsc.h timelist.h
evtelex.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
evtelex.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
evtelex.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
evtelex.o: /usr/include/stdio_impl.h /usr/include/string.h
evtelex.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
evtelex.o: /programs/sol2/sh/source/sysbase.h
evtelex.o: /programs/sol2/sh/source/fousrdef.h
evtelex.o: /programs/sol2/sh/source/tcusrdef.h
evtelex.o: /programs/sol2/sh/source/utusrdef.h eventdsc.h evtelex.h
fconcat.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
fconcat.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
fconcat.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
fconcat.o: /usr/include/stdio_impl.h /usr/include/string.h
fconcat.o: /usr/include/iso/string_iso.h /usr/include/math.h
fconcat.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
fconcat.o: /programs/sol2/sh/source/sysbase.h
fconcat.o: /programs/sol2/sh/source/fousrdef.h
fconcat.o: /programs/sol2/sh/source/cpar.h
fereg.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
fereg.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
fereg.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
fereg.o: /usr/include/stdio_impl.h /usr/include/string.h
fereg.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
fereg.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
fereg.o: /programs/sol2/sh/source/sysbase.h
fereg.o: /programs/sol2/sh/source/fousrdef.h /programs/sol2/sh/source/cpar.h
fereg.o: /programs/sol2/sh/source/earthloc.h inpfiles.h
find_next_loc.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
find_next_loc.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
find_next_loc.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
find_next_loc.o: /usr/include/stdio_impl.h /usr/include/string.h
find_next_loc.o: /usr/include/iso/string_iso.h
find_next_loc.o: /programs/sol2/sh/source/basecnst.h
find_next_loc.o: /programs/sol2/sh/source/sysbase.h
find_next_loc.o: /programs/sol2/sh/source/fousrdef.h
find_next_loc.o: /programs/sol2/sh/source/utusrdef.h
find_next_loc.o: /programs/sol2/sh/source/glusrdef.h
find_next_loc.o: /programs/sol2/sh/source/earthloc.h
find_next_loc.o: /programs/sol2/sh/source/ptusrdef.h
find_topo30_id.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
find_topo30_id.o: /usr/include/sys/feature_tests.h
find_topo30_id.o: /usr/include/sys/isa_defs.h /usr/include/sys/va_list.h
find_topo30_id.o: /usr/include/stdio_tag.h /usr/include/stdio_impl.h
find_topo30_id.o: /usr/include/string.h /usr/include/iso/string_iso.h
findevnt.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
findevnt.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
findevnt.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
findevnt.o: /usr/include/stdio_impl.h /usr/include/string.h
findevnt.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
findevnt.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
findevnt.o: /programs/sol2/sh/source/sysbase.h
findevnt.o: /programs/sol2/sh/source/fousrdef.h
findevnt.o: /programs/sol2/sh/source/cpar.h
findevnt.o: /programs/sol2/sh/source/earthloc.h
findevnt.o: /programs/sol2/sh/source/tcusrdef.h ../source/ptusrdef.h
findevnt.o: /programs/sol2/sh/source/utusrdef.h inpfiles.h
fit_to_line.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
fit_to_line.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
fit_to_line.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
fit_to_line.o: /usr/include/stdio_impl.h /usr/include/string.h
fit_to_line.o: /usr/include/iso/string_iso.h /usr/include/math.h
fit_to_line.o: /usr/include/iso/math_iso.h
fit_to_line.o: /programs/sol2/sh/source/basecnst.h /usr/include/stdlib.h
fit_to_line.o: /usr/include/iso/stdlib_iso.h
fit_to_line.o: /programs/sol2/sh/source/sysbase.h
fit_to_line.o: /programs/sol2/sh/source/fousrdef.h
fit_to_line.o: /programs/sol2/sh/source/numres.h
fit_travel.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
fit_travel.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
fit_travel.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
fit_travel.o: /usr/include/stdio_impl.h /usr/include/string.h
fit_travel.o: /usr/include/iso/string_iso.h /usr/include/math.h
fit_travel.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
fit_travel.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
fit_travel.o: /programs/sol2/sh/source/sysbase.h
fit_travel.o: /programs/sol2/sh/source/fousrdef.h
fit_travel.o: /programs/sol2/sh/source/cpar.h
fit_travel.o: /programs/sol2/sh/source/ptusrdef.h
fit_travel.o: /programs/sol2/sh/source/earthloc.h
fit_travel.o: /programs/sol2/sh/source/glusrdef.h
fit_travel.o: /programs/sol2/sh/source/erusrdef.h
fit_travel.o: /programs/sol2/sh/source/tcusrdef.h
fit_travel2.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
fit_travel2.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
fit_travel2.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
fit_travel2.o: /usr/include/stdio_impl.h /usr/include/string.h
fit_travel2.o: /usr/include/iso/string_iso.h /usr/include/math.h
fit_travel2.o: /usr/include/iso/math_iso.h
fit_travel2.o: /programs/sol2/sh/source/basecnst.h /usr/include/stdlib.h
fit_travel2.o: /usr/include/iso/stdlib_iso.h
fit_travel2.o: /programs/sol2/sh/source/sysbase.h
fit_travel2.o: /programs/sol2/sh/source/fousrdef.h
fit_travel2.o: /programs/sol2/sh/source/cpar.h
fit_travel2.o: /programs/sol2/sh/source/ptusrdef.h
fit_travel2.o: /programs/sol2/sh/source/earthloc.h
fit_travel2.o: /programs/sol2/sh/source/numres.h
fit_travel2.o: /programs/sol2/sh/source/glusrdef.h
fit_travel2.o: /programs/sol2/sh/source/erusrdef.h
fit_travel2.o: /programs/sol2/sh/source/tcusrdef.h
fit_travel2.o: /programs/sol2/sh/source/globalparams.h
flf2flt.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
flf2flt.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
flf2flt.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
flf2flt.o: /usr/include/stdio_impl.h /usr/include/string.h
flf2flt.o: /usr/include/iso/string_iso.h /usr/include/math.h
flf2flt.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
flf2flt.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
flf2flt.o: /programs/sol2/sh/source/sysbase.h
flf2flt.o: /programs/sol2/sh/source/fousrdef.h
flf2flt.o: /programs/sol2/sh/source/cpar.h
flf2flt.o: /programs/sol2/sh/source/flerrors.h
flf2flt.o: /programs/sol2/sh/source/erusrdef.h
floatop.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
floatop.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
floatop.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
floatop.o: /usr/include/stdio_impl.h /usr/include/string.h
floatop.o: /usr/include/iso/string_iso.h /usr/include/math.h
floatop.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
floatop.o: /programs/sol2/sh/source/sysbase.h
floatop.o: /programs/sol2/sh/source/fousrdef.h
floatop.o: /programs/sol2/sh/source/cpar.h
getresid.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
getresid.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
getresid.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
getresid.o: /usr/include/stdio_impl.h /usr/include/string.h
getresid.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
getresid.o: /programs/sol2/sh/source/sysbase.h
getresid.o: /programs/sol2/sh/source/fousrdef.h
getresid.o: /programs/sol2/sh/source/erusrdef.h
getresid.o: /programs/sol2/sh/source/residual.h
gsetest.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
gsetest.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
gsetest.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
gsetest.o: /usr/include/stdio_impl.h /usr/include/string.h
gsetest.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
gsetest.o: /programs/sol2/sh/source/sysbase.h
gsetest.o: /programs/sol2/sh/source/fousrdef.h
gsetest.o: /programs/sol2/sh/source/foreign/gsedata.h
ht2pz.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
ht2pz.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
ht2pz.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
ht2pz.o: /usr/include/stdio_impl.h /usr/include/string.h
ht2pz.o: /usr/include/iso/string_iso.h /usr/include/math.h
ht2pz.o: /usr/include/iso/math_iso.h
hypo_stlist.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
hypo_stlist.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
hypo_stlist.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
hypo_stlist.o: /usr/include/stdio_impl.h /usr/include/string.h
hypo_stlist.o: /usr/include/iso/string_iso.h /usr/include/math.h
hypo_stlist.o: /usr/include/iso/math_iso.h
hypo_stlist.o: /programs/sol2/sh/source/basecnst.h /usr/include/stdlib.h
hypo_stlist.o: /usr/include/iso/stdlib_iso.h
hypo_stlist.o: /programs/sol2/sh/source/sysbase.h
hypo_stlist.o: /programs/sol2/sh/source/fousrdef.h
hypo_stlist.o: /programs/sol2/sh/source/glusrdef.h
hypo_stlist.o: /programs/sol2/sh/source/cpar.h
hypo_stlist.o: /programs/sol2/sh/source/utusrdef.h inpfiles.h
lineparse.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
lineparse.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
lineparse.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
lineparse.o: /usr/include/stdio_impl.h /usr/include/string.h
lineparse.o: /usr/include/iso/string_iso.h
lineparse.o: /programs/sol2/sh/source/basecnst.h
lineparse.o: /programs/sol2/sh/source/sysbase.h
lineparse.o: /programs/sol2/sh/source/fousrdef.h lineparse.h
locadd.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
locadd.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
locadd.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
locadd.o: /usr/include/stdio_impl.h /usr/include/string.h
locadd.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
locadd.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
locadd.o: /programs/sol2/sh/source/sysbase.h
locadd.o: /programs/sol2/sh/source/fousrdef.h /programs/sol2/sh/source/cpar.h
locadd.o: /programs/sol2/sh/source/utusrdef.h
locadd.o: /programs/sol2/sh/source/glusrdef.h
locadd.o: /programs/sol2/sh/source/earthloc.h inpfiles.h
locdiff.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
locdiff.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
locdiff.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
locdiff.o: /usr/include/stdio_impl.h /usr/include/string.h
locdiff.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
locdiff.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
locdiff.o: /programs/sol2/sh/source/sysbase.h
locdiff.o: /programs/sol2/sh/source/fousrdef.h
locdiff.o: /programs/sol2/sh/source/cpar.h
locdiff.o: /programs/sol2/sh/source/utusrdef.h
locdiff.o: /programs/sol2/sh/source/glusrdef.h
locdiff.o: /programs/sol2/sh/source/earthloc.h
locdiff.o: /programs/sol2/sh/source/ptusrdef.h inpfiles.h
logseries.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
logseries.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
logseries.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
logseries.o: /usr/include/stdio_impl.h /usr/include/string.h
logseries.o: /usr/include/iso/string_iso.h
lynx_reformat.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
lynx_reformat.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
lynx_reformat.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
lynx_reformat.o: /usr/include/stdio_impl.h /usr/include/string.h
lynx_reformat.o: /usr/include/iso/string_iso.h
onset_convert.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
onset_convert.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
onset_convert.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
onset_convert.o: /usr/include/stdio_impl.h /usr/include/string.h
onset_convert.o: /usr/include/iso/string_iso.h
onset_convert.o: /programs/sol2/sh/source/basecnst.h
onset_convert.o: /programs/sol2/sh/source/sysbase.h
onset_convert.o: /programs/sol2/sh/source/fousrdef.h
onset_convert.o: /programs/sol2/sh/source/tcusrdef.h
q2gse.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
q2gse.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
q2gse.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
q2gse.o: /usr/include/stdio_impl.h /usr/include/string.h
q2gse.o: /usr/include/iso/string_iso.h /usr/include/math.h
q2gse.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
q2gse.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
q2gse.o: /programs/sol2/sh/source/sysbase.h
q2gse.o: /programs/sol2/sh/source/fousrdef.h /programs/sol2/sh/source/cpar.h
q2gse.o: /programs/sol2/sh/source/qfusrdef.h
q2gse.o: /programs/sol2/sh/source/qferrors.h
q2gse.o: /programs/sol2/sh/source/dfqentry.h
q2gse.o: /programs/sol2/sh/source/tcusrdef.h
q2gse.o: /programs/sol2/sh/source/erusrdef.h
q2gse.o: /programs/sol2/sh/source/glusrdef.h inpfiles.h
qed_ms_make_evt.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
qed_ms_make_evt.o: /usr/include/sys/feature_tests.h
qed_ms_make_evt.o: /usr/include/sys/isa_defs.h /usr/include/sys/va_list.h
qed_ms_make_evt.o: /usr/include/stdio_tag.h /usr/include/stdio_impl.h
qed_ms_make_evt.o: /usr/include/string.h /usr/include/iso/string_iso.h
qed_ms_make_evt.o: /programs/sol2/sh/source/basecnst.h
qed_ms_make_evt.o: /programs/sol2/sh/source/sysbase.h
qed_ms_make_evt.o: /programs/sol2/sh/source/fousrdef.h eventdsc.h
qed_ms_make_evt.o: /programs/sol2/sh/source/tcusrdef.h
reformat_hypoe_out.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
reformat_hypoe_out.o: /usr/include/sys/feature_tests.h
reformat_hypoe_out.o: /usr/include/sys/isa_defs.h /usr/include/sys/va_list.h
reformat_hypoe_out.o: /usr/include/stdio_tag.h /usr/include/stdio_impl.h
reformat_hypoe_out.o: /usr/include/string.h /usr/include/iso/string_iso.h
reformat_locsat_out.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
reformat_locsat_out.o: /usr/include/sys/feature_tests.h
reformat_locsat_out.o: /usr/include/sys/isa_defs.h /usr/include/sys/va_list.h
reformat_locsat_out.o: /usr/include/stdio_tag.h /usr/include/stdio_impl.h
reformat_locsat_out.o: /usr/include/string.h /usr/include/iso/string_iso.h
reformat_locsat_out.o: /programs/sol2/sh/source/basecnst.h
reformat_locsat_out.o: /programs/sol2/sh/source/sysbase.h
reformat_locsat_out.o: /programs/sol2/sh/source/fousrdef.h
reformat_locsat_out.o: /programs/sol2/sh/source/tcusrdef.h
relonset2slo.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
relonset2slo.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
relonset2slo.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
relonset2slo.o: /usr/include/stdio_impl.h /usr/include/string.h
relonset2slo.o: /usr/include/iso/string_iso.h /usr/include/math.h
relonset2slo.o: /usr/include/iso/math_iso.h
relonset2slo.o: /programs/sol2/sh/source/basecnst.h /usr/include/stdlib.h
relonset2slo.o: /usr/include/iso/stdlib_iso.h
relonset2slo.o: /programs/sol2/sh/source/sysbase.h
relonset2slo.o: /programs/sol2/sh/source/fousrdef.h
relonset2slo.o: /programs/sol2/sh/source/cpar.h
relonset2slo.o: /programs/sol2/sh/source/erusrdef.h
relonset2slo.o: /programs/sol2/sh/source/glusrdef.h
relonset2slo.o: /programs/sol2/sh/source/earthloc.h
relonset2slo.o: /programs/sol2/sh/source/fctxmt.h
relonset2slo.o: /programs/sol2/sh/source/shconst.h
restfreq.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
restfreq.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
restfreq.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
restfreq.o: /usr/include/stdio_impl.h /usr/include/string.h
restfreq.o: /usr/include/iso/string_iso.h /usr/include/math.h
restfreq.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
restfreq.o: /programs/sol2/sh/source/sysbase.h
restfreq.o: /programs/sol2/sh/source/fousrdef.h /usr/include/stdlib.h
restfreq.o: /usr/include/iso/stdlib_iso.h /programs/sol2/sh/source/cpar.h
restrec.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
restrec.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
restrec.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
restrec.o: /usr/include/stdio_impl.h /usr/include/string.h
restrec.o: /usr/include/iso/string_iso.h /usr/include/math.h
restrec.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
restrec.o: /programs/sol2/sh/source/sysbase.h
restrec.o: /programs/sol2/sh/source/fousrdef.h
seisan2gse.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
seisan2gse.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
seisan2gse.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
seisan2gse.o: /usr/include/stdio_impl.h /usr/include/string.h
seisan2gse.o: /usr/include/iso/string_iso.h
simfreq.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
simfreq.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
simfreq.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
simfreq.o: /usr/include/stdio_impl.h /usr/include/string.h
simfreq.o: /usr/include/iso/string_iso.h /usr/include/math.h
simfreq.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
simfreq.o: /programs/sol2/sh/source/sysbase.h
simfreq.o: /programs/sol2/sh/source/fousrdef.h
simfreq.o: /programs/sol2/sh/source/cpar.h
simrec.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
simrec.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
simrec.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
simrec.o: /usr/include/stdio_impl.h /usr/include/string.h
simrec.o: /usr/include/iso/string_iso.h /usr/include/math.h
simrec.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
simrec.o: /programs/sol2/sh/source/sysbase.h
simrec.o: /programs/sol2/sh/source/fousrdef.h
slo2distance.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
slo2distance.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
slo2distance.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
slo2distance.o: /usr/include/stdio_impl.h /usr/include/string.h
slo2distance.o: /usr/include/iso/string_iso.h
slo2distance.o: /programs/sol2/sh/source/basecnst.h /usr/include/stdlib.h
slo2distance.o: /usr/include/iso/stdlib_iso.h
slo2distance.o: /programs/sol2/sh/source/sysbase.h
slo2distance.o: /programs/sol2/sh/source/fousrdef.h
slo2distance.o: /programs/sol2/sh/source/ptusrdef.h
slo2distance.o: /programs/sol2/sh/source/erusrdef.h
slo2distance.o: /programs/sol2/sh/source/cpar.h
statinf.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
statinf.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
statinf.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
statinf.o: /usr/include/stdio_impl.h /usr/include/string.h
statinf.o: /usr/include/iso/string_iso.h /usr/include/math.h
statinf.o: /usr/include/iso/math_iso.h /programs/sol2/sh/source/basecnst.h
statinf.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
statinf.o: /programs/sol2/sh/source/sysbase.h
statinf.o: /programs/sol2/sh/source/fousrdef.h
statinf.o: /programs/sol2/sh/source/cpar.h
statinf.o: /programs/sol2/sh/source/glusrdef.h
stddev.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
stddev.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
stddev.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
stddev.o: /usr/include/stdio_impl.h /usr/include/string.h
stddev.o: /usr/include/iso/string_iso.h /usr/include/math.h
stddev.o: /usr/include/iso/math_iso.h
stringop.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
stringop.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
stringop.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
stringop.o: /usr/include/stdio_impl.h /usr/include/string.h
stringop.o: /usr/include/iso/string_iso.h
timelist.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
timelist.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
timelist.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
timelist.o: /usr/include/stdio_impl.h /usr/include/string.h
timelist.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
timelist.o: /usr/include/stdlib.h /usr/include/iso/stdlib_iso.h
timelist.o: /programs/sol2/sh/source/sysbase.h
timelist.o: /programs/sol2/sh/source/fousrdef.h timelist.h
timelist.o: /programs/sol2/sh/source/tcusrdef.h
timename.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
timename.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
timename.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
timename.o: /usr/include/stdio_impl.h /usr/include/string.h
timename.o: /usr/include/iso/string_iso.h /programs/sol2/sh/source/basecnst.h
timename.o: /programs/sol2/sh/source/sysbase.h
timename.o: /programs/sol2/sh/source/fousrdef.h
timename.o: /programs/sol2/sh/source/tcusrdef.h
timename.o: /programs/sol2/sh/source/cpar.h
traveltime.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
traveltime.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
traveltime.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
traveltime.o: /usr/include/stdio_impl.h /usr/include/string.h
traveltime.o: /usr/include/iso/string_iso.h
traveltime.o: /programs/sol2/sh/source/basecnst.h /usr/include/stdlib.h
traveltime.o: /usr/include/iso/stdlib_iso.h
traveltime.o: /programs/sol2/sh/source/sysbase.h
traveltime.o: /programs/sol2/sh/source/fousrdef.h
traveltime.o: /programs/sol2/sh/source/ptusrdef.h
traveltime.o: /programs/sol2/sh/source/erusrdef.h
traveltime.o: /programs/sol2/sh/source/cpar.h
vrs_read_diff.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
vrs_read_diff.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
vrs_read_diff.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
vrs_read_diff.o: /usr/include/stdio_impl.h /usr/include/string.h
vrs_read_diff.o: /usr/include/iso/string_iso.h
vrs_read_diff.o: /programs/sol2/sh/source/basecnst.h
vrs_read_diff.o: /programs/sol2/sh/source/sysbase.h
vrs_read_diff.o: /programs/sol2/sh/source/fousrdef.h
weather2ascii.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
weather2ascii.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
weather2ascii.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
weather2ascii.o: /usr/include/stdio_impl.h /usr/include/string.h
weather2ascii.o: /usr/include/iso/string_iso.h /usr/include/math.h
weather2ascii.o: /usr/include/iso/math_iso.h
weather2ascii.o: /programs/sol2/sh/source/basecnst.h /usr/include/stdlib.h
weather2ascii.o: /usr/include/iso/stdlib_iso.h
weather2ascii.o: /programs/sol2/sh/source/sysbase.h
weather2ascii.o: /programs/sol2/sh/source/fousrdef.h
weather2ascii.o: /programs/sol2/sh/source/cpar.h
weather2ascii.o: /programs/sol2/sh/source/erusrdef.h
weather2ascii.o: /programs/sol2/sh/source/tcusrdef.h
