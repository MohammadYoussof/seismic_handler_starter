#
# SccsId:  @(#)Makefile	44.1	9/20/91
#

SRCS		= predsat.c locate_event.c locsat0.f check_data_.c \
		  hypcut0.f hypinv0.f azcal_.c denuis_.c ellips_.c \
		  fstat_.c index_array_.c predsat0.f slocal0.f \
		  solve_via_svd_.c dsvdc.f daxpy.f ddot.f dnrm2.f \
		  drot.f drotg.f dscal.f dswap.f ssscor.f ttcal0.f elpcor.f

OBJS		= predsat.o locate_event.o locsat0.o check_data_.o \
		  hypcut0.o hypinv0.o azcal_.o denuis_.o ellips_.o \
		  fstat_.o index_array_.o predsat0.o slocal0.o \
		  solve_via_svd_.o dsvdc.o daxpy.o ddot.o dnrm2.o \
		  drot.o drotg.o dscal.o dswap.o ssscor.o ttcal0.o elpcor.o

TEST		= test.o solve_via_svd_.o dsvdc.o daxpy.o ddot.o dnrm2.o \
		  drot.o drotg.o dscal.o dswap.o

HDRS		= rdcor.h
LIBS		=
LIBRARY		= libloc.a
SUBDIRS		=

O		= -O
INCLUDES	= -I../../include -I../../include/db3
CFLAGS		= $O $(INCLUDES) -DSCCSID
FFLAGS		= $O
LDFLAGS		= $O
LFLAGS		= $(INCLUDES)

DESTDIR		= ../../lib

CC		= cc
F77		= g77
LD		= $(CC)
LINT		= lint
YACC		= yacc
LEX		= lex

LMODE		= 664
PMODE		= 775

GET		= sccs get
GFLAGS		=
SCLEAN		= sccs clean

RM		= rm
INSTALL		= install
MAKE		= make
PRINT		= lpr -p
DEPEND		= makedepend
AR		= ar
RANLIB		= ranlib

FLAGS		= $(MFLAGS) "CC=$(CC)" "F77=$(F77)" "O=$O" "GET=$(GET)" \
		  "GFLAGS=$(GFLAGS)" "SCLEAN=$(SCLEAN)" "LMODE=$(LMODE)" \
		  "PMODE=$(PMODE)" "PRINT=$(PRINT)" "MAKE=$(MAKE)"

all:		$(LIBRARY)

$(LIBRARY):	$(HDRS) $(OBJS)
		-$(RM) -f $(LIBRARY)
		$(AR) cq $(LIBRARY) $(OBJS)
		$(RANLIB) $(LIBRARY)

install:	all
		$(INSTALL) -m $(LMODE) $(LIBRARY) $(DESTDIR)
		$(RANLIB) $(DESTDIR)/$(LIBRARY)

clean:
		-$(RM) -f $(OBJS) core

xclean:
		-$(RM) -f $(OBJS) core
		-$(RM) -f $(PROGRAM) $(LIBRARY)
		-$(SCLEAN)
		
test:		$(TEST)	
		f77 $(FFLAGS) $(TEST) -o test

lint:		$(SRCS) $(HDRS)
		$(LINT) $(LFLAGS) $(SRCS)

print:		$(SRCS) $(HDRS) Makefile
		$(PRINT) $(SRCS) $(HDRS) Makefile

sccs:
		$(GET) $(GFLAGS) $(SRCS) $(HDRS) Makefile

depend:		$(SRCS) $(HDRS) Makefile
		$(DEPEND) $(INCLUDES) $(SRCS)

# DO NOT DELETE THIS LINE -- make depend depends on it.

predsat.o: /usr/include/stdio.h /usr/include/features.h
predsat.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
predsat.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/stddef.h
predsat.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/stdarg.h
predsat.o: /usr/include/bits/types.h /usr/include/libio.h
predsat.o: /usr/include/_G_config.h /usr/include/bits/stdio_lim.h
predsat.o: /usr/include/string.h /usr/include/math.h
predsat.o: /usr/include/bits/huge_val.h /usr/include/bits/mathdef.h
predsat.o: /usr/include/bits/mathcalls.h
predsat.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/float.h
predsat.o: ../../include/aesir.h ../../include/db3/db_origerr.h
predsat.o: ../../include/db3/db_origin.h ../../include/db3/db_site.h
locate_event.o: /usr/include/stdio.h /usr/include/features.h
locate_event.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
locate_event.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/stddef.h
locate_event.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/stdarg.h
locate_event.o: /usr/include/bits/types.h /usr/include/libio.h
locate_event.o: /usr/include/_G_config.h /usr/include/bits/stdio_lim.h
locate_event.o: /usr/include/ctype.h /usr/include/endian.h
locate_event.o: /usr/include/bits/endian.h /usr/include/string.h
locate_event.o: /usr/include/math.h /usr/include/bits/huge_val.h
locate_event.o: /usr/include/bits/mathdef.h /usr/include/bits/mathcalls.h
locate_event.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/float.h
locate_event.o: ../../include/aesir.h ../../include/db3/db_site.h
locate_event.o: ../../include/db3/db_arrival.h ../../include/db3/db_assoc.h
locate_event.o: ../../include/db3/db_origin.h ../../include/db3/db_origerr.h
locate_event.o: ../../include/loc_params.h ../../include/css/trim.h
check_data_.o: /usr/include/stdio.h /usr/include/features.h
check_data_.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
check_data_.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/stddef.h
check_data_.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/stdarg.h
check_data_.o: /usr/include/bits/types.h /usr/include/libio.h
check_data_.o: /usr/include/_G_config.h /usr/include/bits/stdio_lim.h
check_data_.o: /usr/include/string.h
azcal_.o: /usr/include/math.h /usr/include/features.h
azcal_.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
azcal_.o: /usr/include/bits/huge_val.h /usr/include/bits/mathdef.h
azcal_.o: /usr/include/bits/mathcalls.h
azcal_.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/float.h
ellips_.o: /usr/include/math.h /usr/include/features.h
ellips_.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
ellips_.o: /usr/include/bits/huge_val.h /usr/include/bits/mathdef.h
ellips_.o: /usr/include/bits/mathcalls.h
ellips_.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/float.h
fstat_.o: /usr/include/math.h /usr/include/features.h
fstat_.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
fstat_.o: /usr/include/bits/huge_val.h /usr/include/bits/mathdef.h
fstat_.o: /usr/include/bits/mathcalls.h
fstat_.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/float.h
solve_via_svd_.o: ../../include/aesir.h
