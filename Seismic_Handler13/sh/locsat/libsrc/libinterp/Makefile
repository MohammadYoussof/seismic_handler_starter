#
# SccsId:  @(#)Makefile	44.2	9/25/91
#
SRCS		= holin2.f holint2.f holint.f interp.f quaint.f brack.f \
		  fixhol.f hermit.f splie2.c splin2.c spline.c splint.c \
		  ratint.c nrutil.c
OBJS		= holin2.o holint2.o holint.o interp.o quaint.o brack.o \
		  fixhol.o hermit.o splie2.o splin2.o spline.o splint.o \
		  ratint.o nrutil.o
HDRS		=
LIBS		=
LIBRARY		= libinterp.a
SUBDIRS		=

O		= -O
INCLUDES	=
CFLAGS		= $O $(INCLUDES)
FFLAGS		= $O				# Fortran flags
LDFLAGS		= $O				# Linker flags
LFLAGS		= $(INCLUDES)			# Lint flags

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

FLAGS		= $(MFLAGS) "CC=$(CC)" "F77=$(F77)" "O=$O" \
		  "GET=$(GET)" "GFLAGS=$(GFLAGS)" "SCLEAN=$(SCLEAN)" \
		  "LMODE=$(LMODE)" "PMODE=$(PMODE)" \
		  "PRINT=$(PRINT)" "MAKE=$(MAKE)"

all:		$(LIBRARY)

$(LIBRARY):	$(OBJS)
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
		
lint:		$(SRCS) $(HDRS)
		$(LINT) $(LFLAGS) $(SRCS)

print:		$(SRCS) $(HDRS) Makefile
		$(PRINT) $(SRCS) $(HDRS) Makefile

sccs:
		$(GET) $(GFLAGS) $(SRCS) $(HDRS) Makefile

depend:		$(SRCS) $(HDRS) Makefile
		$(DEPEND) $(INCLUDES) $(SRCS)

###
# DO NOT DELETE THIS LINE -- make depend depends on it.

ratint.o: /usr/include/math.h /usr/include/features.h
ratint.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
ratint.o: /usr/include/bits/huge_val.h /usr/include/bits/mathdef.h
ratint.o: /usr/include/bits/mathcalls.h
ratint.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/float.h
nrutil.o: /usr/include/malloc.h
nrutil.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/stddef.h
nrutil.o: /usr/include/stdio.h /usr/include/features.h
nrutil.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
nrutil.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/stdarg.h
nrutil.o: /usr/include/bits/types.h /usr/include/libio.h
nrutil.o: /usr/include/_G_config.h /usr/include/bits/stdio_lim.h
