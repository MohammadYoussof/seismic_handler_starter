#
# SccsId:  @(#)Makefile	44.1 9/20/91
#
SRCS		= rdtab.f rdtab1.f rdcortab.f rdcortab1.f clitok.f \
		  bicube.f rdtttab.c lnblnk.f
OBJS		= rdtab.o rdtab1.o rdcortab.o rdcortab1.o clitok.o \
		  bicube.o rdtttab.o lnblnk.o
HDRS		= rdcor.h
LIBS		=
LIBRARY		= librdwrt.a
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

# DO NOT DELETE THIS LINE -- make depend depends on it.

rdtttab.o: /usr/include/stdio.h /usr/include/features.h
rdtttab.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
rdtttab.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/stddef.h
rdtttab.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/stdarg.h
rdtttab.o: /usr/include/bits/types.h /usr/include/libio.h
rdtttab.o: /usr/include/_G_config.h /usr/include/bits/stdio_lim.h
rdtttab.o: /usr/include/strings.h
