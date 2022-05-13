#
# SccsId: @(#)Makefile	44.1 11/1/91
#
SRCS		= farrloc.f interpl.f fdistaz.f fmidpt.f \
		  fitime.f fregion.f reg.c deepevent.f
OBJS		= farrloc.o interpl.o fdistaz.o fmidpt.o \
		  fitime.o fregion.o reg.o deepevent.o
HDRS		= reg.h regonindex.h
LIBS		= ../libtimes.a ../libgeog.a
LIBRARY		= libxprt2.a
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
		
lint:		$(SRCS) $(HDRS)
		$(LINT) $(LFLAGS) $(SRCS)

print:		$(SRCS) $(HDRS) Makefile
		$(PRINT) $(SRCS) $(HDRS) Makefile

sccs:
		$(GET) $(GFLAGS) $(SRCS) $(HDRS) Makefile

depend:		$(SRCS) $(HDRS) Makefile
		$(DEPEND) $(INCLUDES) $(SRCS)

ttime.o:	../../include/param.h
# DO NOT DELETE THIS LINE -- make depend depends on it.

reg.o: reg.h regonindex.h
