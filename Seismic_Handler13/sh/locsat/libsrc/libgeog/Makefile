#
# SccsId:  @(#)Makefile	43.1	9/9/91
#
SRCS		= azcros.f latlon.f distaz.f cart.f geog.f rotate.f \
		  crossings_.c azcros2_.c distaz2_.c latlon2_.c
OBJS		= azcros.o latlon.o distaz.o cart.o geog.o rotate.o \
		  crossings_.o azcros2_.o distaz2_.o latlon2_.o
HDRS		=
LIBS		=
LIBRARY		= libgeog.a
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

crossings_.o: /usr/include/math.h /usr/include/features.h
crossings_.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
crossings_.o: /usr/include/bits/huge_val.h /usr/include/bits/mathdef.h
crossings_.o: /usr/include/bits/mathcalls.h
crossings_.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/float.h
azcros2_.o: /usr/include/math.h /usr/include/features.h
azcros2_.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
azcros2_.o: /usr/include/bits/huge_val.h /usr/include/bits/mathdef.h
azcros2_.o: /usr/include/bits/mathcalls.h
azcros2_.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/float.h
distaz2_.o: /usr/include/math.h /usr/include/features.h
distaz2_.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
distaz2_.o: /usr/include/bits/huge_val.h /usr/include/bits/mathdef.h
distaz2_.o: /usr/include/bits/mathcalls.h
distaz2_.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/float.h
latlon2_.o: /usr/include/math.h /usr/include/features.h
latlon2_.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
latlon2_.o: /usr/include/bits/huge_val.h /usr/include/bits/mathdef.h
latlon2_.o: /usr/include/bits/mathcalls.h
latlon2_.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/float.h
