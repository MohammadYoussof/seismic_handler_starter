#
# Makefile.prg
# SccsId:  @(#)Makefile	31.1  8/26/88
#
SRCS		= constants.f func.f main.f pastim.f phaztm.f ptable.f \
		  ampcal.f rgsrtm.f setvel.f stable.f timlpp.f trvltm.f \
		  trvtim.f ttime.f solowa.f solmwa.f range.f clixtr.f \
		  cliget.f ciptok.f cipnum.f cipscn.f ciptyp.f
OBJS		= constants.o func.o main.o pastim.o phaztm.o ptable.o \
		  ampcal.o rgsrtm.o setvel.o stable.o timlpp.o trvltm.o \
		  trvtim.o ttime.o solowa.o solmwa.o range.o clixtr.o \
		  cliget.o ciptok.o cipnum.o cipscn.o ciptyp.o
HDRS		=
LIBS 		= $(LIBDIR1)/libgeog.a
PROGRAM		= TTTab
STDLIBS		=
LIBRARY		=
SUBDIRS		=

O		= -g
INCLUDES	=
CFLAGS		= $O $(INCLUDES)
FFLAGS		= $O
LDFLAGS		= $O
LFLAGS		= -bchu $(INCLUDES)

LIBDIR1		= ../../lib
LIBDIR2		= 
DESTDIR		= ../../bin

CC		= cc
F77		= f77
LD		= $(F77)
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
AR		= ar
RANLIB		= ranlib
FLAGS		= $(MFLAGS) "CC=$(CC)" "F77=$(F77)" "O=$O" "GET=$(GET)" \
		  "GFLAGS=$(GFLAGS)" "SCLEAN=$(SCLEAN)" "LMODE=$(LMODE)" \
		  "PMODE=$(PMODE)" "PRINT=$(PRINT)" "MAKE=$(MAKE)"

all:		$(PROGRAM)

$(PROGRAM):	$(HDRS) $(OBJS) $(LIBS)
		$(LD) $(LDFLAGS) -o $(PROGRAM) $(OBJS) $(LIBS) $(STDLIBS)

install:	all
		$(INSTALL) -m $(PMODE) $(PROGRAM) $(DESTDIR)

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

pastim.o: ../../include/phtbls.h ../../include/ptabls.h
phaztm.o: ../../include/phtbls.h ../../include/numcon.h ../../include/iaapaq.h
setvel.o: ../../include/ptabls.h
stable.o: ../../include/phtbls.h
trvltm.o: ../../include/phtbls.h ../../include/ptabls.h ../../include/numcon.h
trvtim.o: ../../include/ptabls.h ../../include/numcon.h ../../include/iaapaq.h
ttime.o: ../../include/ptabls.h ../../include/numcon.h
