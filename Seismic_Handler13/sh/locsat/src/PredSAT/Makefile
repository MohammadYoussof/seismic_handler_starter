#
# PredSAT Makefile
# SccsId:   @(#)Makefile	43.1	9/9/91
#
SRCS		= main_pred.c getargp.c
OBJS		= main_pred.o getargp.o
HDRS		=
LIBS 		= $(LIBDIR1)/libloc.a \
       		  $(LIBDIR1)/librdwrt.a \
	          $(LIBDIR1)/libinterp.a \
       		  $(LIBDIR1)/libxprt2.a \
       		  $(LIBDIR1)/libtime.a \
	          $(LIBDIR1)/libgeog.a
STDLIBS		=
PROGRAM		= PredSAT
LIBRARY		=
SUBDIRS		=

O		= -O
INCS		= ../../include
INCLUDES	= -I$(INCS) -I$(INCS)/db3
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
DEPEND		= makedepend
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

depend:		$(SRCS) $(HDRS) Makefile
		$(DEPEND) $(INCLUDES) $(SRCS)

# DO NOT DELETE THIS LINE -- make depend depends on it.

main_pred.o: /usr/include/stdio.h /usr/include/sys/stdsyms.h
main_pred.o: /usr/include/sys/types.h /usr/include/sys/_inttypes.h
main_pred.o: /usr/include/strings.h /usr/include/ctype.h
main_pred.o: ../../include/aesir.h ../../include/db3/db_origerr.h
main_pred.o: ../../include/db3/db_origin.h ../../include/db3/db_site.h
getargp.o: /usr/include/stdio.h /usr/include/sys/stdsyms.h
getargp.o: /usr/include/sys/types.h /usr/include/sys/_inttypes.h
getargp.o: /usr/include/strings.h
