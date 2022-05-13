#
# Makefile for libsrc
# SccsId:  @(#)Makefile	42.1	8/23/91
#
SRCS		=
OBJS		=
HDRS		=
LIBS		=
STDLIBS		=
PROGRAM		=
LIBRARY		=
SUBDIRS		= \
		  libgeog libinterp libloc librdwrt libtime libxprt2

# Note that libcf depends upon libaesir & libdb30

O		= -g
INCLUDES	=
CFLAGS		= $O $(INCLUDES)
#FFLAGS		= $O +e +es +ppu
FFLAGS		= $O
LDFLAGS		= $O
LFLAGS		= $(INCLUDES)

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
FLAGS		= $(MFLAGS) "F77=$(F77)" "O=$O" "FFLAGS=$(FFLAGS)" \
		  "GET=$(GET)" "GFLAGS=$(GFLAGS)" "SCLEAN=$(SCLEAN)" \
		  "LMODE=$(LMODE)" "PMODE=$(PMODE)" \
		  "PRINT=$(PRINT)" "MAKE=$(MAKE)"

all:		subdir

subdir:
		@for dir in $(SUBDIRS); do \
			(cd $$dir; echo -n "*** " ; pwd; date; \
			$(MAKE) $(FLAGS) || exit 1); \
		done

install:
		-@for dir in $(SUBDIRS); do \
			(cd $$dir; echo -n "*** " ; pwd; date; \
			$(MAKE) $(FLAGS) install); \
		done

clean:
		-@for dir in $(SUBDIRS); do \
			(cd $$dir; echo -n "*** " ; pwd; date; \
			$(MAKE) $(FLAGS) clean); \
		done

xclean:
		-@for dir in $(SUBDIRS); do \
			(cd $$dir; echo -n "*** " ; pwd; date; \
			$(MAKE) $(FLAGS) xclean); \
		done
		-$(SCLEAN)
		
lint:
		-@for dir in $(SUBDIRS); do \
			(cd $$dir; echo -n "*** " ; pwd; date; \
			$(MAKE) $(FLAGS) lint); \
		done

print:
		-@for dir in $(SUBDIRS); do \
			(cd $$dir; echo -n "*** " ; pwd; date; \
			$(MAKE) $(FLAGS) print); \
		done

sccs:
		-@for dir in $(SUBDIRS); do \
			(cd $$dir; echo -n "*** " ; pwd; date; \
			$(MAKE) $(FLAGS) sccs); \
		done

depend:
		-@for dir in $(SUBDIRS); do \
			(cd $$dir; echo -n "*** " ; pwd; date; \
			$(MAKE) $(FLAGS) depend); \
		done

###

