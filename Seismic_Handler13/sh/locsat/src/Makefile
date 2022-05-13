#
# SccsId:  @(#)Makefile	43.1	9/9/91
#
#	Makefile for src
#
SRCS		=
OBJS		=
HDRS		=
LIBS		=
STDLIBS		=
PROGRAM		=
LIBRARY		=
SUBDIRS		= LocSAT # PredSAT TTTab
O		= -O
INCLUDES	=
CFLAGS		= $O $(INCLUDES)
FFLAGS		= $O +e +es +ppu				# Fortran flags
LDFLAGS		= $O				# Linker flags
LFLAGS		= $(INCLUDES)			# Lint flags

CC		= cc
F77		= g77
LD		= $(CC)
LINT		= lint
YACC		= yacc
LEX		= lex

GET		= sccs get
GFLAGS		=
SCLEAN		= sccs clean

RM		= rm
INSTALL		= install
MAKE		= make
PRINT		= lpr -p
DEPEND		= mkmf
AR		= ar
RANLIB		= ranlib
FLAGS		= $(MFLAGS) "F77=$(F77)" "O=$O" \
		  "GET=$(GET)" "GFLAGS=$(GFLAGS)" "SCLEAN=$(SCLEAN)" \
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
# had install || echo -1 and removed - from for statement

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

