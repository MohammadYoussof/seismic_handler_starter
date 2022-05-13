# file Makefile
#      ========
#
# version 7, 21-Nov-2005
#
# main Makefile for SH/SHM
# K. Stammler, 19-Feb-96


# without SHM
S_DEPEND_ALL		= sd_source
S_DEPEND_DEPEND	= sd_source_d sd_foreign_d sd_newgraph_d sd_seed_io_d \
							sd_gcf_d sd_util_d
S_DEPEND_CLEAN		= sd_source_c
# including SHM
F_DEPEND_ALL		= sd_source sd_motif sd_img
F_DEPEND_DEPEND	= sd_source_d sd_foreign_d sd_newgraph_d sd_seed_io_d \
					sd_gcf_d sd_util_d sd_motif_d sd_img_d
F_DEPEND_CLEAN		= sd_source_c sd_motif_c sd_img_c sd_gcf_c



#
# only command line version SH
#
DEPEND_ALL		= $(S_DEPEND_ALL)
DEPEND_DEPEND	= $(S_DEPEND_DEPEND)
DEPEND_CLEAN	= $(S_DEPEND_CLEAN)
#
#
# including SHM
#
#DEPEND_ALL		= $(F_DEPEND_ALL)
#DEPEND_DEPEND	= $(F_DEPEND_DEPEND)
#DEPEND_CLEAN	= $(F_DEPEND_CLEAN)
#

all: $(DEPEND_ALL)

depend: $(DEPEND_DEPEND)

clean: $(DEPEND_CLEAN)

shm: $(F_DEPEND_ALL)

depend_shm: $(F_DEPEND_DEPEND)

clean_shm: $(F_DEPEND_CLEAN)

world:
	@cp shc shc_world; \
	if  [ -f source/motif/shm ]; \
		then \
		cp source/motif/shm source/motif/shm_world; \
		cp source/motif/shm.uid source/motif/shm_world.uid; \
		cp source/motif/shm_smfnt.uid source/motif/shm_world_smfnt.uid; \
	fi



# compiling

sd_source:
	cd source; make

sd_foreign:
	cd source/foreign; make

sd_newgraph:
	cd source/newgraph; make

sd_seed_io:
	cd source/seed_io; make

sd_gcf:
	cd source/gcf; make

sd_util:
	cd util; make

sd_motif:
	cd source/motif; make

sd_img:
	cd source/img; make



# depend

sd_source_d:
	cd source; make depend

sd_foreign_d:
	cd source/foreign; make depend

sd_newgraph_d:
	cd source/newgraph; make depend

sd_seed_io_d:
	cd source/seed_io; make depend

sd_gcf_d:
	cd source/gcf; make depend

sd_util_d:
	cd util; make depend

sd_motif_d:
	cd source/motif; make depend

sd_img_d:
	cd source/img; make depend



# clean

sd_source_c:
	cd source; make clean

sd_foreign_c:
	cd source/foreign; make clean

sd_newgraph_c:
	cd source/newgraph; make clean

sd_seed_io_c:
	cd source/seed_io; make clean

sd_gcf_c:
	cd source/gcf; make clean

sd_util_c:
	cd util; make clean

sd_motif_c:
	cd source/motif; make clean

sd_img_c:
	cd source/img; make clean

# DO NOT DELETE THIS LINE -- make depend depends on it.
