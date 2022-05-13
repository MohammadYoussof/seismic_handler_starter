#! /bin/csh -f
#
# file shm_size_files.csh
#      ==================
#
# version 1, 14-Oct-2005
#
# Sets window sizes of SHM
# K. Stammler, 14-Oct-2005

if  ("$1" == "")  then
	echo "Usage: $0 <resol>"
	exit
endif

# get parameters
set resol="$1"

if  ("$resol" == "auto")  then
	set res=`$SH_UTIL/get_screen_resolution.csh`
	if  ($#res != 3)  then
		echo "$0 : cant find screen resolution"
		exit
	endif
	set resol=$res[3]
endif

if  ("$resol" == "vga" || "$resol" == "640x480")  then
	set parfile=$SH_INPUTS/shm_dsp_param_xga.dat
	set uidfile=shm_smfnt.uid
	set uidfilew=shm_world_smfnt.uid
else if  ("$resol" == "svga" || "$resol" == "800x600")  then
	set parfile=$SH_INPUTS/shm_dsp_param_xga.dat
	set uidfile=shm_smfnt.uid
	set uidfilew=shm_world_smfnt.uid
else if  ("$resol" == "xga" || "$resol" == "1024x768")  then
	set parfile=$SH_INPUTS/shm_dsp_param_xga.dat
	set uidfile=shm_smfnt.uid
	set uidfilew=shm_world_smfnt.uid
else if  ("$resol" == "wxga" || "$resol" == "1280x800")  then
	set parfile=$SH_INPUTS/shm_dsp_param_wxga.dat
	set uidfile=shm.uid
	set uidfilew=shm_world.uid
else if  ("$resol" == "wxga2" || "$resol" == "1280x768")  then
	set parfile=$SH_INPUTS/shm_dsp_param_wxga.dat
	set uidfile=shm.uid
	set uidfilew=shm_world.uid
else if  ("$resol" == "sxga" || "$resol" == "1280x1024")  then
	set parfile=$SH_INPUTS/shm_dsp_param_sxga.dat
	set uidfile=shm.uid
	set uidfilew=shm_world.uid
else if  ("$resol" == "sxga+" || "$resol" == "1400x1050")  then
	set parfile=$SH_INPUTS/shm_dsp_param_sxga.dat
	set uidfile=shm.uid
	set uidfilew=shm_world.uid
else if  ("$resol" == "uxga" || "$resol" == "1600x1200")  then
	set parfile=$SH_INPUTS/shm_dsp_param_sxga.dat
	set uidfile=shm.uid
	set uidfilew=shm_world.uid
else if  ("$resol" == "qxga" || "$resol" == "2048x1536")  then
	set parfile=$SH_INPUTS/shm_dsp_param_sxga.dat
	set uidfile=shm.uid
	set uidfilew=shm_world.uid
else
	echo "$0 : Unknown resolution $resol.  Abort."
	exit
endif

echo $parfile $uidfile $uidfilew
