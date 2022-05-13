#! /bin/csh
#
# file shm_configure.csh
#      =================
#
# version 20, 05-Jul-2005
#
# Writes private SHM_USER_STARTUP.SHC and modifies $HOME/.Xresources file
# for tailoring SHM parameters.
#
# The scripts reads in default values of the parameters from possibly
# existing files $SH_USERROOT/private/SHM_USER_STARTUP.SHC and $HOME/.Xdefaults
# or $HOME/.Xresources except for parameters which are marked by (*).
# If these files are not found the script takes its own default values.
# Parameters marked with (*) are not found in the input files and are
# always set to the same default values when starting shm_configure.csh.
# When leaving the script menu shm_configure.csh DELETES an already
# existing SHM_USER_STARTUP.SHC file and replaces it by its own output.
# All entries starting with 'SHM*' are removed in the X-resource file
# (.Xdefaults or .Xresources) and are replaced by the settings made here.
# To load the changed X-resources xrdb is called.
#
# This script is called from SHM by the 'Configure ...' menu entry.  However,
# changes of parameters marked with (!) are effective only after restart
# of the program.
# 
# K. Stammler, 27-Dec-96
#
# list of parameters (pars marked with (*) do not read defaults from file):
#
# name                f  default           range
# DEPTH_PHASE_LIST       pP,sP,pS,sS       comma separated phase list
# THEO_PHASE_LIST     !  P,S,pP,pS,sP,sS,ScS,PcP,PP,SS  --"--
# DIFF_PHASE_LIST        S-P,PP-P,Pg-Pn    comma separated phase difference list
# CLOSE_PHASE_RESOL      8                 integer 1-??
# MIN_DRAG_BOX_WIDTH     7                 integer 1-??
# DEFPATH_FILTER         $SH_FILTER        path
# DEFPATH_EVENTS      !  $HOME/evtout      path
# DEFPATH_GSE         !  $HOME             path
# DEFPATH_GSE2        !  $HOME             path
# DEFPATH_AH          !  $HOME             path
# DEFPATH_Q           !  $HOME             path
# DEFPATH_EVID           $HOME/evid        path
# DEFPATH_EVTOUT         $HOME/evtout      path
# DEFPATH_DATA        !  $HOME             path
# TOP_DOWN_ORDER         FALSE             TRUE FALSE
# DOUBLE_CLICK_TIME      200               integer 1-??
# TRACE_ZOOM_BASE        10.0              float > 0.0
# TRACE_ZOOM_EXP         3.3333333         float > 0.0
# COLOR_MARK          !  1.0,0.0,0.0       3 floats between 0 and 1
# COLOR_THEO          !  0.0,1.0,0.0       3 floats between 0 and 1
# COLOR_AUTO          !  1.0,0.0,0.5       3 floats between 0 and 1
# COLOR_CRSR          !  0.0,0.0,1.0       3 floats between 0 and 1
# REFSTATION             GRA1              string
# AUTOPICK_FIRST         TRUE              TRUE FALSE
# EDIT_CMD            *  default           string (*)
# CALIB_WDW_WIDTH        2.5               float > 0.0
# CALIB_WDW_HEIGHT       2.5               float > 0.0
# CALIB_AZIMUTH_GRID     10.0              float > 0.0
# CALIB_SLOWNESS_GRID    0.5               float > 0.0
# AUTO_SCALING           ON                ON OFF
# USE_REC_FILTERS        TRUE              TRUE FALSE
# PROMPT_ANALYST      !  FALSE             TRUE FALSE
# MOTIF_LOG           !  ;;                ;; or filename
# PHASE_AMPL_DIFF        60.0              float
# REVERSE_XORS        !  FALSE             TRUE FALSE
# X_MAX_DRAWLTH       !  4096              integer 1-??
# DEFAULT_QUALITY     !  2                 integer 1-9
# DRAG_BOX_RUBBER_VAL    10                integer 1-??
# DEFAULT_FILTER      !  ;;                string
# DEFAULT_PHASE_TYPE  !  OTHER             OTHER TELE_QUAKE NUCLEAR REGIO_QUAKE
#                                          LOCAL_QUAKE BLAST MINING
# DEFAULT_DEPTH_TYPE  !  UNDEFINED         UNDEFINED PRESET ESTIMATED FREE POOR
#                                          LESSWELL RELIABLE EXTERNAL
# DEFAULT_DEPTH       !  33.0              float
# DEFAULT_LOC_QUALITY !  UNDEFINED         UNDEFINED TOOWEAK INCOHERENT
#                                          NOBEARING REGION RELIABLE
# MAX_CURSOR_FORM        WAVEFORM_HILB     CROSSHAIR WAVEFORM WAVEFORM_NEG
#                                          WAVEFORM_HILB WAVEFORM_NEGHILB
# EVENT_CHECK_PROC       shm_exec_check_event.csh   program
# SCREENDUMP_PROC        sd_xwd.csh        program
# EVTVIEW_PROC           ev2view           program
# REMREQHOST             ersn06            hostname
# FULL_PHASE_NAMES       FALSE             TRUE FALSE
# DEFAULT_SOURCE         SZGRF             string
# MOVE_WDW_STEP          1.0               float > 0.0
# TOP_MARGIN          !  20                integer 1-??
# AUTO_PHASE             beam              string
# JUKEBOX_ROOT           /jukebox          path
# DRAW_AREA_WIDTH        896               integer > 0
# DRAW_AREA_HEIGHT       593               integer > 0
# AREA_ZOOM_BASE         10.0              float > 0.0
# AREA_ZOOM_EXP          25.0              float > 0.0
# DEFAULT_PHASE_FLAGS    CLEAR             CLEAR CALIB IGNORE TELEX_ALL
# REFORMAT_PROC          undefined         program
# FINAL_PROC             undefined         program
# ML_SIGMA            !  default           string
# MINMAXFMT              <empty>           string
# OWN_ACCELERATORS       TRUE              TRUE FALSE
#
# Values controlled by X-Resources:
# LOCSAT_PREFIX          tab               string
# FK_FRQLO               0.4               float > 0.0
# FK_FRQHI               3.0               float > 0.0
# FK_SLOWNESS            15                float > 0.0
# FK_RESOL               51                integer > 2
# FK_COLNUM              10                integer > 1
# FILTER_BUTPAR_HI       5Hz               frq
# FILTER_BUTPAR_LO       100s              frq
# FILTER_BUTPAR_ORDER    3                 integer > 0
# OPICK_THRESHOLD        10.0              float > 0.0
# OPICK_DURATION         1.0               float > 0.0
# OPICK_BREAK            0.0               float >= 0.0

#set echo

# find xresource
if  (-e $HOME/.Xdefaults)  then
	set xresfile=$HOME/.Xdefaults
else if  (-e $HOME/.Xresources)  then
	set xresfile=$HOME/.Xresources
else
	set xresfile=$HOME/.Xdefaults
endif

# set startup file
set startupfile=$SH_USERROOT/private/SHM_USER_STARTUP.SHC

# set defaults
set depth_phase_list="pP,sP,pS,sS"
set theo_phase_list="P,S,pP,pS,sP,sS,ScS,PcP,PP,SS"
set diff_phase_list="S-P,PP-P,Pg-Pn"
set close_phase_resol="8"
set min_drag_box_width="7"
set defpath_filter=$SH_FILTER
set defpath_events=$HOME/evtout/
set defpath_gse=$HOME
set defpath_gse2=$HOME
set defpath_ah=$HOME
set defpath_q=$HOME
set defpath_evid=$HOME/evid/
set defpath_evtout=$HOME/evtout/
set defpath_data=$HOME
set top_down_order=FALSE
set double_click_time=200
set trace_zoom_base="10.0"
set trace_zoom_exp="3.33333333"
set color_mark="1.0,0.0,0.0"
set color_theo="0.0,1.0,0.0"
set color_auto="1.0,0.0,0.5"
set color_crsr="0.0,0.0,1.0"
set refstation=GRA1
set autopick_first=TRUE
set edit_cmd="default"
set calib_wdw_width="2.5"
set calib_wdw_height="2.5"
set calib_azimuth_grid="10.0"
set calib_slowness_grid="0.5"
set auto_scaling=ON
set use_rec_filters=TRUE
set prompt_analyst=FALSE
set motif_log=";;"
set phase_ampl_diff="60.0"
set reverse_xors=FALSE
set x_max_drawlth=4096
set default_quality=2
set drag_box_rubber_val=10
set default_filter=";;"
set default_phase_type="OTHER"
set default_depth_type="UNDEFINED"
set default_depth="33.0"
set default_loc_quality="UNDEFINED"
set max_cursor_form="WAVEFORM_HILB"
set event_check_proc=$SH_UTIL/shm_exec_check_event.csh
set screendump_proc=$SH_UTIL/sd_xwd.csh
set evtview_proc=ev2view
set remreqhost="ersn06"
set full_phase_names="FALSE"
set default_source="SZGRF"
set move_wdw_step="1.0"
set top_margin=20
set auto_phase="beam"
#set jukebox_root="/jukebox"
set draw_area_width=896
set draw_area_height=593
set area_zoom_base="10.0"
set area_zoom_exp="25.0"
set default_phase_flags="CLEAR"
set reformat_proc="undefined"
set final_proc="undefined"
set ml_sigma="default"
set minmaxfmt="none"
#set own_accelerators="TRUE"

# X-Resources
set locsat_prefix="tab"
set fk_frqlo="0.4"
set fk_frqhi="3.0"
set fk_slowness="15"
set fk_resol="51"
set fk_colnum="10"
set filter_butpar_hi="5Hz"
set filter_butpar_lo="100s"
set filter_butpar_order="3"
set opick_thresh="10.0"
set opick_duration="1.0"
set opick_break="0.00"

# read defaults from startupfile if exist
if  (-e $startupfile)  then
	set res=`grep DEPTH_PHASE_LIST $startupfile`
	if  ("$res" != "")  then
		set depth_phase_list=$res[3]
	endif
	set res=`grep THEO_PHASE_LIST $startupfile`
	if  ("$res" != "")  then
		set theo_phase_list=$res[3]
	endif
	set res=`grep DIFF_PHASE_LIST $startupfile`
	if  ("$res" != "")  then
		set diff_phase_list=$res[3]
	endif
	set res=`grep CLOSE_PHASE_RESOL $startupfile`
	if  ("$res" != "")  then
		set close_phase_resol=$res[3]
	endif
	set res=`grep MIN_DRAG_BOX_WIDTH $startupfile`
	if  ("$res" != "")  then
		set min_drag_box_width=$res[3]
	endif
	set res=`grep -n DEFPATH_FILTER $startupfile`
	if  ("$res" != "")  then
		set res=`echo $res | sed 's/:/ /'`
		@ res = $res[1] - 1
		set res=`sed -n $res"p" $startupfile | sed 's/\\/\//g'`
		set defpath_filter=$res[5]
	endif
	set res=`grep -n DEFPATH_EVENTS $startupfile`
	if  ("$res" != "")  then
		set res=`echo $res | sed 's/:/ /'`
		@ res = $res[1] - 1
		set res=`sed -n $res"p" $startupfile | sed 's/\\/\//g'`
		set defpath_events=$res[5]
	endif
	set res=`grep -n DEFPATH_GSE $startupfile`
	if  ("$res" != "")  then
		set res=`echo $res | sed 's/:/ /'`
		@ res = $res[1] - 1
		set res=`sed -n $res"p" $startupfile | sed 's/\\/\//g'`
		set defpath_gse=$res[5]
	endif
	set res=`grep -n DEFPATH_GSE2 $startupfile`
	if  ("$res" != "")  then
		set res=`echo $res | sed 's/:/ /'`
		@ res = $res[1] - 1
		set res=`sed -n $res"p" $startupfile | sed 's/\\/\//g'`
		set defpath_gse2=$res[5]
	endif
	set res=`grep -n DEFPATH_AH $startupfile`
	if  ("$res" != "")  then
		set res=`echo $res | sed 's/:/ /'`
		@ res = $res[1] - 1
		set res=`sed -n $res"p" $startupfile | sed 's/\\/\//g'`
		set defpath_ah=$res[5]
	endif
	set res=`grep -n DEFPATH_Q $startupfile`
	if  ("$res" != "")  then
		set res=`echo $res | sed 's/:/ /'`
		@ res = $res[1] - 1
		set res=`sed -n $res"p" $startupfile | sed 's/\\/\//g'`
		set defpath_q=$res[5]
	endif
	set res=`grep -n DEFPATH_EVID $startupfile`
	if  ("$res" != "")  then
		set res=`echo $res | sed 's/:/ /'`
		@ res = $res[1] - 1
		set res=`sed -n $res"p" $startupfile | sed 's/\\/\//g'`
		set defpath_evid=$res[5]
	endif
	set res=`grep -n DEFPATH_EVTOUT $startupfile`
	if  ("$res" != "")  then
		set res=`echo $res | sed 's/:/ /'`
		@ res = $res[1] - 1
		set res=`sed -n $res"p" $startupfile | sed 's/\\/\//g'`
		set defpath_evtout=$res[5]
	endif
	set res=`grep -n DEFPATH_DATA $startupfile`
	if  ("$res" != "")  then
		set res=`echo $res | sed 's/:/ /'`
		@ res = $res[1] - 1
		set res=`sed -n $res"p" $startupfile | sed 's/\\/\//g'`
		set defpath_data=$res[5]
	endif
	set res=`grep -i TOP_DOWN_ORDER $startupfile`
	if  ("$res" != "")  then
		set top_down_order=$res[3]
	endif
	set res=`grep -i DOUBLE_CLICK_TIME $startupfile`
	if  ("$res" != "")  then
		set double_click_time=$res[3]
	endif
	set res=`grep -i TRACE_ZOOM_BASE $startupfile`
	if  ("$res" != "")  then
		set trace_zoom_base=$res[3]
	endif
	set res=`grep -i TRACE_ZOOM_EXP $startupfile`
	if  ("$res" != "")  then
		set trace_zoom_exp=$res[3]
	endif
	set res=`grep -i COLOR_MARK $startupfile`
	if  ("$res" != "")  then
		set color_mark=$res[3]
	endif
	set res=`grep -i COLOR_THEO $startupfile`
	if  ("$res" != "")  then
		set color_theo=$res[3]
	endif
	set res=`grep -i COLOR_AUTO $startupfile`
	if  ("$res" != "")  then
		set color_auto=$res[3]
	endif
	set res=`grep -i COLOR_CRSR $startupfile`
	if  ("$res" != "")  then
		set color_crsr=$res[3]
	endif
	set res=`grep -i REFSTATION $startupfile`
	if  ("$res" != "")  then
		set refstation=$res[3]
	endif
	set res=`grep -i AUTOPICK_FIRST $startupfile`
	if  ("$res" != "")  then
		set autopick_first=$res[3]
	endif
#	set res=`grep EDIT_CMD $startupfile`
#	if  ("$res" != "")  then
#		set edit_cmd=$res[3]
#	endif
	set res=`grep -i CALIB_WDW_WIDTH $startupfile`
	if  ("$res" != "")  then
		set calib_wdw_width=$res[3]
	endif
	set res=`grep -i CALIB_WDW_HEIGHT $startupfile`
	if  ("$res" != "")  then
		set calib_wdw_height=$res[3]
	endif
	set res=`grep -i CALIB_AZIMUTH_GRID $startupfile`
	if  ("$res" != "")  then
		set calib_azimuth_grid=$res[3]
	endif
	set res=`grep -i CALIB_SLOWNESS_GRID $startupfile`
	if  ("$res" != "")  then
		set calib_slowness_grid=$res[3]
	endif
	set res=`grep -i shm_cmd_autoscale $startupfile`
	if  ("$res" != "")  then
		set auto_scaling=$res[2]
	endif
	set res=`grep -i USE_REC_FILTERS $startupfile`
	if  ("$res" != "")  then
		set use_rec_filters=$res[3]
	endif
	set res=`grep -i PROMPT_ANALYST $startupfile`
	if  ("$res" != "")  then
		set prompt_analyst=$res[3]
	endif
	set res=`grep -i MOTIF_LOG $startupfile`
	if  ("$res" != "")  then
		set motif_log=$res[3]
	endif
	set res=`grep -i PHASE_AMPL_DIFF $startupfile`
	if  ("$res" != "")  then
		set phase_ampl_diff=$res[3]
	endif
	set res=`grep -i REVERSE_XORS $startupfile`
	if  ("$res" != "")  then
		set reverse_xors=$res[3]
	endif
	set res=`grep -i X_MAX_DRAWLTH $startupfile`
	if  ("$res" != "")  then
		set x_max_drawlth=$res[3]
	endif
	set res=`grep -i DEFAULT_QUALITY $startupfile`
	if  ("$res" != "")  then
		set default_quality=$res[3]
	endif
	set res=`grep -i DRAG_BOX_RUBBER_VAL $startupfile`
	if  ("$res" != "")  then
		set drag_box_rubber_val=$res[3]
	endif
	set res=`grep -i DEFAULT_FILTER $startupfile`
	if  ("$res" != "")  then
		set default_filter=$res[3]
	endif
	set res=`grep -i DEFAULT_PHASE_TYPE $startupfile`
	if  ("$res" != "")  then
		set default_phase_type=$res[3]
	endif
	set res=`grep -i DEFAULT_DEPTH_TYPE $startupfile`
	if  ("$res" != "")  then
		set default_depth_type=$res[3]
	endif
	set res=`grep -i 'DEFAULT_DEPTH ' $startupfile`
	if  ("$res" != "")  then
		set default_depth=$res[3]
	endif
	set res=`grep -i DEFAULT_LOC_QUALITY $startupfile`
	if  ("$res" != "")  then
		set default_loc_quality=$res[3]
	endif
	set res=`grep -i MAX_CURSOR_FORM $startupfile`
	if  ("$res" != "")  then
		set max_cursor_form=$res[3]
	endif
	set res=`grep -n EVENT_CHECK_PROC $startupfile`
	if  ("$res" != "")  then
		set res=`echo $res | sed 's/:/ /'`
		@ res = $res[1] - 1
		set res=`sed -n $res"p" $startupfile | sed 's/\\/\//g'`
		set event_check_proc=$res[5]
	endif
	set res=`grep -n SCREENDUMP_PROC $startupfile`
	if  ("$res" != "")  then
		set res=`echo $res | sed 's/:/ /'`
		@ res = $res[1] - 1
		set res=`sed -n $res"p" $startupfile | sed 's/\\/\//g'`
		set screendump_proc=$res[5]
	endif
	set res=`grep -n EVTVIEW_PROC $startupfile`
	if  ("$res" != "")  then
		set res=`echo $res | sed 's/:/ /'`
		@ res = $res[1] - 1
		set res=`sed -n $res"p" $startupfile | sed 's/\\/\//g'`
		set evtview_proc=$res[5]
	endif
	set res=`grep -i REMREQHOST $startupfile`
	if  ("$res" != "")  then
		set remreqhost=$res[3]
	endif
	set res=`grep -i FULL_PHASE_NAMES $startupfile`
	if  ("$res" != "")  then
		set full_phase_names=$res[3]
	endif
	set res=`grep -i DEFAULT_SOURCE $startupfile`
	if  ("$res" != "")  then
		set default_source=$res[3]
	endif
	set res=`grep -i MOVE_WDW_STEP $startupfile`
	if  ("$res" != "")  then
		set move_wdw_step=$res[3]
	endif
	set res=`grep -i TOP_MARGIN $startupfile`
	if  ("$res" != "")  then
		set top_margin=$res[3]
	endif
	set res=`grep -i AUTO_PHASE $startupfile`
	if  ("$res" != "")  then
		set auto_phase=$res[3]
	endif
#	set res=`grep -i JUKEBOX $startupfile`
#	if  ("$res" != "")  then
#		set jukebox_root=$res[3]
#	endif
	set res=`grep -i DRAW_AREA_WIDTH $startupfile`
	if  ("$res" != "")  then
		set draw_area_width=$res[3]
	endif
	set res=`grep -i DRAW_AREA_HEIGHT $startupfile`
	if  ("$res" != "")  then
		set draw_area_height=$res[3]
	endif
	set res=`grep -i AREA_ZOOM_BASE $startupfile`
	if  ("$res" != "")  then
		set area_zoom_base=$res[3]
	endif
	set res=`grep -i AREA_ZOOM_EXP $startupfile`
	if  ("$res" != "")  then
		set area_zoom_exp=$res[3]
	endif
	set res=`grep -i DEFAULT_PHASE_FLAGS $startupfile | tail -1`
	if  ("$res" != "")  then
		set default_phase_flags=$res[3]
	endif
	set res=`grep -n REFORMAT_PROC $startupfile`
	if  ("$res" != "")  then
		set res=`echo $res | sed 's/:/ /'`
		@ res = $res[1] - 1
		set res=`sed -n $res"p" $startupfile | sed 's/\\/\//g'`
		set reformat_proc=$res[5]
	endif
	set res=`grep -n FINAL_PROC $startupfile`
	if  ("$res" != "")  then
		set res=`echo $res | sed 's/:/ /'`
		@ res = $res[1] - 1
		set res=`sed -n $res"p" $startupfile | sed 's/\\/\//g'`
		set final_proc=$res[5]
	endif
	set res=`grep -i table_ml_sigma $startupfile | sed 's/:/ /'`
	if  ("$res" != "")  then
		set ml_sigma=$res[3]
	endif
	set res=`grep -i minmaxfmt $startupfile | sed 's/:/ /'`
	if  ("$res" != "")  then
		if  ($#res > 2)  then
			set minmaxfmt=$res[3]
		else
			set minmaxfmt="none"
		endif
	endif
#	set res=`grep -i OWN_ACCELERATORS $startupfile`
#	if  ("$res" != "")  then
#		set own_accelerators=$res[3]
#	endif
endif

# read defaults from X-Resource
set noglob
if  (-e $xresfile)  then
	set res=`grep 'SHM.locsat_prefix_text.value' $xresfile`
	if  ("$res" != "")  then
		set locsat_prefix=$res[2]
	endif
	set res=`grep 'SHM.fk_frqlo_text.value' $xresfile`
	if  ("$res" != "")  then
		set fk_frqlo=$res[2]
	endif
	set res=`grep 'SHM.fk_frqhi_text.value' $xresfile`
	if  ("$res" != "")  then
		set fk_frqhi=$res[2]
	endif
	set res=`grep 'SHM.fk_slowness_text.value' $xresfile`
	if  ("$res" != "")  then
		set fk_slowness=$res[2]
	endif
	set res=`grep 'SHM.fk_resol_text.value' $xresfile`
	if  ("$res" != "")  then
		set fk_resol=$res[2]
	endif
	set res=`grep 'SHM.fk_colnum_text.value' $xresfile`
	if  ("$res" != "")  then
		set fk_colnum=$res[2]
	endif
	set res=`grep 'SHM.filter_butpar_hi_text.value' $xresfile`
	if  ("$res" != "")  then
		set filter_butpar_hi=$res[2]
	endif
	set res=`grep 'SHM.filter_butpar_lo_text.value' $xresfile`
	if  ("$res" != "")  then
		set filter_butpar_lo=$res[2]
	endif
	set res=`grep 'SHM.filter_butpar_order_text.value' $xresfile`
	if  ("$res" != "")  then
		set filter_butpar_order=$res[2]
	endif
	set res=`grep 'SHM.opick_thresh_text.value' $xresfile`
	if  ("$res" != "")  then
		set opick_thresh=$res[2]
	endif
	set res=`grep 'SHM.opick_duration_text.value' $xresfile`
	if  ("$res" != "")  then
		set opick_duration=$res[2]
	endif
	set res=`grep 'SHM.opick_break_text.value' $xresfile`
	if  ("$res" != "")  then
		set opick_break=$res[2]
	endif
endif
unset noglob

# display menu
set cmd=0
set value=""
while  ("$cmd" != "q" && "$cmd" != "x")
	echo ""
	echo " (1)   depth phase list:         $depth_phase_list"
	echo " (2) ! theo phase list:          $theo_phase_list"
	echo " (3)   diff phase list:          $diff_phase_list"
	echo " (4)   close phase resol (pixel):   $close_phase_resol"
	echo " (5)   min dragbox width (pixel):   $min_drag_box_width"
	echo " (6)   default filter path:      $defpath_filter"
	echo " (7) ! default event path:       $defpath_events"
	echo " (8) ! default gse1.0 path:      $defpath_gse"
	echo " (9) ! default gse2.0 path:      $defpath_gse2"
	echo "(10) ! default AH path:          $defpath_ah"
	echo "(70) ! default Q path:           $defpath_q"
	echo "(11)   default evid path:        $defpath_evid"
	echo "(12)   default evtout path:      $defpath_evtout"
	echo "(13) ! default Mini-SEED path:   $defpath_data"
	echo "(14)   top-down order:           $top_down_order"
	echo "(15)   double click time (ms):   $double_click_time"
	echo "(16)   trace zoom base:          $trace_zoom_base"
	echo "(17)   trace zoom exponent:      $trace_zoom_exp"
	echo "(18) ! color of handpicked p.:   $color_mark"
	echo "(19) ! color of theoretical p.:  $color_theo"
	echo "(20) ! color of autopicked p.:   $color_auto"
	echo "(21) ! color of cursor:          $color_crsr"
	echo "(22)   reference station:        $refstation"
	echo "(23)   autopick - first instead of largest:     $autopick_first"
	echo "(24) * edit command:             $edit_cmd"
	echo "(25)   calib wdw width:          $calib_wdw_width"
	echo "(26)   calib wdw height:         $calib_wdw_height"
	echo "(27)   calib azimuth grid:       $calib_azimuth_grid"
	echo "(28)   calib slowness grid:      $calib_slowness_grid"
	echo "(29)   auto scaling of traces:   $auto_scaling"
	echo "(30)   use recursive filters:    $use_rec_filters"
	echo "(31) ! prompt analyst:           $prompt_analyst"
	echo "(32) ! motif action log (dbg):   $motif_log"
	echo "(33)   max ampl dist from phase: $phase_ampl_diff"
	echo "(34) ! reverse XORs:             $reverse_xors"
	echo "(35) ! X-max drawlth:            $x_max_drawlth"
	echo "(36) ! default_quality:          $default_quality"
	echo "(37) ! drag box rubber value:    $drag_box_rubber_val"
	echo "(38) ! default filter:           $default_filter"
	echo "(39) ! default event type:       $default_phase_type"
	echo "(40) ! default depth type:       $default_depth_type"
	echo "(41) ! default depth (km):       $default_depth"
	echo "(42) ! default location quality: $default_loc_quality"
	echo "(43)   max. cursor form:         $max_cursor_form"
	echo "(44)   event check proc:         $event_check_proc"
	echo "(45)   screendump proc:          $screendump_proc"
	echo "(46)   evt view proc:            $evtview_proc"
	echo "(47)   remote request host:      $remreqhost"
	echo "(48)   use full phase names:     $full_phase_names"
	echo "(49)   default source:           $default_source"
	echo "(50)   move time window step:    $move_wdw_step"
	echo "(51) ! top margin (pixel):       $top_margin"
	echo "(52)   LocSAT t-tables prefix:   $locsat_prefix"
	echo "(53)   FK low frq limit:         $fk_frqlo"
	echo "(54)   FK high frq limit:        $fk_frqhi"
	echo "(55)   FK slowness limit:        $fk_slowness"
	echo "(56)   FK resolution:            $fk_resol"
	echo "(57)   FK number of colors:      $fk_colnum"
	echo "(58)   But-Fil default low frq:  $filter_butpar_lo"
	echo "(59)   But-Fil default high frq: $filter_butpar_hi"
	echo "(60)   But-Fil default order:    $filter_butpar_order"
	echo "(61)   Name of autopick phase:   $auto_phase"
#	echo "(62)   Jukebox root path:        $jukebox_root"
	echo "(63)   Drawing area width:       $draw_area_width"
	echo "(64)   Drawing area height:      $draw_area_height"
	echo "(65)   Drawing area zoom base:   $area_zoom_base"
	echo "(66)   Drawing area zoom exp.:   $area_zoom_exp"
	echo "(67)   Onset Pick Threshold:     $opick_thresh"
	echo "(68)   Onset Pick Duration:      $opick_duration"
	echo "(69)   Onset Pick Break:         $opick_break"
	echo "(71)   Default Phase Flags:      $default_phase_flags"
	echo "(72)   Reformat proc:            $reformat_proc"
	echo "(73)   Final proc:               $final_proc"
	echo "(74)   ml Sigma:                 $ml_sigma"
	echo "(75)   min/max format in s-wdw:  $minmaxfmt"
#	echo "(76)   use own accelerators      $own_accelerators"
	echo ""
	echo "! = needs restart of program, * = default not read from file"
	echo -n "enter selection (q exits and writes startup file): "
	set cmd=$<
	if  ("$cmd" != "x" && "$cmd" != "q")  then
		echo -n "          value: "
		set value="$<"
	endif
	if  ($cmd == 1)   set depth_phase_list=$value
	if  ($cmd == 2)   set theo_phase_list=$value
	if  ($cmd == 3)   set diff_phase_list=$value
	if  ($cmd == 4)   set close_phase_resol=$value
	if  ($cmd == 5)   set min_drag_box_width=$value
	if  ($cmd == 6)   set defpath_filter=$value
	if  ($cmd == 7)   set defpath_events=$value
	if  ($cmd == 8)   set defpath_gse=$value
	if  ($cmd == 9)   set defpath_gse2=$value
	if  ($cmd == 10)  set defpath_ah=$value
	if  ($cmd == 70)  set defpath_q=$value
	if  ($cmd == 11)  set defpath_evid=$value
	if  ($cmd == 12)  set defpath_evtout=$value
	if  ($cmd == 13)  set defpath_data=$value
	if  ($cmd == 14)  set top_down_order=$value
	if  ($cmd == 15)  set double_click_time=$value
	if  ($cmd == 16)  set trace_zoom_base=$value
	if  ($cmd == 17)  set trace_zoom_exp=$value
	if  ($cmd == 18)  set color_mark=$value
	if  ($cmd == 19)  set color_theo=$value
	if  ($cmd == 20)  set color_auto=$value
	if  ($cmd == 21)  set color_crsr=$value
	if  ($cmd == 22)  set refstation=$value
	if  ($cmd == 23)  set autopick_first=$value
	if  ($cmd == 24)  set edit_cmd=$value
	if  ($cmd == 25)  set calib_wdw_width=$value
	if  ($cmd == 26)  set calib_wdw_height=$value
	if  ($cmd == 27)  set calib_azimuth_grid=$value
	if  ($cmd == 28)  set calib_slowness_grid=$value
	if  ($cmd == 29)  set auto_scaling=$value
	if  ($cmd == 30)  set use_rec_filters=$value
	if  ($cmd == 31)  set prompt_analyst=$value
	if  ($cmd == 32)  set motif_log=$value
	if  ($cmd == 33)  set phase_ampl_diff=$value
	if  ($cmd == 34)  set reverse_xors=$value
	if  ($cmd == 35)  set x_max_drawlth=$value
	if  ($cmd == 36)  set default_quality=$value
	if  ($cmd == 37)  set drag_box_rubber_val=$value
	if  ($cmd == 38)  set default_filter=$value
	if  ($cmd == 39)  set default_phase_type=$value
	if  ($cmd == 40)  set default_depth_type=$value
	if  ($cmd == 41)  set default_depth=$value
	if  ($cmd == 42)  set default_loc_quality=$value
	if  ($cmd == 43)  set max_cursor_form=$value
	if  ($cmd == 44)  set event_check_proc=$value
	if  ($cmd == 45)  set screendump_proc=$value
	if  ($cmd == 46)  set evtview_proc=$value
	if  ($cmd == 47)  set remreqhost=$value
	if  ($cmd == 48)  set full_phase_names=$value
	if  ($cmd == 49)  set default_source=$value
	if  ($cmd == 50)  set move_wdw_step=$value
	if  ($cmd == 51)  set top_margin=$value
	if  ($cmd == 52)  set locsat_prefix=$value
	if  ($cmd == 53)  set fk_frqlo=$value
	if  ($cmd == 54)  set fk_frqhi=$value
	if  ($cmd == 55)  set fk_slowness=$value
	if  ($cmd == 56)  set fk_resol=$value
	if  ($cmd == 57)  set fk_colnum=$value
	if  ($cmd == 58)  set filter_butpar_lo=$value
	if  ($cmd == 59)  set filter_butpar_hi=$value
	if  ($cmd == 60)  set filter_butpar_order=$value
	if  ($cmd == 61)  set auto_phase=$value
#	if  ($cmd == 62)  set jukebox_root=$value
	if  ($cmd == 63)  set draw_area_width=$value
	if  ($cmd == 64)  set draw_area_height=$value
	if  ($cmd == 65)  set area_zoom_base=$value
	if  ($cmd == 66)  set area_zoom_exp=$value
	if  ($cmd == 67)  set opick_thresh=$value
	if  ($cmd == 68)  set opick_duration=$value
	if  ($cmd == 69)  set opick_break=$value
	if  ($cmd == 71)  set default_phase_flags=$value
	if  ($cmd == 72)  set reformat_proc=$value
	if  ($cmd == 73)  set final_proc=$value
	if  ($cmd == 74)  set ml_sigma=$value
	if  ($cmd == 75)  set minmaxfmt=$value
#	if  ($cmd == 76)  set own_accelerators=$value
end

# replace '/' by '\\'
if  ("`uname`" == "Linux")  then
	set defpath_filter=`echo $defpath_filter | sed 's/\//\\\\/g'`
	set defpath_events=`echo $defpath_events | sed 's/\//\\\\/g'`
	set defpath_gse=`echo $defpath_gse | sed 's/\//\\\\/g'`
	set defpath_gse2=`echo $defpath_gse2 | sed 's/\//\\\\/g'`
	set defpath_ah=`echo $defpath_ah | sed 's/\//\\\\/g'`
	set defpath_q=`echo $defpath_q | sed 's/\//\\\\/g'`
	set defpath_evid=`echo $defpath_evid | sed 's/\//\\\\/g'`
	set defpath_evtout=`echo $defpath_evtout | sed 's/\//\\\\/g'`
	set defpath_data=`echo $defpath_data | sed 's/\//\\\\/g'`
	set event_check_proc=`echo $event_check_proc | sed 's/\//\\\\/g'`
	set screendump_proc=`echo $screendump_proc | sed 's/\//\\\\/g'`
	set evtview_proc=`echo $evtview_proc | sed 's/\//\\\\/g'`
#	set jukebox_root=`echo $jukebox_root | sed 's/\//\\\\/g'`
	set reformat_proc=`echo $reformat_proc | sed 's/\//\\\\/g'`
	set final_proc=`echo $final_proc | sed 's/\//\\\\/g'`
else
	set defpath_filter=`echo $defpath_filter | sed 's/\//\\/g'`
	set defpath_events=`echo $defpath_events | sed 's/\//\\/g'`
	set defpath_gse=`echo $defpath_gse | sed 's/\//\\/g'`
	set defpath_gse2=`echo $defpath_gse2 | sed 's/\//\\/g'`
	set defpath_ah=`echo $defpath_ah | sed 's/\//\\/g'`
	set defpath_q=`echo $defpath_q | sed 's/\//\\/g'`
	set defpath_evid=`echo $defpath_evid | sed 's/\//\\/g'`
	set defpath_evtout=`echo $defpath_evtout | sed 's/\//\\/g'`
	set defpath_data=`echo $defpath_data | sed 's/\//\\/g'`
	set event_check_proc=`echo $event_check_proc | sed 's/\//\\/g'`
	set screendump_proc=`echo $screendump_proc | sed 's/\//\\/g'`
	set evtview_proc=`echo $evtview_proc | sed 's/\//\\/g'`
#	set jukebox_root=`echo $jukebox_root | sed 's/\//\\/g'`
	set reformat_proc=`echo $reformat_proc | sed 's/\//\\/g'`
	set final_proc=`echo $final_proc | sed 's/\//\\/g'`
endif

# rewrite startup file
if  (-e $startupfile.save)  \rm $startupfile.save
mv $startupfile $startupfile.save
touch $startupfile
echo "\! file SHM_USER_STARTUP.SHC"                      >>$startupfile
echo "\! created by $0 at `date`"                        >>$startupfile
echo ""                                                  >>$startupfile
echo "sdef tmp"                                          >>$startupfile
echo ""                                                  >>$startupfile
echo "@SHMSETUP DEPTH_PHASE_LIST $depth_phase_list"      >>$startupfile
echo "@SHMSETUP THEO_PHASE_LIST $theo_phase_list"        >>$startupfile
echo "@SHMSETUP DIFF_PHASE_LIST $diff_phase_list"        >>$startupfile
echo "@SHMSETUP CLOSE_PHASE_RESOL $close_phase_resol"    >>$startupfile
echo "@SHMSETUP MIN_DRAG_BOX_WIDTH $min_drag_box_width"  >>$startupfile
echo "@CALC S &TMP = $defpath_filter DOSLASH"            >>$startupfile
echo  @SHMSETUP DEFPATH_FILTER \"TMP                     >>$startupfile
echo "@CALC S &TMP = $defpath_events DOSLASH"            >>$startupfile
echo  @SHMSETUP DEFPATH_EVENTS \"TMP                     >>$startupfile
echo "@CALC S &TMP = $defpath_gse DOSLASH"               >>$startupfile
echo  @SHMSETUP DEFPATH_GSE \"TMP                        >>$startupfile
echo "@CALC S &TMP = $defpath_gse2 DOSLASH"              >>$startupfile
echo  @SHMSETUP DEFPATH_GSE2 \"TMP                       >>$startupfile
echo "@CALC S &TMP = $defpath_ah DOSLASH"                >>$startupfile
echo  @SHMSETUP DEFPATH_AH \"TMP                         >>$startupfile
echo "@CALC S &TMP = $defpath_q DOSLASH"                 >>$startupfile
echo  @SHMSETUP DEFPATH_Q \"TMP                          >>$startupfile
echo "@CALC S &TMP = $defpath_evid DOSLASH"              >>$startupfile
echo  @SHMSETUP DEFPATH_EVID \"TMP                       >>$startupfile
echo "@CALC S &TMP = $defpath_evtout DOSLASH"            >>$startupfile
echo  @SHMSETUP DEFPATH_EVTOUT \"TMP                     >>$startupfile
echo "@CALC S &TMP = $defpath_data DOSLASH"              >>$startupfile
echo  @SHMSETUP DEFPATH_DATA \"TMP                       >>$startupfile
echo "shmsetup top_down_order $top_down_order"           >>$startupfile
echo "shmsetup double_click_time $double_click_time"     >>$startupfile
echo "shmsetup trace_zoom_base $trace_zoom_base"         >>$startupfile
echo "shmsetup trace_zoom_exp $trace_zoom_exp"           >>$startupfile
echo "shmsetup color_mark $color_mark"                   >>$startupfile
echo "shmsetup color_theo $color_theo"                   >>$startupfile
echo "shmsetup color_auto $color_auto"                   >>$startupfile
echo "shmsetup color_crsr $color_crsr"                   >>$startupfile
echo "shmsetup refstation $refstation"                   >>$startupfile
echo "shmsetup autopick_first $autopick_first"           >>$startupfile
echo "@SHMSETUP EDIT_CMD "'|'"$edit_cmd"'|$BLANK|$PERCENT|s|' >>$startupfile
echo "shmsetup calib_wdw_width $calib_wdw_width"         >>$startupfile
echo "shmsetup calib_wdw_height $calib_wdw_height"       >>$startupfile
echo "shmsetup calib_azimuth_grid $calib_azimuth_grid"   >>$startupfile
echo "shmsetup calib_slowness_grid $calib_slowness_grid" >>$startupfile
echo "shm_cmd_autoscale $auto_scaling"                   >>$startupfile
echo "shmsetup use_rec_filters $use_rec_filters"         >>$startupfile
echo "shmsetup prompt_analyst $prompt_analyst"           >>$startupfile
echo "shmsetup motif_log $motif_log"                     >>$startupfile
echo "shmsetup phase_ampl_diff $phase_ampl_diff"         >>$startupfile
echo "shmsetup reverse_xors $reverse_xors"               >>$startupfile
echo "shmsetup x_max_drawlth $x_max_drawlth"             >>$startupfile
echo "shmsetup default_quality $default_quality"         >>$startupfile
echo "shmsetup drag_box_rubber_val $drag_box_rubber_val" >>$startupfile
echo "shmsetup default_filter $default_filter"           >>$startupfile
echo "shmsetup default_phase_type $default_phase_type"   >>$startupfile
echo "shmsetup default_depth_type $default_depth_type"   >>$startupfile
echo "shmsetup default_depth $default_depth"             >>$startupfile
echo "shmsetup default_loc_quality $default_loc_quality" >>$startupfile
echo "shmsetup max_cursor_form $max_cursor_form"         >>$startupfile
echo "@CALC S &TMP = $event_check_proc DOSLASH"          >>$startupfile
echo  @SHMSETUP EVENT_CHECK_PROC \"TMP                   >>$startupfile
echo "@CALC S &TMP = $screendump_proc DOSLASH"           >>$startupfile
echo  @SHMSETUP SCREENDUMP_PROC \"TMP                    >>$startupfile
echo "@CALC S &TMP = $evtview_proc DOSLASH"              >>$startupfile
echo  @SHMSETUP EVTVIEW_PROC \"TMP                       >>$startupfile
echo "shmsetup remreqhost $remreqhost"                   >>$startupfile
echo "shmsetup full_phase_names $full_phase_names"       >>$startupfile
echo "shmsetup default_source $default_source"           >>$startupfile
echo "shmsetup move_wdw_step $move_wdw_step"             >>$startupfile
echo "shmsetup top_margin $top_margin"                   >>$startupfile
echo "@SHMSETUP AUTO_PHASE $auto_phase"                  >>$startupfile
echo "shmsetup draw_area_width $draw_area_width"         >>$startupfile
echo "shmsetup draw_area_height $draw_area_height"       >>$startupfile
echo "shmsetup area_zoom_base $area_zoom_base"           >>$startupfile
echo "shmsetup area_zoom_exp $area_zoom_exp"             >>$startupfile
echo "shmsetup default_phase_flags clear"                >>$startupfile
echo "shmsetup default_phase_flags $default_phase_flags" >>$startupfile
echo "@CALC S &TMP = $reformat_proc DOSLASH"             >>$startupfile
echo  @SHMSETUP REFORMAT_PROC \"TMP                      >>$startupfile
echo "@CALC S &TMP = $final_proc DOSLASH"                >>$startupfile
echo  @SHMSETUP FINAL_PROC \"TMP                         >>$startupfile
if  ("$ml_sigma" != "default")  then
	echo "external_routine table_ml_sigma:$ml_sigma"      >>$startupfile
endif
if  ("$minmaxfmt" == "none")  then
	echo "external_routine minmaxfmt:"                    >>$startupfile
else
	echo "external_routine minmaxfmt:$minmaxfmt"          >>$startupfile
endif
#echo "shmsetup own_accelerators $own_accelerators"       >>$startupfile
echo ""                                                  >>$startupfile
echo "return"                                            >>$startupfile

# manipulate xresource file
if  (-e $xresfile.$$)  \rm $xresfile.$$
grep -v 'SHM*' $xresfile >$xresfile.$$

\rm $xresfile
mv $xresfile.$$ $xresfile
echo 'SHM*locsat_prefix_text.value:       '"$locsat_prefix"         >>$xresfile
echo 'SHM*fk_frqlo_text.value:            '"$fk_frqlo"              >>$xresfile
echo 'SHM*fk_frqhi_text.value:            '"$fk_frqhi"              >>$xresfile
echo 'SHM*fk_slowness_text.value:         '"$fk_slowness"           >>$xresfile
echo 'SHM*fk_resol_text.value:            '"$fk_resol"              >>$xresfile
echo 'SHM*fk_colnum_text.value:           '"$fk_colnum"             >>$xresfile
echo 'SHM*filter_butpar_hi_text.value:    '"$filter_butpar_hi"      >>$xresfile
echo 'SHM*filter_butpar_lo_text.value:    '"$filter_butpar_lo"      >>$xresfile
echo 'SHM*filter_butpar_order_text.value: '"$filter_butpar_order"   >>$xresfile
echo 'SHM*opick_thresh_text.value:        '"$opick_thresh"          >>$xresfile
echo 'SHM*opick_duration_text.value:      '"$opick_duration"        >>$xresfile
echo 'SHM*opick_break_text.value:         '"$opick_break"           >>$xresfile

if  (-e $SH_USERROOT/private/shm_resources.dat)  \
	cat $SH_USERROOT/private/shm_resources.dat >>$xresfile

xrdb $xresfile


