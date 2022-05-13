#! /bin/csh
#
# file shm_setup.csh
#      =============
#
# version 1, 27-Dec-96
#
# Writes private SHM_USER_STARTUP.SHC and modifies $HOME/.Xresources file
# for tailoring SHM parameters.
# K. Stammler, 27-Dec-96
#
# list of parameters (pars marked with (*) do not read defaults from file):
#
# name                default           range
# DEPTH_PHASE_LIST    pP,sP,pS,sS       comma separated phase list
# THEO_PHASE_LIST     P,S,pP,pS,sP,sS,ScS,PcP,PP,SS  --"--
# DIFF_PHASE_LIST     S-P,PP-P,Pg-Pn    comma separated phase difference list
# CLOSE_PHASE_RESOL   8                 integer 1-??
# MIN_DRAG_BOX_WIDTH  7                 integer 1-??
# DEFPATH_FILTER      $SH_FILTER        path (*)
# DEFPATH_EVENTS      $HOME/evtout      path (*)
# DEFPATH_GSE         $HOME             path (*)
# DEFPATH_GSE2        $HOME             path (*)
# DEFPATH_AH          $HOME             path (*)
# DEFPATH_EVID        $HOME/evid        path (*)
# DEFPATH_EVTOUT      $HOME/evtout      path (*)
# DEFPATH_DATA        $HOME             path (*)
# TOP_DOWN_ORDER      FALSE             TRUE FALSE
# DOUBLE_CLICK_TIME   200               integer 1-??
# TRACE_ZOOM_BASE     10.0              float > 0.0
# TRACE_ZOOM_EXP      3.3333333         float > 0.0
# COLOR_MARK          1.0,0.0,0.0       3 floats between 0 and 1
# COLOR_THEO          0.0,1.0,0.0       3 floats between 0 and 1
# COLOR_AUTO          1.0,0.0,0.5       3 floats between 0 and 1
# COLOR_CRSR          0.0,0.0,1.0       3 floats between 0 and 1
# REFSTATION          GRA1              string
# AUTOPICK_FIRST      TRUE              TRUE FALSE
# EDIT_CMD            textedit          string (*)
# CALIB_WDW_WIDTH     2.5               float > 0.0
# CALIB_WDW_HEIGHT    2.5               float > 0.0
# CALIB_AZIMUTH_GRID  10.0              float > 0.0
# CALIB_SLOWNESS_GRID 0.5               float > 0.0
# AUTO_SCALING        ON                ON OFF
# USE_REC_FILTERS     TRUE              TRUE FALSE
# PROMPT_ANALYST      FALSE             TRUE FALSE
# MOTIF_LOG           ;;                ;; or filename
# PHASE_AMPL_DIFF     60.0              float
# REVERSE_XORS        FALSE             TRUE FALSE
# X_MAX_DRAWLTH       4096              integer 1-??
# DEFAULT_QUALITY     2                 integer 1-9
# DRAG_BOX_RUBBER_VAL 10                integer 1-??
# DEFAULT_FILTER      ;;                string

#set echo

## find xresource
#if  (-e $HOME/.Xdefaults)  then
#	set xresfile=$HOME/.Xdefaults
#else if  (-e $HOME/.Xresources)  then
#	set xresfile=$HOME/.Xresources
#else
#	set xresfile=$HOME/.Xdefaults
#endif

# set startup file
set startupfile=$SH_USERROOT/private/SHM_USER_STARTUP.SHC

# set defaults
set depth_phase_list="pP,sP,pS,sS"
set theo_phase_list="P,S,pP,pS,sP,sS,ScS,PcP,PP,SS"
set diff_phase_list="S-P,PP-P,Pg-Pn"
set close_phase_resol="8"
set min_drag_box_width="7"
set defpath_filter=`echo $SH_FILTER | sed 's/\//\\\\/g'`
set defpath_events=`echo $HOME/evtout | sed 's/\//\\\\/g'`
set defpath_gse=`echo $HOME | sed 's/\//\\\\/g'`
set defpath_gse2=`echo $HOME | sed 's/\//\\\\/g'`
set defpath_ah=`echo $HOME | sed 's/\//\\\\/g'`
set defpath_evid=`echo $HOME/evid | sed 's/\//\\\\/g'`
set defpath_evtout=`echo $HOME/evtout | sed 's/\//\\\\/g'`
set defpath_data=`echo $HOME | sed 's/\//\\\\/g'`
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
set edit_cmd="textedit"
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
#	set res=`grep DEFPATH_FILTER $startupfile`
#	if  ("$res" != "")  then
#		set defpath_filter=$res[3]
#	endif
#	set res=`grep DEFPATH_EVENTS $startupfile`
#	if  ("$res" != "")  then
#		set defpath_events=$res[3]
#	endif
#	set res=`grep DEFPATH_GSE $startupfile`
#	if  ("$res" != "")  then
#		set defpath_gse=$res[3]
#	endif
#	set res=`grep DEFPATH_GSE2 $startupfile`
#	if  ("$res" != "")  then
#		set defpath_gse2=$res[3]
#	endif
#	set res=`grep DEFPATH_AH $startupfile`
#	if  ("$res" != "")  then
#		set defpath_ah=$res[3]
#	endif
#	set res=`grep DEFPATH_EVID $startupfile`
#	if  ("$res" != "")  then
#		set defpath_evid=$res[3]
#	endif
#	set res=`grep DEFPATH_EVTOUT $startupfile`
#	if  ("$res" != "")  then
#		set defpath_evtout=$res[3]
#	endif
#	set res=`grep DEFPATH_DATA $startupfile`
#	if  ("$res" != "")  then
#		set defpath_data=$res[3]
#	endif
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
endif

# display menu
set cmd=0
set value=""
while  ("$cmd" != "q" && "$cmd" != "x")
	echo ""
	echo " (1) depth phase list  :       $depth_phase_list"
	echo " (2) theo phase list   :       $theo_phase_list"
	echo " (3) diff phase list   :       $diff_phase_list"
	echo " (4) close phase resol (pixel):   $close_phase_resol"
	echo " (5) min dragbox width (pixel):   $min_drag_box_width"
	echo " (6) default filter path:      $defpath_filter"
	echo " (7) default event path:       $defpath_events"
	echo " (8) default gse1.0 path:      $defpath_gse"
	echo " (9) default gse2.0 path:      $defpath_gse2"
	echo "(10) default AH path:          $defpath_ah"
	echo "(11) default evid path:        $defpath_evid"
	echo "(12) default evtout path:      $defpath_evtout"
	echo "(13) default Mini-SEED path:   $defpath_data"
	echo "(14) top-down order:           $top_down_order"
	echo "(15) double click time (ms):   $double_click_time"
	echo "(16) trace zoom base:          $trace_zoom_base"
	echo "(17) trace zoom exponent:      $trace_zoom_exp"
	echo "(18) color of handpicked p.:   $color_mark"
	echo "(19) color of theoretical p.:  $color_theo"
	echo "(20) color of autopicked p.:   $color_auto"
	echo "(21) color of cursor:          $color_crsr"
	echo "(22) reference station:        $refstation"
	echo "(23) autopick: first instead of largest:     $autopick_first"
	echo "(24) edit command:             $edit_cmd"
	echo "(25) calib wdw width:          $calib_wdw_width"
	echo "(26) calib wdw height:         $calib_wdw_height"
	echo "(27) calib azimuth grid:       $calib_azimuth_grid"
	echo "(28) calib slowness grid:      $calib_slowness_grid"
	echo "(29) auto scaling of traces:   $auto_scaling"
	echo "(30) use recursive filters:    $use_rec_filters"
	echo "(31) prompt analyst:           $prompt_analyst"
	echo "(32) motif action log (dbg):   $motif_log"
	echo "(33) max ampl dist from phase: $phase_ampl_diff"
	echo "(34) reverse XORs:             $reverse_xors"
	echo "(35) X-max drawlth:            $x_max_drawlth"
	echo "(36) default_quality:          $default_quality"
	echo "(37) drag box rubber value:    $drag_box_rubber_val"
	echo "(38) default filter:           $default_filter"
	echo ""
	echo -n "enter selection: "
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
end

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
echo ""                                                  >>$startupfile
echo "return"                                            >>$startupfile

