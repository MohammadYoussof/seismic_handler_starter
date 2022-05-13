
/* file shm.c
 *      =====
 *
 * version 220, 21-Nov-2007
 *
 * main module of shm
 * K. Stammler, 15-Feb-93
 */


/*
 *
 *  SeismicHandler, seismic analysis software
 *  Copyright (C) 1996,  Klaus Stammler, Federal Institute for Geosciences
 *                                       and Natural Resources (BGR), Germany
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */


#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "basecnst.h"
#undef BC_DEFINE_TRUE_FALSE
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include <Xm/Xm.h>
#include <Xm/ToggleB.h>
#include <Xm/Scale.h>
#include <Xm/Text.h>
#include <Mrm/MrmPublic.h>
#include <X11/Xresource.h>

#define MAINPROG

#define HELPTEXT(s) if (xmv_help) cl4_help(xmv_w[k_widget_help_scroll],s)

#include "sysbase.h"
#include "shconst.h"
#include "shvars.h"
#include "shdirs.h"
#include "causrdef.h"
#include "tcusrdef.h"
#include "earthloc.h"
#include "phaseinf.h"
#include "phasemgr.h"
#include "seusrdef.h"
#include "fctxmn5.h"
#include "callsh.h"
#include "infoidx.h"
#include "glusrdef.h"
#include "motifgraph.h"
#include "cbutil.h"
#include "mfexec.h"
#include "station_no.h"
#include "pixmaps.h"
#include "seismics.h"
#include "cblib.h"
#include "cblib2.h"
#include "cblib3.h"
#include "cblib4.h"
#include "calibloc.h"
#include "spectrum.h"
#include "partmotion.h"
#include "polarfil.h"
#include "shm_widgets.h"
#include "seed_io/seedcfg.h"
#include "seed_io/seed_lib.h"
#include "globalparams.h"
#include "sqliface.h"


/* menu entry numbers */
#define k_entry_command 1
#define k_entry_quit 2
#define k_entry_read_grsn 3
#define k_entry_stw 4
#define k_entry_dtw 5
#define k_entry_parameters 6
#define k_entry_filter 7
#define k_entry_amplper_p 8
#define k_entry_amplper_z 9
#define k_entry_ampl_man 10
#define k_entry_per_man 11
#define k_entry_abort_selection 12
#define k_entry_dump_params 13
#define k_entry_short_info 14
#define k_entry_trc_delete 15
#define k_entry_trc_demean 16
#define k_entry_trc_despike 17
#define k_entry_trc_trend 18
#define k_entry_readgse 19
#define k_entry_readah 20
#define k_entry_readq 21
#define k_entry_calib_accept 22
#define k_entry_calib_quit 23
#define k_entry_cmd_speccmd 24
#define k_entry_readrfmt 25
/* 26 */
#define k_entry_cmd_del_horiz 27
/* 28 */
#define k_entry_cmd_screendump 29
#define k_entry_magn_mb 30
#define k_entry_magn_ms_plain 31
#define k_entry_magn_ms_c_na 32
#define k_entry_magn_ms_c_eu 33
#define k_entry_magn_ms_c_o 34
#define k_entry_magn_ms_o 35
#define k_entry_magn_ml 36
#define k_entry_trc_hide 37
#define k_entry_trc_invhide 38
#define k_entry_final_params 39
#define k_entry_cancel_params 40
#define k_entry_calib_screendump 41
#define k_entry_gencomment 42
#define k_entry_params_save 43
#define k_entry_params_restore 44
#define k_entry_ext_location 45
#define k_entry_phase_difference 46
#define k_entry_source_region 47
#define k_entry_rotate 48
#define k_entry_deltheo 49
#define k_entry_fk 50
#define k_entry_params_recover 51
#define k_entry_wdw_move_right 52
#define k_entry_wdw_move_left 53
#define k_entry_wdw_grow_right 54
#define k_entry_wdw_grow_left 55
#define k_entry_help 56
#define k_entry_info_source 57
#define k_entry_vespa 58
#define k_entry_trc_sort_d 59
#define k_entry_trc_sort_a 60
#define k_entry_cmd_userdef 61
#define k_entry_magn_sep 62
#define k_entry_locsat 63
#define k_entry_setup 64
#define k_entry_readgse2 65
#define k_entry_trc_refml 66
#define k_entry_magn_del_ml 67
#define k_entry_params_evt 68
#define k_entry_flags_setcalib 69
#define k_entry_flags_unsetcalib 70
#define k_entry_ampl_surface 71
#define k_entry_refstation 72
#define k_entry_mparam_sn_auto 73
#define k_entry_mparam_sn_repeat 74
#define k_entry_mparam_sn_clear 75
#define k_entry_mparam_sn_delete 76
#define k_entry_configure 77
#define k_entry_onset_pick 78
#define k_entry_opick_setup 79
#define k_entry_wdw_3traces 80
#define k_entry_spectrum 81
#define k_entry_spct_insert 82
#define k_entry_spct_quit 83
#define k_entry_spct_screendump 84
#define k_entry_spct_amplo_up 85
#define k_entry_spct_amplo_down 86
#define k_entry_spct_amphi_up 87
#define k_entry_spct_amphi_down 88
#define k_entry_spct_frqlo_up 89
#define k_entry_spct_frqlo_down 90
#define k_entry_spct_frqhi_up 91
#define k_entry_spct_frqhi_down 92
#define k_entry_spct_mode_powspc 93
#define k_entry_spct_mode_fft 94
#define k_entry_spct_mode_fft2 95
/* 96-99 */
#define k_entry_spct_clear 100
#define k_entry_spct_setup 101
#define k_entry_spct_autoscale 102
#define k_entry_pmotion 103
#define k_entry_pm_screendump 104
#define k_entry_pm_quit 105
#define k_entry_pm_lincol 106
#define k_entry_pm_linmono 107
#define k_entry_pm_plot1 108
#define k_entry_pm_plot3 109
#define k_entry_pm_zoom_on 110
#define k_entry_pm_zoom_off 111
#define k_entry_write_mseed 112
#define k_entry_write_gse 113
#define k_entry_planewave 114
#define k_entry_align 115
#define k_entry_beam 116
#define k_entry_fixbeam 117
#define k_entry_autopick1 118
#define k_entry_onsetpick 119
#define k_entry_calib 120
#define k_entry_residcorr 121
#define k_entry_residdel 122
#define k_entry_corrpick 123
#define k_entry_eproc1 124
#define k_entry_eproc2 125
#define k_entry_eproc3 126
#define k_entry_eproc4 127
#define k_entry_eproc5 128
#define k_entry_eproc6 129
#define k_entry_eproc7 130
#define k_entry_eproc8 131
#define k_entry_eproc9 132
#define k_entry_eproc10 133
#define k_entry_eproc11 134
#define k_entry_eproc12 135
#define k_entry_eproc13 136
#define k_entry_eproc14 137
#define k_entry_eproc15 138
#define k_entry_eproc16 139
#define k_entry_eproc17 140
#define k_entry_eproc18 141
#define k_entry_eproc19 142
#define k_entry_eproc20 143
#define k_entry_adrm_request 144
#define k_entry_match_location 145
#define k_entry_identify_phase 146
#define k_entry_spct_fitline 147
#define k_entry_trc_keep_r 148
#define k_entry_trc_keep_t 149
#define k_entry_locate_tele 150
#define k_entry_polarfil 151
#define k_entry_pol_quit 152
#define k_entry_pol_screendump 153
#define k_entry_pol_inc_cohlth 154
#define k_entry_pol_dec_cohlth 155
#define k_entry_pol_inc_pow_linfil 156
#define k_entry_pol_dec_pow_linfil 157
#define k_entry_add_plugin 158
#define k_entry_spectrogram 159
#define k_entry_trc_del_badqual 160

#define k_entry_key_arrow_up 300
#define k_entry_key_arrow_down 301
#define k_entry_key_arrow_left 302
#define k_entry_key_arrow_right 303

/* scale numbers */
#define k_scale_read_grsn_length 1
#define k_scale_param_ctrl_zoom 2
#define k_scale_filter_autocut 3
#define k_scale_setup_dh 4
#define k_scale_setup_dw 5

/* button numbers */
#define k_button_read_grsn_read 1
#define k_button_read_grsn_cancel 2
#define k_button_read_grsn_1hz 3
#define k_button_read_grsn_20hz 4
#define k_button_read_grsn_80hz 5
   /* reserved 6-34 entries */
#define k_button_read_grsn_comp_z 35
#define k_button_read_grsn_comp_n 36
#define k_button_read_grsn_comp_e 37
#define k_button_read_grsn_secup 38
#define k_button_read_grsn_minup 39
#define k_button_read_grsn_hourup 40
#define k_button_read_grsn_dayup 41
#define k_button_read_grsn_secdn 42
#define k_button_read_grsn_mindn 43
#define k_button_read_grsn_hourdn 44
#define k_button_read_grsn_daydn 45
#define k_button_read_grsn_grf 46
#define k_button_read_grsn_grsn 47
#define k_button_read_grsn_keep 48
#define k_button_read_grsn_yearup 49
#define k_button_read_grsn_yeardn 50
#define k_button_read_grsn_monthup 51
#define k_button_read_grsn_monthdn 52
#define k_button_read_grsn_nexttime 53
#define k_button_read_grsn_prevtime 54
#define k_button_phase_ok 55
#define k_button_phase_dismiss 56
#define k_button_phase_sign_p 57
#define k_button_phase_sign_m 58
#define k_button_phase_sign_0 59
#define k_button_phase_reliab_yes 60
#define k_button_phase_reliab_no 61
#define k_button_phase_name_b 62
#define k_button_phase_name_Pn 63
#define k_button_phase_name_Pg 64
#define k_button_phase_name_Sn 65
#define k_button_phase_name_Sg 66
#define k_button_phase_name_Lg 67
#define k_button_phase_name_Rg 68
#define k_button_phase_name_P 69
#define k_button_phase_name_pP 70
#define k_button_phase_name_sP 71
#define k_button_phase_name_PKPdf 72
#define k_button_phase_name_PKPbc 73
#define k_button_phase_name_PKPab 74
#define k_button_phase_name_pPKPdf 75
#define k_button_phase_name_pPKPbc 76
#define k_button_phase_name_pPKPab 77
#define k_button_phase_name_PKP 78
#define k_button_phase_name_PP 79
#define k_button_phase_name_pPP 80
#define k_button_phase_name_S  81
#define k_button_phase_name_SKS 82
#define k_button_phase_name_PKKP  83
#define k_button_phase_name_PKPPKP  84
#define k_button_phase_name_PcP  85
#define k_button_phase_name_L 86
#define k_button_phase_name_X 87
#define k_button_phase_spec_e 100
#define k_button_phase_spec_i 101
#define k_button_phase_delete 102
#define k_button_param_dismiss 103
#define k_button_param_localize 104
#define k_button_param_theo 105
#define k_button_filter_ok 106
#define k_button_filter_cancel 107
#define k_button_filter_none 108
#define k_button_filter_other 109
#define k_button_filter_wwssn_sp 110
#define k_button_filter_wwssn_lp 111
#define k_button_filter_lrsm_sp 112
#define k_button_filter_lrsm_lp 113
#define k_button_filter_kirnos 114
#define k_button_filter_woodand 115
#define k_button_filter_standard_bp 116
#define k_button_filter_but_bp 117
#define k_button_filter_but_lp 118
#define k_button_filter_but_hp 119
#define k_button_filter_sro_lp 120
#define k_button_filter_displace 121
   /* reserved 122-123 */
#define k_button_multipli_1_3 124
#define k_button_multipli_1_2 125
#define k_button_multipli_1 126
#define k_button_multipli_2 127
#define k_button_multipli_3 128
#define k_button_multipli_4 129
#define k_button_multipli_edit_mul 130
#define k_button_multipli_edit_div 131
#define k_button_phase_type_other 132
#define k_button_phase_type_local 133
#define k_button_phase_type_regio 134
#define k_button_phase_type_tele 135
#define k_button_phase_type_nuclear 136
#define k_button_phase_type_blast 137
#define k_button_phase_type_mining 138
#define k_button_param_depth 139
#define k_button_read_grsn_eventfile 141
#define k_button_filter_butpar_lo_up 143
#define k_button_filter_butpar_lo_down 144
#define k_button_filter_butpar_hi_up 145
#define k_button_filter_butpar_hi_down 146
#define k_button_filter_butpar_order_up 147
#define k_button_filter_butpar_order_down 148
#define k_button_read_grsn_readnew 150
#define k_button_param_hypoloc 151
/*   :  */
#define k_button_hypo_ok 170
#define k_button_hypo_cancel 171
#define k_button_hypo_depth_0 172
#define k_button_hypo_depth_5 173
#define k_button_hypo_depth_10 174
#define k_button_hypo_depth_15 175
#define k_button_hypo_depth_20 176
#define k_button_hypo_depth_33 177
#define k_button_hypo_depth_free 180
#define k_button_hypo_use_s_yes 181
#define k_button_hypo_use_s_no 182
#define k_button_phase_qual_0 183
#define k_button_phase_qual_1 184
#define k_button_phase_qual_2 185
#define k_button_phase_qual_3 186
#define k_button_phase_qual_4 187
#define k_button_phase_qual_5 188
#define k_button_phase_qual_6 189
#define k_button_phase_qual_7 190
#define k_button_phase_qual_8 191
#define k_button_phase_qual_9 192
/*   :   */
#define k_button_param_theo_ext 200
#define k_button_analyst_ok 201
#define k_button_analyst_cancel 202
#define k_button_filter_autocut_0 203
#define k_button_filter_autocut_5sec 204
#define k_button_filter_autocut_20sec 205
#define k_button_filter_autocut_1min 206
#define k_button_filter_autocut_3min 207
#define k_button_filter_autocut_5min 208
#define k_button_filter_autocut_10min 209
/*  :  */
#define k_button_infsource_ok 215
#define k_button_infsource_cancel 216
#define k_button_param_locq_tooweak 217
#define k_button_param_locq_incoherent 218
#define k_button_param_locq_nobearing 219
#define k_button_param_locq_region 220
#define k_button_param_locq_reliable 221
#define k_button_param_set_1 222
#define k_button_param_set_2 223
#define k_button_param_set_3 224
#define k_button_read_grsn_reset 225
#define k_button_fk_ok 227
#define k_button_fk_cancel 228
#define k_button_read_grsn_invhdr 229
#define k_button_fk_unit_deg 230
#define k_button_fk_unit_km 231
#define k_button_fk_get_values 232
#define k_button_theo_phase_ok 233
#define k_button_theo_phase_cancel 234
#define k_button_phase_acc_none 235
#define k_button_phase_acc_query 236
#define k_button_phase_acc_display 237
#define k_button_param_locq_undefined 238
#define k_button_speccmd_hide 239
#define k_button_speccmd_1 240
/* reserved until 259 */
#define k_button_read_grsn_edit_hz 260
#define k_button_read_grsn_station 261
/* reserved until 292 */
#define k_button_vespa_ok 293
#define k_button_vespa_cancel 294
#define k_button_vespa_undo 295
#define k_button_vespa_power_1 296
#define k_button_vespa_power_2 297
#define k_button_vespa_power_3 298
#define k_button_vespa_power_5 299
#define k_button_vespa_power_10 300
#define k_button_vespa_slostep_1 301
#define k_button_vespa_slostep_2 302
#define k_button_vespa_slostep_3 303
#define k_button_vespa_slohi_1 304
#define k_button_vespa_slohi_2 305
#define k_button_vespa_slohi_3 306
#define k_button_read_grsn_all 307
#define k_button_locsat_ok 308
#define k_button_locsat_cancel 309
#define k_button_locsat_depth_0 310
#define k_button_locsat_depth_1 311
#define k_button_locsat_depth_10 312
#define k_button_locsat_depth_15 313
#define k_button_locsat_depth_20 314
#define k_button_locsat_depth_33 315
#define k_button_locsat_depth_free 316
#define k_button_locsat_auto_yes 317
#define k_button_locsat_auto_no 318
#define k_button_setup_hide 319
#define k_button_setup_hc_fast 320
#define k_button_setup_hc_qual 321
#define k_button_setup_maxcrsr_crosshair 322
#define k_button_setup_maxcrsr_waveform 323
#define k_button_setup_maxcrsr_negwave 324
#define k_button_setup_maxcrsr_hilbert 325
#define k_button_setup_maxcrsr_neghilb 326
#define k_button_setup_norm_c 327
#define k_button_setup_norm_af 328
#define k_button_setup_norm_aw 329
#define k_button_setup_norm_sf 330
#define k_button_setup_norm_sw 331
#define k_button_setup_topdown_top 332
#define k_button_setup_topdown_down 333
#define k_button_setup_filtyp_recursive 334
#define k_button_setup_filtyp_fft 335
#define k_button_setup_deffil_none 336
#define k_button_setup_deffil_standard_bp 337
#define k_button_setup_deffil_wwssn_sp 338
#define k_button_setup_evtview_full 339
#define k_button_setup_evtview_brief 340
#define k_button_read_grsn_request 341
#define k_button_locsat_output_yes 342
#define k_button_locsat_output_no 343
#define k_button_setup_phasename_full 344
#define k_button_setup_phasename_brief 345
#define k_button_read_grsn_todaysfd 346
#define k_button_refstat_ok 347
#define k_button_refstat_station_1 348
/* 349-352 reserved */
#define k_button_refstat_station_last 353
#define k_button_phase_attrib_ext 354
#define k_button_attrib_ok 355
#define k_button_evpublic_ignore 356
#define k_button_evpublic_telex1 357
#define k_button_evpublic_telexall 358
#define k_button_setup_deffil_but_bp 359
#define k_button_vespa_export 360
#define k_button_read_grsn_device 361
#define k_button_read_grsn_fbox 362
#define k_button_opick_ok 364
#define k_button_opick_thresh_1 365
#define k_button_opick_thresh_2 366
#define k_button_opick_thresh_3 367
#define k_button_opick_thresh_4 368
#define k_button_opick_thresh_5 369
#define k_button_opick_thresh_6 370
#define k_button_opick_thresh_7 371
#define k_button_opick_duration_1 372
#define k_button_opick_duration_2 373
#define k_button_opick_duration_3 374
#define k_button_opick_duration_4 375
#define k_button_opick_duration_5 376
#define k_button_opick_duration_6 377
#define k_button_opick_duration_7 378
#define k_button_opick_break_1 379
#define k_button_opick_break_2 380
#define k_button_opick_break_3 381
#define k_button_opick_break_4 382
#define k_button_opick_break_5 383
#define k_button_opick_break_6 384
#define k_button_opick_break_7 385
#define k_button_spcsetup_ok 386
#define k_button_spcsetup_cancel 387
#define k_button_read_grsn_nextdiag 388
#define k_button_fk_okauto 389
#define k_button_read_grsn_comp_edit 390
#define k_button_adrm_ok 391
#define k_button_adrm_cancel 392
#define k_button_adrm_1 393
/* up to 407 reserved */
#define k_button_adrm_configure 408
#define k_button_param_setslow 409
#define k_button_locsat_hypo 410
#define k_button_plugin_add 411
#define k_button_plugin_cancel 412
#define k_button_plugin_1 413
/* up to 427 reserved */

/* constants */
#define XMC_MAXWIDGET 400

#define XMC_TIME_BUTTON Button3
#define XMC_TIME_BUTTON_MASK Button3Mask
#define XMC_PHASE_BUTTON Button1
#define XMC_PHASE_BUTTON_MASK Button1Mask
#define XMC_CURSOR_BUTTON Button2
#define XMC_CURSOR_BUTTON_MASK Button2Mask

#define XMC_MAXFILNAME 10


/* exported variables of SH */
int        tc;                 /* local text channels */
int        tc_shv;             /* global text channels */
int        gc;                 /* local graphic channels */
int        gc_shv;             /* global graphic channels */
int        cc;                 /* local console channels */
int        cc_shv;             /* global console channels */
SHFLAGS    shflags_shv;        /* local processing flags */
SHFLAGS    shglbflags_shv;     /* global processing flags */
TSyStatus  shv_last_status;    /* status of last command */
int        shv_maininput=0;    /* main input level for CP's */
char       id_shv[11]={SHC_FILE_PREFIX}; /* session ID */

char       protfile_shv[SHC_FILELTH+1]; /* protocol file */
char       shd_scratch[SHC_FILELTH+1] =  {DD_SCRATCH};
char       shd_help[SHC_FILELTH+1]    =  {DD_HELP};
char       shd_errors[SHC_FILELTH+1]  =  {DD_ERRORS};
char       shd_inputs[SHC_FILELTH+1]  =  {DD_INPUTS};


/* global variables */
static XtAppContext   xmv_appctxt;
static Widget         xmv_appshell;        /* main widget */
static MrmHierarchy   xmv_hier;            /* hierarchy ID */
static char           *xmv_uidlist[3];
static char           xmv_uidentry1[cBcFileLth+1]; /* 1. (and only) UID file */
       Widget         xmv_w[XMC_MAXWIDGET];/* widget array, used a. in mfexec */
static CUT_PARAMSETS  xmv_par;             /* analysis parameters */
static CUT_PARAMS     *xmv_cpar;           /* pointer to current set */
static MGT_DSPCTRL    xmv_dspctrl;         /* display control parameters */
static CUT_SELECTIONS xmv_select;          /* additional selections */
static int            xmv_onset_acc=CUC_ACCURACY_NONE; /* accuracy of phases */
static TSyBoolean     xmv_help=FALSE;      /* show help text */
/* SH commands */
       MX_CMD_READG   xmv_cmd_readg;       /* read GRSN, used also in mfexec.c*/
static MX_CMD_FILTER  xmv_cmd_filter;      /* filter applied */
static TPiPhase       xmv_phase;           /* curent phase info */
static CUT_TRACE      *xmv_phasetrc=NULL;  /* phase trace */
static char           xmv_sortstr[cBcShortStrLth+1];   /* sort info */
static char           xmv_sortinfo[cBcShortStrLth+1];  /* sort display text */
static TSyBoolean     xmv_xt_initialized=FALSE;        /* Xt initialized */
static int            xmv_prevent_resize=0; /* prevents resize events if >0 */
static TSyBoolean     xmv_pm_on=FALSE;      /* particle motion switched on */
static TSyBoolean     xmv_polar_on=FALSE;      /* particle motion switched on */
static TSyBoolean     xmv_keeptrc_auto=FALSE; /* 'Keep Traces' Button autom. */
/*static char           xmv_filter_name[cBcShortStrLth+1][MAXFILNAME];*/ /* filter names */
/* theoretical phases (must be same sequence as in dialog box) */
       char           *xmv_thpname[] = {
		"P","pP","sP","PP","PS","PKPab","PKPbc","PKPdf","pPKPab","pPKPbc",
		"pPKPdf","sPKPab","sPKPbc","sPKPdf","PcP","PcS","PKKPdf","PKiKP","PPP",
		"Pdiff","S","pS","sS","SS","SP","SKSac","SKSdf","SKPac","SKPdf",
		"sSKSac","sSKSdf","ScS","ScP","SKKSac","SKKSdf","SSS","SKKPdf","Sdiff",""
		};


/* prototypes of local routines */
static void xm_initialize( void );
static void xmh_log_action( char inf[], int num );
static void xm_exec_userdef( char fname[] );
static void xm_read_requested_data( Widget w[], STATUS *status );
static void xm_configure_windows( Widget w[] );
static void xm_fetch_widgets( void );
static void xm_call_configure( void );
static void xm_call_cmdexec( Widget widget, char *tag,
	XmCommandCallbackStruct *data );
static void xm_call_create_widget( Widget widget, int *tag,
	XmAnyCallbackStruct *data );
static void xm_call_expose_drawingarea( Widget widget, char *tag,
	XmDrawingAreaCallbackStruct *data );
static void xm_call_resize_object( Widget widget, int *tag,
	XmAnyCallbackStruct *data );
static void xm_call_activate( Widget widget, int *tag,
	XmToggleButtonCallbackStruct *data );
static void xm_call_menu_select( Widget widget, int *tag,
	XmAnyCallbackStruct *data );
static void xm_call_scale( Widget widget, int *tag,
	XmScaleCallbackStruct *data );
static void xm_call_drawing_input( Widget widget, int *tag, 
	XmDrawingAreaCallbackStruct *data );
static void xm_call_text_input( Widget widget, int *tag,
	XmTextVerifyCallbackStruct *data );
static void xm_call_file_selection( Widget widget, int *tag,
	XmFileSelectionBoxCallbackStruct *data );
static void xmh_read_gsefile( char datafile[], TSyStatus *status );
void xm_action_motion( Widget w, XEvent *ev, String *params,
	Cardinal *parno );
void xm_set_processing_state( int state );
void xm_set_x_resources( Display *display );
void xm_hex_colour( float col, char *a, char *b );
void xm_move_drag_window( int key_entry );


/* names to be registered */
static MrmRegisterArg   xmv_regvec[] = {
	{ "xm_call_menu_select", (XtPointer) xm_call_menu_select },
	{ "xm_call_create_widget", (XtPointer) xm_call_create_widget },
	{ "xm_call_expose_drawingarea", (XtPointer) xm_call_expose_drawingarea },
	{ "xm_call_resize_object", (XtPointer) xm_call_resize_object },
	{ "xm_call_activate", (XtPointer) xm_call_activate },
	{ "xm_call_scale", (XtPointer) xm_call_scale },
	{ "xm_call_drawing_input", (XtPointer) xm_call_drawing_input },
	{ "xm_call_text_input", (XtPointer) xm_call_text_input },
	{ "xm_call_file_selection", (XtPointer) xm_call_file_selection },
	{ "xm_call_cmdexec", (XtPointer) xm_call_cmdexec }
};

/* action routines */
static   XtActionsRec xmv_new_actions[] = {
	{"xm_action_motion", xm_action_motion}
};



int main( int argc, char *argv[] )
{
	/* local variables */
	/* SH variables */
	STATUS   status;                   /* SH return status */
	char     cmd[BC_LINELTH+1];        /* SH command line */
	char     progname[cBcFileLth+1];   /* program name (shm or shm_world) */
	/* Motif variables */
	int      n;                        /* counter */
	Arg      arglist[2];               /* argument list */
	Display  *display;                 /* display */
	MrmType  class;                    /* widget class */

	/* executable code */


	printf( "\n" );
	printf( "SeismicHandler version 2.4h (05-May-2007), Copyright (C) 2006\n" );
	printf( "Klaus Stammler, Seismological Observatory Graefenberg (SZGRF)\n" );
	printf( "of the Federal Institute for Geosciences and Natural Resources (BGR).\n" );
	printf( "SeismicHandler comes with ABSOLUTELY NO WARRANTY.\n" );
	printf( "This is free software, and you are welcome to redistribute it\n" );
	printf( "under certain conditions; see $SH_ROOT/doc/gpl.txt for details.\n" );
	printf( "\n" );

	/* read global parameters */
	GpReadParfile();
	if  (GpGetInt(cGpI_debug_level) > 5)  {
		printf( "shm-dbg6: dumping global parameters\n" );
		GpDumpParams();
	} /*endif*/
	cl_read_autofilters();

	strcpy( xmv_uidentry1, argv[0] );
	if  (GpGetBoolean(cGpB_small_menu_font))  {
		strcat( xmv_uidentry1, "_smfnt.uid" );
	} else {
		strcat( xmv_uidentry1, ".uid" );
	} /*endif*/
	xmv_uidlist[0] = xmv_uidentry1;
	xmv_uidlist[1] = NULL;

	/* initialize SH */
	/* ------------- */

	status = BC_NOERROR;
	sy_initprocess();
	se_initialize( argc, argv, &status );
	if  (Severe(&status))  {
		se_dsplymsg( 0, status );
		exit( status );
	} /*endif*/

	/* initialize Motif */
	/* ---------------- */

	MrmInitialize();
	XrmInitialize();

	XtToolkitInitialize();
	xmv_appctxt = XtCreateApplicationContext();
	display = XtOpenDisplay( xmv_appctxt, NULL, argv[0], "SHM",
		NULL, 0, &argc, argv );
	if  (display == NULL)  {
		fprintf( stderr, "*SHM: can't open display\n" );
		exit( 1 );
	} /*endif*/

	/* X resources */
	xm_set_x_resources( display );

	XtAppAddActions( xmv_appctxt, xmv_new_actions, 1 );

	n = 0;
	XtSetArg( arglist[n], XmNallowShellResize, True );  n++;
	XtSetArg( arglist[n], XmNsaveUnder, True ); n++;
	xmv_appshell = XtAppCreateShell( argv[0], "SHM",
		applicationShellWidgetClass, display, arglist, n );

	switch  (MrmOpenHierarchyPerDisplay( XtDisplay(xmv_appshell),
		1/*(MrmCount)XtNumber(xmv_uidlist)*/, xmv_uidlist,
		NULL, &xmv_hier ))  {
	case MrmSUCCESS:
		break;
	case MrmNOT_FOUND:
		fprintf( stderr, "*SHM: Unable to open UID files.\n" );
		exit( 1 );
	default:
		fprintf( stderr, "*SHM: Unable to open UID hierarchy.\n" );
		exit( 1 );
	} /*endswitch*/

	if  (MrmRegisterNames(xmv_regvec, (MrmCount) XtNumber(xmv_regvec)) !=
		MrmSUCCESS)  {
		fprintf( stderr, "*SHM: couldn't register names\n" );
	} /*endif*/

	xm_fetch_widgets( );

	/* startup SH, call SHSTRTUP.SHC and SHM_CMD_STARTUP.SHC */
	/*mn5_set_shmsetup_pointer( &shv_global );*/
	mn5_set_external_routine( mx_sh_external_routine );
	callsh( "! just to run the global startup file", &n, &status );
	callsh( "shm_cmd_startup", &n, &status );  /* shm startup */
	mg_set_reverse_xors( GpGetBoolean(cGpB_reverse_xors) );

	/* trace normalisation */
	switch  (GpGetInt(cGpI_trace_normalisation))  {
	case cGp_NORM_CONST:  callsh( "shm_cmd_norm c", &n, &status );  break;
	case cGp_NORM_AW:     callsh( "shm_cmd_norm aw", &n, &status ); break;
	case cGp_NORM_SW:     callsh( "shm_cmd_norm sw", &n, &status );  break;
	default:
		fprintf( stderr, "*SHM: error in program, check trace_normalisation code\n" );
	} /*endswitch*/

#ifdef XXX
	if  (strcmp(GpGetStringElem(cGpL_station_info_file),"default") != 0)  {
		gl_locfile_name( GpGetString(cGpS_station_info_file) );
	} /*endif*/
#endif
	cu_next_read_dialog( xmv_w );

	if  (GpGetBoolean(cGpB_prompt_analyst))  {
		if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[analyst]" );
		if  (MrmFetchWidget(xmv_hier,"analyst_box",xmv_appshell,
			xmv_w+k_widget_analyst_box,&class) != MrmSUCCESS)  {
			fprintf( stderr, "*SHM: can't fetch analyst_box widget\n" );
		} /*endif*/
		XtManageChild( xmv_w[k_widget_analyst_box] );
	} /*endif*/

	xm_initialize();

	xm_configure_windows( xmv_w );

	cl3_init_extproc_entries( xmv_w );

	/* take SH script and evt file from command line */
	{
	char autoexec[cBcLineLth+1]="";  /* autoexec name */
	ca_qstr( argc, argv, "exec", autoexec );
	if  (*autoexec != '\0')  {
		printf( "--> execute auto .%s.\n", autoexec+1 );
		status = cBcNoError;
		callsh( autoexec+1, &n, &status );
		mg_tracedisplay( xmv_w[k_widget_draw], &xmv_dspctrl, &status );
	} /*endif*/
	ca_qstr( argc, argv, "evt", autoexec );
	if  (*autoexec != '\0')  {
		printf( "--> execute evt .%s.\n", autoexec+1 );
		status = cBcNoError;
		cl3_restore_from_evt( autoexec+1, xmv_cpar, NULL,	&status );
		mg_tracedisplay( xmv_w[k_widget_draw], &xmv_dspctrl, &status );
	} /*endif*/
	}

	xmv_xt_initialized = TRUE;
	XtAppMainLoop( xmv_appctxt );

	if  (MrmCloseHierarchy(xmv_hier) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: Unable to close UID hierarchy.\n" );
		exit( 1 );
	} /*endif*/

	se_terminate();
	SqlDeleteScratchFile();

	return 0;

} /* end of main */



/*--------------------------------------------------------------------------*/



static void xm_initialize( void )

/* initializes global variables
 *
 * no parameters
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];  /* scratch */
	char     tmpstr[cBcShortStrLth+1];  /* scratch string */
	STATUS   locstat=BC_NOERROR; /* local status */
	int      i;                  /* counter */
	char     *cptr;              /* pointer to resource string */
	Arg      arglist[3];         /* argument list */
	int      n;                  /* length of list */
	int      u_draw_area_width;  /* drawing area width */
	int      u_draw_area_height; /* drawing area height */

	/* executable code */

	u_draw_area_width = GpGetInt( cGpI_draw_area_width );
	u_draw_area_height = GpGetInt( cGpI_draw_area_height );

	/* check for resource values which are set in resource files usually */
	cptr = cu_get_string( xmv_w[k_widget_locsat_prefix_text] );
	if  (*cptr <= ' ')
		cu_set_string( xmv_w[k_widget_locsat_prefix_text], "tab" );
	cptr = cu_get_string( xmv_w[k_widget_fk_frqlo_text] );
	if  (*cptr <= ' ')
		cu_set_string( xmv_w[k_widget_fk_frqlo_text], "0.4" );
	cptr = cu_get_string( xmv_w[k_widget_fk_frqhi_text] );
	if  (*cptr <= ' ')
		cu_set_string( xmv_w[k_widget_fk_frqhi_text], "3.0" );
	cptr = cu_get_string( xmv_w[k_widget_fk_slowness_text] );
	if  (*cptr <= ' ')
		cu_set_string( xmv_w[k_widget_fk_slowness_text], "15" );
	cptr = cu_get_string( xmv_w[k_widget_fk_resol_text] );
	if  (*cptr <= ' ')
		cu_set_string( xmv_w[k_widget_fk_resol_text], "51" );
	cptr = cu_get_string( xmv_w[k_widget_fk_colnum_text] );
	if  (*cptr <= ' ')
		cu_set_string( xmv_w[k_widget_fk_colnum_text], "10" );
	cptr = cu_get_string( xmv_w[k_widget_filter_butpar_hi_text] );
	if  (*cptr <= ' ')
		cu_set_string( xmv_w[k_widget_filter_butpar_hi_text], "5Hz" );
	cptr = cu_get_string( xmv_w[k_widget_filter_butpar_lo_text] );
	if  (*cptr <= ' ')
		cu_set_string( xmv_w[k_widget_filter_butpar_lo_text], "100s" );
	cptr = cu_get_string( xmv_w[k_widget_filter_butpar_order_text] );
	if  (*cptr <= ' ')
		cu_set_string( xmv_w[k_widget_filter_butpar_order_text], "3" );
	cptr = cu_get_string( xmv_w[k_widget_opick_thresh_text] );
	if  (*cptr <= ' ')
		cu_set_string( xmv_w[k_widget_opick_thresh_text], "10" );
	cptr = cu_get_string( xmv_w[k_widget_opick_duration_text] );
	if  (*cptr <= ' ')
		cu_set_string( xmv_w[k_widget_opick_duration_text], "1.0" );
	cptr = cu_get_string( xmv_w[k_widget_opick_break_text] );
	if  (*cptr <= ' ')
		cu_set_string( xmv_w[k_widget_opick_break_text], "0.0" );

	/* read values from X-resources into shv_global */
	cptr = cu_get_string( xmv_w[k_widget_read_grsn_device] );
	if  (*cptr > ' ' && strlen(cptr) < cBcFileLth)
		GpSetString( cGpS_defpath_data, cptr, NULL );

	xmh_log_action( "--reset--", 0 );

	xmv_dspctrl.zoom = 1.0;
	xmv_dspctrl.norm = MGC_NORM_AF;
	xmv_dspctrl.show_phase_acc = FALSE;

	/* readg */
	strcpy( xmv_cmd_readg.device, GpGetString(cGpS_defpath_data) );
	strcpy( xmv_cmd_readg.start, "24-Feb-93_8:0:0" );
	cu_set_string( xmv_w[k_widget_read_grsn_device], xmv_cmd_readg.device );
	xmv_cmd_readg.seclth = 900.0;
	for  (i=0; i<=xmv_cmd_readg.sl.set2end; i++)
		if  (XmToggleButtonGetState( xmv_w[k_widget_read_grsn_station+i] ))
			xmv_cmd_readg.stations |= (1<<i);
	if  (XmToggleButtonGetState(xmv_w[k_widget_read_grsn_comp_z]))
		if  (strlen(xmv_cmd_readg.comp) < MXC_COMP_STRLTH)
			strcat( xmv_cmd_readg.comp, "Z" );
	if  (XmToggleButtonGetState(xmv_w[k_widget_read_grsn_comp_n]))
		if  (strlen(xmv_cmd_readg.comp) < MXC_COMP_STRLTH)
			strcat( xmv_cmd_readg.comp, "N" );
	if  (XmToggleButtonGetState(xmv_w[k_widget_read_grsn_comp_e]))
		if  (strlen(xmv_cmd_readg.comp) < MXC_COMP_STRLTH)
			strcat( xmv_cmd_readg.comp, "E" );
	if  (XmToggleButtonGetState(xmv_w[k_widget_read_grsn_comp_edit]))
		if  (strlen(xmv_cmd_readg.comp)
			+strlen(cu_get_string(xmv_w[k_widget_read_grsn_comp_text]))
			< MXC_COMP_STRLTH)
			strcat( xmv_cmd_readg.comp,
				cu_get_string(xmv_w[k_widget_read_grsn_comp_text]) );
#ifdef XXX
	xmv_cmd_readg.channum = 1;
	strcpy( xmv_cmd_readg.chanstr[0], "BH" );
	XmToggleButtonSetState( xmv_w[k_widget_read_grsn_20hz], TRUE, FALSE );
#endif
	xmv_cmd_readg.keep = FALSE;
	xmv_cmd_readg.use_readk = FALSE;
	xmv_cmd_readg.reads_invhdr = FALSE;
	strcpy( xmv_cmd_readg.sfdfile, "sfdfile.sfd" );

	strcpy( xmv_cmd_filter.name, GpGetString(cGpS_default_filter) );
	xmv_cmd_filter.autocut = 5.0;

	XmToggleButtonSetState( xmv_w[k_widget_filter_none], TRUE, FALSE );

	/* set AutoDRM selection box */
	cl4_init_adrm_buttons( xmv_w );

	cu_reset_phase( &xmv_phase );
	strcpy( xmv_phase.name, GpGetString(cGpS_auto_phase) );
	cu_phase_box_defaults( xmv_w, &xmv_phase );
	xmv_phase.name[0] = '\0';
	for  (i=0; i<CUC_MAXPARSET; i++)
		xmv_par.par[i].comment = NULL;
	cu_reset_paramsets( &xmv_par );
	xmv_cpar = xmv_par.par;
	xm_set_processing_state( CUC_PARSTATE_INITIAL );
	/* xmv_par.state = CUC_PARSTATE_INITIAL; */
	xmv_cpar->soft_change = TRUE;
	cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
	xmv_cpar->soft_change = FALSE;

	cu_current_time( str );
	str[11] = '\0';
	strcpy( str+18, "00" );
	cu_set_string( xmv_w[k_widget_read_grsn_date], str );
	cu_set_string( xmv_w[k_widget_read_grsn_time], str+12 );
	cu_set_string( xmv_w[k_widget_read_grsn_eventfile_text], "eventlist");
	cu_set_string( xmv_w[k_widget_read_grsn_eventno_text], "0");

	XmScaleSetValue( xmv_w[k_widget_read_grsn_length_scale], 6 );

	XtSetSensitive( xmv_w[k_widget_draw], TRUE );

	/* set size of main window and drawing area */
	n = 0;
	XtSetArg( arglist[n], XmNwidth, u_draw_area_width+4 );  n++;
	XtSetArg( arglist[n], XmNheight, u_draw_area_height+37 );  n++;
	XtSetValues( xmv_w[k_widget_main], arglist, n );
	n = 0;
	XtSetArg( arglist[n], XmNwidth, u_draw_area_width );  n++;
	XtSetArg( arglist[n], XmNheight, u_draw_area_height );  n++;
	XtSetValues( xmv_w[k_widget_draw], arglist, n );

	/* set width of single draw box equal to width of main window */
	n = 0;
	XtSetArg( arglist[n], XmNwidth, u_draw_area_width );  n++;
	XtSetValues( xmv_w[k_widget_single_draw], arglist, n );

	/* create window buffer for main window */
	pix_create_window_buffer( XtDisplay(xmv_w[k_widget_draw]),
		XtWindow(xmv_w[k_widget_draw]), TRUE, &locstat );
	if  (Severe(&locstat))  {
		printf( "*SHM: create_window_buffer error %d ***\n", locstat );
	} /*endif*/

	locstat = BC_NOERROR;
	pix_create_window_buffer( XtDisplay(xmv_w[k_widget_single_draw]),
		XtWindow(xmv_w[k_widget_single_draw]), TRUE, &locstat );
	if  (Severe(&locstat))
		printf( "*SHM: error in pix_create_window_buffer for single w ***\n" );

	mg_draw_cursor( xmv_w[k_widget_draw], MGC_WDW_MAIN, &xmv_dspctrl,
		MGC_CRSR_CONTINUE, 0, 0, NULL );

	/* set colours of windows */
	n = 0;
	XtSetArg( arglist[n], XmNbackground, pix_colour(PIXC_COL_BACKGROUND) );  n++;
	XtSetArg( arglist[n], XmNforeground, pix_colour(PIXC_COL_FOREGROUND) );  n++;
	XtSetValues( xmv_w[k_widget_draw], arglist, n );
	n = 0;
	XtSetArg( arglist[n], XmNbackground, pix_colour(PIXC_COL_BACKGROUND) );  n++;
	XtSetArg( arglist[n], XmNforeground, pix_colour(PIXC_COL_FOREGROUND) );  n++;
	XtSetValues( xmv_w[k_widget_single_draw], arglist, n );
	n = 0;
	XtSetArg( arglist[n], XmNbackground, pix_colour(PIXC_COL_BACKGROUND) );  n++;
	XtSetArg( arglist[n], XmNforeground, pix_colour(PIXC_COL_FOREGROUND) );  n++;
	XtSetValues( xmv_w[k_widget_pmwdw], arglist, n );
	n = 0;
	XtSetArg( arglist[n], XmNbackground, pix_colour(PIXC_COL_BACKGROUND) );  n++;
	XtSetArg( arglist[n], XmNforeground, pix_colour(PIXC_COL_FOREGROUND) );  n++;
	XtSetValues( xmv_w[k_widget_spctwdw], arglist, n );
	n = 0;
	XtSetArg( arglist[n], XmNbackground, pix_colour(PIXC_COL_BACKGROUND) );  n++;
	XtSetArg( arglist[n], XmNforeground, pix_colour(PIXC_COL_FOREGROUND) );  n++;
	XtSetValues( xmv_w[k_widget_calibwdw], arglist, n );
	n = 0;
	XtSetArg( arglist[n], XmNbackground, pix_colour(PIXC_COL_BACKGROUND) );  n++;
	XtSetArg( arglist[n], XmNforeground, pix_colour(PIXC_COL_FOREGROUND) );  n++;
	XtSetValues( xmv_w[k_widget_polwdw], arglist, n );

	/* set phases according to cGpS_theo_phase_list */
	*str = ',';
	strncpy( str+1, GpGetString(cGpS_theo_phase_list), BC_LINELTH-2 );
	strcat( str, "," );
	i = 0;
	while  (xmv_thpname[i][0] != '\0')  {
		*tmpstr = ',';
		strcpy( tmpstr+1, xmv_thpname[i] );
		strcat( tmpstr, "," );
		if  (strstr(str,tmpstr) != NULL)
			XmToggleButtonSetState( xmv_w[k_widget_theo_phase_P+i], TRUE, FALSE );
		i++;
	} /*endwhile*/

	cl4_read_special_commands( xmv_w );

	/* manage reference station */
	locstat = BC_NOERROR;
	cl4_read_refstation_list( xmv_w, &locstat );
	if  (Severe(&locstat))
		printf( "*SHM: error reading ref station list (%d) ***\n", locstat );
	/*
	cl4_read_refstation( 1, str, &locstat );
	if  (Severe(&locstat))
		printf( "*SHM: error reading ref station 1 (%d) ***\n", locstat );
	GpSetString( cGpS_refstation, str, NULL );
	*/

	*xmv_sortstr = '\0';

	mg_init_tracedisplay( xmv_w[k_widget_draw], -1, -1,
		GpGetInt(cGpI_top_margin), -1 );

	XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_text], FALSE );
	XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_text], FALSE );
	XtSetSensitive( xmv_w[k_widget_filter_butpar_order_text], FALSE );
	XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_up], FALSE );
	XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_down], FALSE );
	XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_up], FALSE );
	XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_down], FALSE );
	XtSetSensitive( xmv_w[k_widget_filter_butpar_order_up], FALSE );
	XtSetSensitive( xmv_w[k_widget_filter_butpar_ord_down], FALSE );

} /* end of xm_initialize */



/*--------------------------------------------------------------------------*/



static void xm_call_cmdexec( Widget widget, char *tag,
	XmCommandCallbackStruct *data )

/* command execution
 */
{
	/* local variables */
#ifdef XXX
	char     cmdline[BC_LINELTH+1];    /* command line */
	unsigned slen;                     /* string length */
#endif
	char     *cmd;                     /* pointer to command line */

	/* executable code */

#ifdef XXX
	slen = data->length-9;
	strncpy( cmdline, (char *)data->value+9, slen );
	cmdline[slen] = '\0';
#endif
	XmStringGetLtoR( data->value, XmSTRING_DEFAULT_CHARSET, &cmd );
	mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, cmd );
	/* do I have to free cmd ??? */

} /* end of xm_call_cmdexec */



/*--------------------------------------------------------------------------*/



static void xm_call_create_widget( Widget widget, int *tag,
	XmAnyCallbackStruct *data )

/* Callback routine on creation of drawing area
 *
 * parameters of routine
 * Widget     widget;          input; widget number of drawing area
 * int        *tag;            input; widget number
 * XmAnyCallbackStruct *data;  input; not used here
 */
{
	/* local variables */
	int      wno = *tag;                 /* widget number */
	char     acttable[BC_LONGSTRLTH+1];  /* action table */
	XtTranslations new_table;            /* translated table */
	Arg      al[1];
#ifdef XXX
	Widget   w_crsr[3];                  /* cursor widgets */
#endif

	/* executable code */

	if  (wno >= XMC_MAXWIDGET || wno < 0)  {
		printf( "*SHM: illegal widget number\n" );
		return;
	} /*endif*/

	/* printf( "[%d]", wno ); */
	xmv_w[wno] = widget;

	switch (wno)  {
	case k_widget_draw:
		strcpy( acttable, "<Motion>: xm_action_motion()\n" );
		strcat( acttable, "<Btn1Down>: xm_action_motion()\n" );
		strcat( acttable, "<Btn1Up>: xm_action_motion()\n" );
		strcat( acttable, "<Btn2Down>: xm_action_motion()\n" );
		strcat( acttable, "<Btn2Up>: xm_action_motion()\n" );
		strcat( acttable, "<Btn3Down>: xm_action_motion()\n" );
		strcat( acttable, "<Btn3Up>: xm_action_motion()\n" );
		if  (GpGetBoolean(cGpB_own_accelerators))
			strcat( acttable, "<KeyPress>: xm_action_motion()\n" );
		new_table = XtParseTranslationTable( acttable );
		XtOverrideTranslations( xmv_w[k_widget_draw], new_table );
		/*XtSetArg( al[0], XmNtranslations, new_table );
		XtSetValues( xmv_w[k_widget_draw], al, 1 );*/
		mg_init_tracedisplay( xmv_w[k_widget_draw], -1, -1,
			GpGetInt(cGpI_top_margin), -1 );
		mg_add_cursor_widget( widget );
		/* I tried next line to enable accelerators, but had no success:
		XtInstallAllAccelerators( xmv_w[k_widget_draw],
			xmv_w[k_widget_menu_main] ); */
		/*
		w_crsr[0] = xmv_w[k_widget_main];
		w_crsr[1] = xmv_w[k_widget_single_draw];
		w_crsr[2] = xmv_w[k_widget_spctwdw];
		mg_set_cursor_widgets( w_crsr, 3 );
		*/
		break;
	case k_widget_single_draw:
		strcpy( acttable, "<Motion>: xm_action_motion()\n" );
		strcat( acttable, "<Btn1Down>: xm_action_motion()\n" );
		strcat( acttable, "<Btn1Up>: xm_action_motion()\n" );
		strcat( acttable, "<Btn2Down>: xm_action_motion()\n" );
		strcat( acttable, "<Btn2Up>: xm_action_motion()\n" );
		strcat( acttable, "<Btn3Down>: xm_action_motion()\n" );
		strcat( acttable, "<Btn3Up>: xm_action_motion()\n" );
		if  (GpGetBoolean(cGpB_own_accelerators))
			strcat( acttable, "<KeyPress>: xm_action_motion()\n" );
		new_table = XtParseTranslationTable( acttable );
		/* XtOverrideTranslations( xmv_w[k_widget_single_draw], new_table ); */
		XtSetArg( al[0], XmNtranslations, new_table );
		XtSetValues( xmv_w[k_widget_single_draw], al, 1 );
		mg_add_cursor_widget( widget );
		/*
		w_crsr[0] = xmv_w[k_widget_main];
		w_crsr[1] = xmv_w[k_widget_single_draw];
		w_crsr[2] = xmv_w[k_widget_spctwdw];
		mg_set_cursor_widgets( w_crsr, 3 );
		*/
		break;
	case k_widget_calibwdw:
		strcpy( acttable, "<Motion>: xm_action_motion()\n" );
		strcat( acttable, "<Btn1Down>: xm_action_motion()\n" );
		strcat( acttable, "<Btn1Up>: xm_action_motion()\n" );
		strcat( acttable, "<Btn2Down>: xm_action_motion()\n" );
		strcat( acttable, "<Btn2Up>: xm_action_motion()\n" );
		strcat( acttable, "<Btn3Down>: xm_action_motion()\n" );
		strcat( acttable, "<Btn3Up>: xm_action_motion()\n" );
		if  (GpGetBoolean(cGpB_own_accelerators))
			strcat( acttable, "<KeyPress>: xm_action_motion()\n" );
		new_table = XtParseTranslationTable( acttable );
		/* XtOverrideTranslations( xmv_w[k_widget_calibwdw], new_table ); */
		XtSetArg( al[0], XmNtranslations, new_table );
		XtSetValues( xmv_w[k_widget_calibwdw], al, 1 );
		break;
	case k_widget_spctwdw:
		strcpy( acttable, "<Motion>: xm_action_motion()\n" );
		strcat( acttable, "<Btn1Down>: xm_action_motion()\n" );
		strcat( acttable, "<Btn1Up>: xm_action_motion()\n" );
		strcat( acttable, "<Btn2Down>: xm_action_motion()\n" );
		strcat( acttable, "<Btn2Up>: xm_action_motion()\n" );
		strcat( acttable, "<Btn3Down>: xm_action_motion()\n" );
		strcat( acttable, "<Btn3Up>: xm_action_motion()\n" );
		if  (GpGetBoolean(cGpB_own_accelerators))
			strcat( acttable, "<KeyPress>: xm_action_motion()\n" );
		new_table = XtParseTranslationTable( acttable );
		/* XtOverrideTranslations( xmv_w[k_widget_spctwdw], new_table ); */
		XtSetArg( al[0], XmNtranslations, new_table );
		XtSetValues( xmv_w[k_widget_spctwdw], al, 1 );
		/* mg_add_cursor_widget( widget ); */ /* this crashes the program */
		break;
	case k_widget_alert_box:
		cu_set_alert_widget( widget );
		break;
	case k_widget_polwdw:
		strcpy( acttable, "<Motion>: xm_action_motion()\n" );
		strcat( acttable, "<Btn1Down>: xm_action_motion()\n" );
		strcat( acttable, "<Btn1Up>: xm_action_motion()\n" );
		strcat( acttable, "<Btn2Down>: xm_action_motion()\n" );
		strcat( acttable, "<Btn2Up>: xm_action_motion()\n" );
		strcat( acttable, "<Btn3Down>: xm_action_motion()\n" );
		strcat( acttable, "<Btn3Up>: xm_action_motion()\n" );
		if  (GpGetBoolean(cGpB_own_accelerators))
			strcat( acttable, "<KeyPress>: xm_action_motion()\n" );
		new_table = XtParseTranslationTable( acttable );
		/* XtOverrideTranslations( xmv_w[k_widget_spctwdw], new_table ); */
		XtSetArg( al[0], XmNtranslations, new_table );
		XtSetValues( xmv_w[k_widget_polwdw], al, 1 );
		break;
	case k_widget_multiplication_box:
	case k_widget_multipli_edit_text:
	case k_widget_multipli_value_label:
		if  (xmv_w[k_widget_multiplication_box] != 0 &&
			xmv_w[k_widget_multipli_edit_text] != 0 &&
			xmv_w[k_widget_multipli_value_label] != 0)
			cu_set_multiplication_widget(
				xmv_w[k_widget_multiplication_box],
				xmv_w[k_widget_multipli_value_label],
				xmv_w[k_widget_multipli_edit_text] );
		break;
	} /*endswitch*/

} /* end of xm_call_create_widget */



/*--------------------------------------------------------------------------*/



static void xm_call_expose_drawingarea( Widget widget, char *tag,
	XmDrawingAreaCallbackStruct *data )

/* Callback routine on creation of drawing area
 *
 * parameters of routine
 * Widget     widget;          input; widget number of drawing area
 * char       *tag;            input; not used here
 * XmDrawingAreaCallbackStruct *data;  input; 
 */
{
#ifdef XXX
	/* executable code, old version */
	static int cnt;
	STATUS status=0;

	if  (widget != xmv_w[k_widget_draw])  return;
	if  (data->reason != XmCR_EXPOSE)  return;
	if  (GpGetInt(cGpI_debug_level) > 1)
		printf( "SHM-dbg2: expose called %d\n", ++cnt );
	mg_tracedisplay( xmv_w[k_widget_draw], &xmv_dspctrl, &status );
	if  (status != BC_NOERROR)
		printf( "*SHM: tracedisplay: return status %d\n", status );
#endif

	/* local variables */

	/* executable code */

	if  (data->reason != XmCR_EXPOSE)  return;
	pix_manage_exposure( &(data->event->xexpose) );

} /* end of xm_call_expose_drawingarea */



/*--------------------------------------------------------------------------*/



static void xm_call_resize_object( Widget widget, int *tag,
	XmAnyCallbackStruct *data )

/* Callback routine on creation of drawing area
 *
 * parameters of routine
 * Widget     widget;          input; widget number of drawin area
 * int        *tag;            input; tag entry
 * XmAnyCallbackStruct *data;  input; 
 */
{
	/* local variables */
	static int   cnt;
	STATUS       status=0;

	/* executable code */

	switch (*tag)  {
	case k_widget_main:
		if  (GpGetInt(cGpI_debug_level) > 1)
			printf( "SHM-dbg2: main window resized\n" );
		break;
	case k_widget_draw:
		if  (xmv_prevent_resize > 0)  {
			xmv_prevent_resize--;
			break;
		} /*endif*/
		if  (xmv_xt_initialized)  {
#ifdef XXX
			/* try to set size of single trace window */
			{
				/* local variables */
				int    n;               /* number of arguments */
				Arg    arglist[3];      /* argument list */
				Window root_ret;        /* root window */
				int    wx, wy;          /* window position */
				unsigned bw, dp;        /* border, depth */
				unsigned ww, wh;        /* window width and height */
				/* executable code */
				XGetGeometry( XtDisplay(xmv_w[k_widget_draw]),
					XtWindow(xmv_w[k_widget_draw]), &root_ret,
					&wx, &wy, &ww, &wh, &bw, &dp );
				n = 0;
				XtSetArg( arglist[n], XmNwidth, ww );  n++;
				XtSetValues( xmv_w[k_widget_single_draw], arglist, 1 );
			}
#endif
			mg_tracedisplay( xmv_w[k_widget_draw], &xmv_dspctrl, &status );
			if  (status != BC_NOERROR)
				printf( "*SHM: tracedisplay: return status %d\n", status );
		} /*endif*/
		break;
	case k_widget_single_draw:
		/*printf( "*SHM: resize_object k_widget_single_draw\n" );*/
		break;
	case k_widget_spctwdw:
		spc_change_display( cSpcResized, &status );
		break;
	case k_widget_pmwdw:
		/*pmm_change_display( cSpcResized, &status );*/
		break;
	case k_widget_polwdw:
		break;
	default:
		printf( "*SHM: resize_object: this cannot happen\n" );
	} /*endswitch*/

} /* end of xm_call_resize_object */



/*--------------------------------------------------------------------------*/



static void xm_call_activate( Widget widget, int *tag,
	XmToggleButtonCallbackStruct *data )

/* Callback routine on button selection
 *
 * parameters of routine
 * Widget     widget;          input; widget number of drawing area
 * int        *tag;            input; tag entry
 * XmPushButtonCallbackStruct *data;  input; 
 */
{
	/* local variables */
	static int  filter_mode=CLC_CREFIL_NONE;  /* flag for filter creation */
	static BOOLEAN fk_unit_deg=TRUE; /* fk distance unit */
	static char    autoevt[BC_FILELTH+1]="";  /* evt-file automatically read in */
	STATUS   status;                 /* return status */
	long     bit;                    /* station bit */
	char     *str;                   /* string pointer */
	char     timestr[BC_LINELTH+1];  /* time string */
	BOOLEAN  ok;                     /* return flag */
	int      i;                      /* counter */
	char     tmpstr[BC_LINELTH+1];   /* scratch string */
	char     cmd[BC_LONGSTRLTH+1];   /* shell command */
	BOOLEAN  is_readnew;             /* is ReadNew, not ReadAgain */
	long     eventid;                /* event ID for evt recovery */

	/* executable code */

	xmh_log_action( NULL, *tag );

	status = BC_NOERROR;
	is_readnew = FALSE;

	/* XtSetSensitive( xmv_w[k_widget_main], FALSE ); */
	mg_set_cursor( MGC_XCRSR_BUSY );

	switch  (*tag)  {
	case k_button_read_grsn_readnew:
		if  (xmv_par.state == CUC_PARSTATE_PROCESS)  {
			status = CUE_SAVE_FIRST;
			break;
		} /*endif*/
		xm_set_processing_state( CUC_PARSTATE_INITIAL );
		is_readnew = TRUE;
		PiClearAllPhases();
		cu_reset_phase( &xmv_phase );
		strcpy( xmv_phase.name, GpGetString(cGpS_auto_phase) );
		cu_phase_box_defaults( xmv_w, &xmv_phase );
		xmv_cmd_filter.autocut = 5.0;  /* shv_globals not yet implemented */
		strcpy( xmv_cmd_filter.name, GpGetString(cGpS_default_filter) );
		mx_filter_box_defaults( xmv_w, &xmv_cmd_filter );
		xmv_dspctrl.zoom = 1.0;
		mx_clear_rotation();
		*xmv_sortstr = '\0';
		*xmv_sortinfo = '\0';
	case k_button_read_grsn_read:
		xmv_phasetrc = NULL;
		cl3_save_parameters( CUC_SAVEMODE_INC, xmv_cpar, &status );
		status = BC_NOERROR;
		str = cu_get_string( xmv_w[k_widget_read_grsn_date] );
		strcpy( xmv_cmd_readg.start, str );
		strcat( xmv_cmd_readg.start, "_" );
		str = cu_get_string( xmv_w[k_widget_read_grsn_time] );
		strcat( xmv_cmd_readg.start, str );
		str = cu_get_string( xmv_w[k_widget_read_grsn_length] );
		if  (sscanf(str,"%f",&xmv_cmd_readg.seclth) != 1)  {
			printf( "*SHM: length conversion error\n" );
			break;
		} /*endif*/
		mx_get_chanlist( xmv_w, &xmv_cmd_readg );
		str = cu_get_string( xmv_w[k_widget_read_grsn_device] );
		strcpy( xmv_cmd_readg.device, str );
		xmv_cmd_readg.seclth *= 60.0;
		xmv_cmd_readg.format = MXC_FORMAT_SEED;
		if  (XtIsManaged(xmv_w[k_widget_read_grsn]))
			XtUnmanageChild( xmv_w[k_widget_read_grsn] );
		/* mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "nr" ); */
		mg_disable_redraw( TRUE );
		mg_print_filter( xmv_w[k_widget_draw], "" );
		cl2_display_busy( xmv_w, TRUE );
		mx_readg( &xmv_cmd_readg, xmv_w[k_widget_draw],
			xmv_w[k_widget_single_draw], &xmv_dspctrl,
			*tag==k_button_read_grsn_read, autoevt, &status );
		if  (Severe(&status))  {
			cl2_display_busy( xmv_w, FALSE );
			break;
		} else if  (mg_dsptrcs() == 0)  {
			status = CUE_NO_TRACES;
			cl2_display_busy( xmv_w, FALSE );
			break;
		} /*endif*/
		if  (strcmp(GpGetString(cGpS_default_filter),"BUT_BP") == 0)  {
			cl_create_filter( xmv_w, CLC_CREFIL_BUT_BP, &status );
			if  (Severe(&status))  {
				cl2_display_busy( xmv_w, FALSE );
				break;
			} /*endif*/
			strcpy( xmv_cmd_filter.name, cu_get_string(
				xmv_w[k_widget_filter_edit_text]) );
		} /*endif*/
		if  (xmv_cmd_filter.name[0] != '\0')  {
			mg_print_busy( xmv_w[k_widget_draw], TRUE );
			mx_filter( &xmv_cmd_filter, xmv_w[k_widget_draw],
				&xmv_dspctrl, &status );
			mg_print_filter( xmv_w[k_widget_draw], xmv_cmd_filter.name );
			if  (Severe(&status))  {
				cl2_display_busy( xmv_w, FALSE );
				break;
			} /*endif*/
		} /*endif*/
		/* mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "rd" ); */
		if  (*tag == k_button_read_grsn_readnew)  {
			cu_reset_phase( &xmv_phase );
			cu_reset_paramsets( &xmv_par );
			xmv_cpar = xmv_par.par;
			xmv_cpar->soft_change = TRUE;
			cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
			xmv_cpar->soft_change = FALSE;
			cl2_param_box_defaults( xmv_w, xmv_cpar /*, &xmv_dspctrl*/ );
		} /*endif*/
		if  (mx_need_rotation())
			mx_rotation( xmv_w[k_widget_draw], &xmv_dspctrl,
				xmv_cpar->b_azimuth, xmv_cpar->source_lat,
				xmv_cpar->source_lon, &status );
		if  (*xmv_sortstr != '\0')  {
			cl4_sort_traces( xmv_w[k_widget_draw], &xmv_dspctrl, xmv_sortstr,
				xmv_cpar, &status );
			mg_print_sortinfo( xmv_w[k_widget_draw], xmv_sortinfo );
		} /*endif*/
		if  (*tag == k_button_read_grsn_read)  {
			mg_print_lastcmd( xmv_w[k_widget_draw], "Read Again" );
		} else {
			mg_print_lastcmd( xmv_w[k_widget_draw], "Read New" );
		} /*endif*/
		if  (*autoevt != '\0' && *tag == k_button_read_grsn_readnew)  {
			i = strlen( autoevt );
			if  (i > 5 && strcmp(autoevt+i-4,".evt") == 0)  {
				if  (GpGetInt(cGpI_debug_level) > 1)
					printf( "SHM-dbg2: restore evt from %s\n", autoevt );
				/*cu_delete_all_phases( xmv_w[k_widget_draw],
					xmv_w[k_widget_single_draw], "*", TRUE );*/
				cl3_restore_from_evt( autoevt, xmv_cpar, &eventid, &status );
				if  (Severe(&status))  {
					cu_alert( status );
					status = cBcNoError;
				} /*endif*/
				if  (GpGetBoolean(cGpB_recover_evid))  xmv_par.evid = eventid;
				xmv_cpar->soft_change = TRUE;
				cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
				xmv_cpar->soft_change = FALSE;
				/*mg_tracedisplay( xmv_w[k_widget_draw], &xmv_dspctrl, &status );*/
			} /*endif*/
		} /*endif*/
		cl2_display_busy( xmv_w, FALSE );
		mg_disable_redraw( FALSE );
		mg_tracedisplay( xmv_w[k_widget_draw], &xmv_dspctrl, &status );
		/* reset 'Keep Traces' if automatic is switched on */
		if  (xmv_keeptrc_auto)  {
			xmv_cmd_readg.keep = FALSE;
			xmv_keeptrc_auto = FALSE;
			XmToggleButtonSetState( xmv_w[k_widget_read_grsn_keep], FALSE, FALSE );
		} /*endif*/
		if  (is_readnew)
			cl3_call_extproc( CL3C_PLUGIN_INIPROC, xmv_w, &xmv_dspctrl, &xmv_par,
				&xmv_phase, &status );
		break;
#ifdef XXX
	case k_button_read_grsn_request:
		if  (XtIsManaged(xmv_w[k_widget_read_grsn]))
			XtUnmanageChild( xmv_w[k_widget_read_grsn] );
		XmUpdateDisplay( xmv_appshell );
		cl2_display_busy( xmv_w, TRUE );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Request" );
		cl4_request_data( xmv_w, &xmv_cmd_readg, &status );
		cl2_display_busy( xmv_w, FALSE );
		if  (status == BC_NOERROR)
			xm_read_requested_data( xmv_w, &status );
		break;
#endif
	case k_button_read_grsn_cancel:
		if  (XtIsManaged(xmv_w[k_widget_read_grsn]))
			XtUnmanageChild( xmv_w[k_widget_read_grsn] );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Read Cancelled" );
		break;
	case k_button_read_grsn_reset:
		cl3_readbox_reset( xmv_w, &xmv_cmd_readg.sl );
		break;
	case k_button_read_grsn_invhdr:
		xmv_cmd_readg.reads_invhdr = !xmv_cmd_readg.reads_invhdr;
		break;
	case k_button_read_grsn_1hz:
		mx_get_chanlist( xmv_w, &xmv_cmd_readg );
		break;
	case k_button_read_grsn_20hz:
		mx_get_chanlist( xmv_w, &xmv_cmd_readg );
		break;
	case k_button_read_grsn_80hz:
		mx_get_chanlist( xmv_w, &xmv_cmd_readg );
		break;
	case k_button_read_grsn_edit_hz:
		mx_get_chanlist( xmv_w, &xmv_cmd_readg );
		break;
	case k_button_read_grsn_station+0:
	case k_button_read_grsn_station+1:
	case k_button_read_grsn_station+2:
	case k_button_read_grsn_station+3:
	case k_button_read_grsn_station+4:
	case k_button_read_grsn_station+5:
	case k_button_read_grsn_station+6:
	case k_button_read_grsn_station+7:
	case k_button_read_grsn_station+8:
	case k_button_read_grsn_station+9:
	case k_button_read_grsn_station+10:
	case k_button_read_grsn_station+11:
	case k_button_read_grsn_station+12:
	case k_button_read_grsn_station+13:
	case k_button_read_grsn_station+14:
	case k_button_read_grsn_station+15:
	case k_button_read_grsn_station+16:
	case k_button_read_grsn_station+17:
	case k_button_read_grsn_station+18:
	case k_button_read_grsn_station+19:
	case k_button_read_grsn_station+20:
	case k_button_read_grsn_station+21:
	case k_button_read_grsn_station+22:
	case k_button_read_grsn_station+23:
	case k_button_read_grsn_station+24:
	case k_button_read_grsn_station+25:
	case k_button_read_grsn_station+26:
	case k_button_read_grsn_station+27:
	case k_button_read_grsn_station+28:
	case k_button_read_grsn_station+29:
		bit = 1 << (*tag - k_button_read_grsn_station);
		if  (data->set)  {
			xmv_cmd_readg.stations |= bit;
		} else {
			xmv_cmd_readg.stations &= ~bit;
		} /*endif*/
		break;
	case k_button_read_grsn_grf:
		if  (data->set)  {
			for  (bit=xmv_cmd_readg.sl.set1start;
				bit<=xmv_cmd_readg.sl.set1end; bit++)  {
				if  (xmv_cmd_readg.sl.code[bit][0] != '-')  {
					xmv_cmd_readg.stations |= (1<<bit);
					XmToggleButtonSetState(
						xmv_w[k_widget_read_grsn_station+bit], TRUE, FALSE );
				} /*endif*/
			} /*endfor*/
		} else {
			for  (bit=xmv_cmd_readg.sl.set1start;
				bit<=xmv_cmd_readg.sl.set1end; bit++)  {
				if  (xmv_cmd_readg.sl.code[bit][0] != '-')  {
					xmv_cmd_readg.stations &= ~(1<<bit);
					XmToggleButtonSetState(
						xmv_w[k_widget_read_grsn_station+bit], FALSE, FALSE );
				} /*endif*/
			} /*endfor*/
		} /*endif*/
		break;
	case k_button_read_grsn_grsn:
		if  (data->set)  {
			for  (bit=xmv_cmd_readg.sl.set2start;
				bit<=xmv_cmd_readg.sl.set2end; bit++)  {
				if  (xmv_cmd_readg.sl.code[bit][0] != '-')  {
					xmv_cmd_readg.stations |= (1<<bit);
					XmToggleButtonSetState(
						xmv_w[k_widget_read_grsn_station+bit], TRUE, FALSE );
				} /*endif*/
			} /*endfor*/
		} else {
			for  (bit=xmv_cmd_readg.sl.set2start;
				bit<=xmv_cmd_readg.sl.set2end; bit++)  {
				if  (xmv_cmd_readg.sl.code[bit][0] != '-')  {
					xmv_cmd_readg.stations &= ~(1<<bit);
					XmToggleButtonSetState(
						xmv_w[k_widget_read_grsn_station+bit], FALSE, FALSE );
				} /*endif*/
			} /*endfor*/
		} /*endif*/
		break;
	case k_button_read_grsn_all:
		cl4_select_all_possible_stations( xmv_w, &xmv_cmd_readg, &status );
		break;
	case k_button_read_grsn_comp_z:
	case k_button_read_grsn_comp_n:
	case k_button_read_grsn_comp_e:
	case k_button_read_grsn_comp_edit:
		xmv_cmd_readg.comp[0] ='\0';
		if  (XmToggleButtonGetState(xmv_w[k_widget_read_grsn_comp_z]))
			strcat( xmv_cmd_readg.comp, "Z" );
		if  (XmToggleButtonGetState(xmv_w[k_widget_read_grsn_comp_n]))
			strcat( xmv_cmd_readg.comp, "N" );
		if  (XmToggleButtonGetState(xmv_w[k_widget_read_grsn_comp_e]))
			strcat( xmv_cmd_readg.comp, "E" );
		if  (XmToggleButtonGetState(xmv_w[k_widget_read_grsn_comp_edit]))
			if  (strlen(xmv_cmd_readg.comp)
				+strlen(cu_get_string(xmv_w[k_widget_read_grsn_comp_text]))
				< MXC_COMP_STRLTH)
				strcat( xmv_cmd_readg.comp,
					cu_get_string(xmv_w[k_widget_read_grsn_comp_text]) );
		break;
	case k_button_read_grsn_secup:
		cu_set_read_time( xmv_w[k_widget_read_grsn_date],
			xmv_w[k_widget_read_grsn_time], 10.0, &status );
		break;
	case k_button_read_grsn_secdn:
		cu_set_read_time( xmv_w[k_widget_read_grsn_date],
			xmv_w[k_widget_read_grsn_time], -10.0, &status );
		break;
	case k_button_read_grsn_minup:
		cu_set_read_time( xmv_w[k_widget_read_grsn_date],
			xmv_w[k_widget_read_grsn_time], 60.0, &status );
		break;
	case k_button_read_grsn_mindn:
		cu_set_read_time( xmv_w[k_widget_read_grsn_date],
			xmv_w[k_widget_read_grsn_time], -60.0, &status );
		break;
	case k_button_read_grsn_hourup:
		cu_set_read_time( xmv_w[k_widget_read_grsn_date],
			xmv_w[k_widget_read_grsn_time], 3600.0, &status );
		break;
	case k_button_read_grsn_hourdn:
		cu_set_read_time( xmv_w[k_widget_read_grsn_date],
			xmv_w[k_widget_read_grsn_time], -3600.0, &status );
		break;
	case k_button_read_grsn_dayup:
		cu_set_read_time( xmv_w[k_widget_read_grsn_date],
			xmv_w[k_widget_read_grsn_time], 86400.0, &status );
		break;
	case k_button_read_grsn_daydn:
		cu_set_read_time( xmv_w[k_widget_read_grsn_date],
			xmv_w[k_widget_read_grsn_time], -86400.0, &status );
		break;
	case k_button_read_grsn_monthup:
		cu_set_read_time( xmv_w[k_widget_read_grsn_date],
			xmv_w[k_widget_read_grsn_time], CUC_MONTH_INC, &status);
		break;
	case k_button_read_grsn_monthdn:
		cu_set_read_time( xmv_w[k_widget_read_grsn_date],
			xmv_w[k_widget_read_grsn_time], CUC_MONTH_DEC, &status);
		break;
	case k_button_read_grsn_yearup:
		cu_set_read_time( xmv_w[k_widget_read_grsn_date],
			xmv_w[k_widget_read_grsn_time], CUC_YEAR_INC, &status );
		break;
	case k_button_read_grsn_yeardn:
		cu_set_read_time( xmv_w[k_widget_read_grsn_date],
			xmv_w[k_widget_read_grsn_time], CUC_YEAR_DEC, &status );
		break;
	case k_button_read_grsn_nexttime:
		cu_set_read_time_file( xmv_w, CUC_LIST_NEXT, autoevt, &status );
		break;
	case k_button_read_grsn_prevtime:
		cu_set_read_time_file( xmv_w, CUC_LIST_PREV, autoevt, &status );
		break;
	case k_button_read_grsn_keep:
		xmv_cmd_readg.keep = data->set;
		break;
	case k_button_read_grsn_eventfile:
		cl_file_select_init( xmv_w[k_widget_filesel], CLC_PATH_EVENTS, &status );
		break;
	case k_button_read_grsn_device:
		if  (XmToggleButtonGetState(xmv_w[k_widget_read_grsn_fbox]))
			cl_file_select_init( xmv_w[k_widget_filesel], CLC_PATH_SFD, &status );
		break;
	case k_button_read_grsn_todaysfd:
		cl4_manage_sfdfile( xmv_w, &xmv_cmd_readg );
		break;
	case k_button_read_grsn_nextdiag:
		cu_next_read_dialog( xmv_w );
		break;
	case k_button_phase_dismiss:
		if  (XtIsManaged(xmv_w[k_widget_phase_box]))
			XtUnmanageChild( xmv_w[k_widget_phase_box] );
		break;
	case k_button_phase_ok:
		mg_plot_phases( xmv_w[k_widget_draw], xmv_dspctrl.show_phase_acc,
			&status );
		cu_rename_phase( xmv_w );
		mg_plot_phases( xmv_w[k_widget_draw], xmv_dspctrl.show_phase_acc,
			&status );
		break;
	case k_button_phase_delete:
#		ifdef XXX
		mg_delete_phase( &xmv_phase, &ok );
		if  (ok)  {
			mg_mark_one_phase( xmv_w[k_widget_draw],
				xmv_w[k_widget_single_draw], &xmv_phase,
				xmv_onset_acc==CUC_ACCURACY_DISPLAY );
			xmv_phase.name[0] = '\0';
		} /*endif*/
#		endif
		cu_delete_all_phases( xmv_w[k_widget_draw], xmv_w[k_widget_single_draw],
			cu_get_string(xmv_w[k_widget_phase_name_text]), TRUE );
		break;
	case k_button_phase_sign_p:
		if  (data->set)  xmv_phase.sign = 1;
		break;
	case k_button_phase_sign_m:
		if  (data->set)  xmv_phase.sign = -1;
		break;
	case k_button_phase_sign_0:
		if  (data->set)  xmv_phase.sign = 0;
		break;
	case k_button_phase_spec_e:
		if  (data->set)  xmv_phase.spec = cPiSpecEmergent;
		break;
	case k_button_phase_spec_i:
		if  (data->set)  xmv_phase.spec = cPiSpecImpulsive;
		break;
	case k_button_phase_reliab_yes:
		if  (data->set)  xmv_phase.reliable = TRUE;
		break;
	case k_button_phase_reliab_no:
		if  (data->set)  xmv_phase.reliable = FALSE;
		break;
	case k_button_phase_name_b:
		cu_set_string( xmv_w[k_widget_phase_name_text], "beam" );
		break;
	case k_button_phase_name_Pn:
		cu_set_string( xmv_w[k_widget_phase_name_text], "Pn" );
		break;
	case k_button_phase_name_Pg:
		cu_set_string( xmv_w[k_widget_phase_name_text], "Pg" );
		break;
	case k_button_phase_name_Sn:
		cu_set_string( xmv_w[k_widget_phase_name_text], "Sn" );
		break;
	case k_button_phase_name_Sg:
		cu_set_string( xmv_w[k_widget_phase_name_text], "Sg" );
		break;
	case k_button_phase_name_Lg:
		cu_set_string( xmv_w[k_widget_phase_name_text], "Lg" );
		break;
	case k_button_phase_name_Rg:
		cu_set_string( xmv_w[k_widget_phase_name_text], "Rg" );
		break;
	case k_button_phase_name_P:
		cu_set_string( xmv_w[k_widget_phase_name_text], "P" );
		break;
	case k_button_phase_name_pP:
		cu_set_string( xmv_w[k_widget_phase_name_text], "pP" );
		break;
	case k_button_phase_name_sP:
		cu_set_string( xmv_w[k_widget_phase_name_text], "sP" );
		break;
	case k_button_phase_name_PKPdf:
		cu_set_string( xmv_w[k_widget_phase_name_text], "PKPdf" );
		break;
	case k_button_phase_name_PKPbc:
		cu_set_string( xmv_w[k_widget_phase_name_text], "PKPbc" );
		break;
	case k_button_phase_name_PKPab:
		cu_set_string( xmv_w[k_widget_phase_name_text], "PKPab" );
		break;
	case k_button_phase_name_pPKPdf:
		cu_set_string( xmv_w[k_widget_phase_name_text], "pPKPdf" );
		break;
	case k_button_phase_name_pPKPbc:
		cu_set_string( xmv_w[k_widget_phase_name_text], "pPKPbc" );
		break;
	case k_button_phase_name_pPKPab:
		cu_set_string( xmv_w[k_widget_phase_name_text], "pPKPab" );
		break;
	case k_button_phase_name_PKP:
		cu_set_string( xmv_w[k_widget_phase_name_text], "PKP" );
		break;
	case k_button_phase_name_PP:
		cu_set_string( xmv_w[k_widget_phase_name_text], "PP" );
		break;
	case k_button_phase_name_pPP:
		cu_set_string( xmv_w[k_widget_phase_name_text], "pPP" );
		break;
	case k_button_phase_name_S:
		cu_set_string( xmv_w[k_widget_phase_name_text], "S" );
		break;
	case k_button_phase_name_SKS:
		cu_set_string( xmv_w[k_widget_phase_name_text], "SKS" );
		break;
	case k_button_phase_name_PKKP:
		cu_set_string( xmv_w[k_widget_phase_name_text], "PKKP" );
		break;
	case k_button_phase_name_PKPPKP:
		cu_set_string( xmv_w[k_widget_phase_name_text], "PKPPKP" );
		break;
	case k_button_phase_name_PcP:
		cu_set_string( xmv_w[k_widget_phase_name_text], "PcP" );
		break;
	case k_button_phase_name_L:
		cu_set_string( xmv_w[k_widget_phase_name_text], CUC_LP_PHASE_NAME );
		break;
	case k_button_phase_name_X:
		cu_set_string( xmv_w[k_widget_phase_name_text], "X" );
		break;
	case k_button_phase_qual_0:
	case k_button_phase_qual_1:
	case k_button_phase_qual_2:
	case k_button_phase_qual_3:
	case k_button_phase_qual_4:
	case k_button_phase_qual_5:
	case k_button_phase_qual_6:
	case k_button_phase_qual_7:
	case k_button_phase_qual_8:
	case k_button_phase_qual_9:
		xmv_phase.quality = (*tag) - k_button_phase_qual_0;
		xmv_phase.weight = cu_quality2weight( xmv_phase.quality );
		break;
	case k_button_phase_acc_none:
		mg_plot_phases( xmv_w[k_widget_draw], xmv_dspctrl.show_phase_acc,
			&status );
		xmv_onset_acc = CUC_ACCURACY_NONE;
		xmv_dspctrl.show_phase_acc = FALSE;
		mg_plot_phases( xmv_w[k_widget_draw], xmv_dspctrl.show_phase_acc,
			&status );
		break;
	case k_button_phase_acc_query:
		mg_plot_phases( xmv_w[k_widget_draw], xmv_dspctrl.show_phase_acc,
			&status );
		xmv_onset_acc = CUC_ACCURACY_QUERY;
		xmv_dspctrl.show_phase_acc = FALSE;
		mg_plot_phases( xmv_w[k_widget_draw], xmv_dspctrl.show_phase_acc,
			&status );
		break;
	case k_button_phase_acc_display:
		mg_plot_phases( xmv_w[k_widget_draw], xmv_dspctrl.show_phase_acc,
			&status );
		xmv_onset_acc = CUC_ACCURACY_DISPLAY;
		xmv_dspctrl.show_phase_acc = TRUE;
		mg_plot_phases( xmv_w[k_widget_draw], xmv_dspctrl.show_phase_acc,
			&status );
		break;
	case k_button_param_dismiss:
		if  (XtIsManaged(xmv_w[k_widget_param_box]))
			XtUnmanageChild( xmv_w[k_widget_param_box] );
		break;
	case k_button_param_hypoloc:
		/*
		mg_print_lastcmd( xmv_w[k_widget_draw], "Hypoellipse Box" );
		if  (!XtIsManaged(xmv_w[k_widget_hypoellipse_box]))
			XtManageChild( xmv_w[k_widget_hypoellipse_box] );
		break;
		*/
	case k_button_hypo_ok:
		/*
		if  (XtIsManaged(xmv_w[k_widget_hypoellipse_box]))
			XtUnmanageChild( xmv_w[k_widget_hypoellipse_box] );
		*/
		XmUpdateDisplay( xmv_appshell );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Locate External" );
		cl2_display_busy( xmv_w, TRUE );
		XtSetSensitive( xmv_w[k_widget_menu_main], FALSE );
		cu_get_param_values( xmv_w, xmv_cpar );
		cl3_regio_localization(
			xmv_w, cu_get_string(xmv_w[k_widget_phase_name_text]),
			&xmv_par, &status );
		xmv_cpar->soft_change = TRUE;
		cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
		xmv_cpar->soft_change = FALSE;
		cu_print_param_values( xmv_cpar );
		XtSetSensitive( xmv_w[k_widget_menu_main], TRUE );
		cl2_display_busy( xmv_w, FALSE );
		break;
/*
	case k_button_hypo_cancel:
		mg_print_lastcmd( xmv_w[k_widget_draw], "Hypoellipse Cancelled" );
		if  (XtIsManaged(xmv_w[k_widget_hypoellipse_box]))
			XtUnmanageChild( xmv_w[k_widget_hypoellipse_box] );
		break;
*/
	case k_button_hypo_depth_free:
		cu_set_string( xmv_w[k_widget_hypo_depth_text], "free" );
		break;
	case k_button_hypo_depth_0:
		cu_set_string( xmv_w[k_widget_hypo_depth_text], "0" );
		break;
	case k_button_hypo_depth_5:
		cu_set_string( xmv_w[k_widget_hypo_depth_text], "5" );
		break;
	case k_button_hypo_depth_10:
		cu_set_string( xmv_w[k_widget_hypo_depth_text], "10" );
		break;
	case k_button_hypo_depth_15:
		cu_set_string( xmv_w[k_widget_hypo_depth_text], "15" );
		break;
	case k_button_hypo_depth_20:
		cu_set_string( xmv_w[k_widget_hypo_depth_text], "20" );
		break;
	case k_button_hypo_depth_33:
		cu_set_string( xmv_w[k_widget_hypo_depth_text], "33" );
		break;
	case k_button_hypo_use_s_yes:
	case k_button_hypo_use_s_no:
		break;
	case k_button_param_theo:
		mg_print_lastcmd( xmv_w[k_widget_draw], "Theo Phase Box" );
		if  (XtIsManaged(xmv_w[k_widget_theo_phase_box]))  {
			XtUnmanageChild( xmv_w[k_widget_theo_phase_box] );
		} else {
			XtManageChild( xmv_w[k_widget_theo_phase_box] );
		} /*endif*/
		break;
	case k_button_theo_phase_ok:
		if  (XtIsManaged(xmv_w[k_widget_theo_phase_box]))
			XtUnmanageChild( xmv_w[k_widget_theo_phase_box] );
		XmUpdateDisplay( xmv_appshell );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Theo (Tables)" );
		cl2_display_busy( xmv_w, TRUE );
		XtSetSensitive( xmv_w[k_widget_menu_main], FALSE );
		cu_get_param_values( xmv_w, xmv_cpar );
		cl3_set_theo_phases( xmv_w, &status );
		if  (Severe(&status))  break;
		cu_theo_arrivals( xmv_w[k_widget_draw], xmv_w[k_widget_single_draw],
			xmv_cpar, &status );
		XtSetSensitive( xmv_w[k_widget_menu_main], TRUE );
		cl2_display_busy( xmv_w, FALSE );
		break;
	case k_button_theo_phase_cancel:
		if  (XtIsManaged(xmv_w[k_widget_theo_phase_box]))
			XtUnmanageChild( xmv_w[k_widget_theo_phase_box] );
		break;
	case k_button_param_theo_ext:
		mg_print_lastcmd( xmv_w[k_widget_draw], "Theo (ExtProg)" );
		cl2_display_busy( xmv_w, TRUE );
		XtSetSensitive( xmv_w[k_widget_menu_main], FALSE );
		cu_get_param_values( xmv_w, xmv_cpar );
		cl2_extern_theo( xmv_w[k_widget_draw], xmv_w[k_widget_single_draw],
			xmv_cpar, xmv_onset_acc==CUC_ACCURACY_DISPLAY, &status );
		XtSetSensitive( xmv_w[k_widget_menu_main], TRUE );
		cl2_display_busy( xmv_w, FALSE );
		break;
	case k_button_param_depth:
		mg_print_lastcmd( xmv_w[k_widget_draw], "Depth" );
		cl2_display_busy( xmv_w, TRUE );
		cu_get_param_values( xmv_w, xmv_cpar );
		cl_fit_depth( xmv_cpar->distance, &(xmv_cpar->depth),
			&(xmv_cpar->err.dep), &(xmv_cpar->depth_type), &status );
		xmv_cpar->soft_change = TRUE;
		cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
		xmv_cpar->soft_change = FALSE;
		cu_print_param_values( xmv_cpar );
		cl2_display_busy( xmv_w, FALSE );
		break;
	case k_button_param_locq_tooweak:
		xmv_cpar->loc_quality = CUC_LOCQ_TOOWEAK;
		break;
	case k_button_param_locq_incoherent:
		xmv_cpar->loc_quality = CUC_LOCQ_INCOHERENT;
		break;
	case k_button_param_locq_nobearing:
		xmv_cpar->loc_quality = CUC_LOCQ_NOBEARING;
		break;
	case k_button_param_locq_region:
		xmv_cpar->loc_quality = CUC_LOCQ_REGION;
		break;
	case k_button_param_locq_reliable:
		xmv_cpar->loc_quality = CUC_LOCQ_RELIABLE;
		break;
	case k_button_param_locq_undefined:
		xmv_cpar->loc_quality = CUC_LOCQ_UNDEFINED;
		break;
	case k_button_filter_none:
		if  (! data->set)  break;
		filter_mode = CLC_CREFIL_NONE;
		cu_set_string( xmv_w[k_widget_filter_edit_text], "" );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_text], FALSE );
		break;
	case k_button_filter_other:
		if  (! data->set)  break;
		filter_mode = CLC_CREFIL_NONE;
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_down], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_down], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_ord_down], FALSE );
		break;
	case k_button_filter_wwssn_sp:
		if  (! data->set)  break;
		filter_mode = CLC_CREFIL_NONE;
		cu_set_string( xmv_w[k_widget_filter_edit_text], "S+G_WWSSN_SP" );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_down], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_down], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_ord_down], FALSE );
		break;
	case k_button_filter_wwssn_lp:
		if  (! data->set)  break;
		filter_mode = CLC_CREFIL_NONE;
		cu_set_string( xmv_w[k_widget_filter_edit_text], "S+WWSSN_LP" );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_down], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_down], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_ord_down], FALSE );
		break;
	case k_button_filter_lrsm_sp:
		if  (! data->set)  break;
		filter_mode = CLC_CREFIL_NONE;
		cu_set_string( xmv_w[k_widget_filter_edit_text], "S+LRSM_SP" );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_down], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_down], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_ord_down], FALSE );
		break;
	case k_button_filter_lrsm_lp:
		if  (! data->set)  break;
		filter_mode = CLC_CREFIL_NONE;
		cu_set_string( xmv_w[k_widget_filter_edit_text], "S+LRSM_LP" );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_down], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_down], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_ord_down], FALSE );
		break;
	case k_button_filter_kirnos:
		if  (! data->set)  break;
		filter_mode = CLC_CREFIL_NONE;
		cu_set_string( xmv_w[k_widget_filter_edit_text], "S+KIRNOS" );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_down], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_down], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_ord_down], FALSE );
		break;
	case k_button_filter_woodand:
		if  (! data->set)  break;
		filter_mode = CLC_CREFIL_NONE;
		cu_set_string( xmv_w[k_widget_filter_edit_text], "S+WOODAND" );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_down], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_down], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_ord_down], FALSE );
		break;
	case k_button_filter_standard_bp:
		if  (! data->set)  break;
		filter_mode = CLC_CREFIL_NONE;
		cu_set_string( xmv_w[k_widget_filter_edit_text], "S+STANDARD_BP" );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_down], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_down], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_ord_down], FALSE );
		break;
	case k_button_filter_sro_lp:
		if  (! data->set)  break;
		filter_mode = CLC_CREFIL_NONE;
		cu_set_string( xmv_w[k_widget_filter_edit_text], "S+SRO_LP" );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_down], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_down], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_ord_down], FALSE );
		break;
	case k_button_filter_but_bp:
		if  (! data->set)  break;
		filter_mode = CLC_CREFIL_BUT_BP;
		cu_set_string( xmv_w[k_widget_filter_edit_text], "---" );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_text], TRUE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_text], TRUE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_text], TRUE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_up], TRUE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_down], TRUE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_up], TRUE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_down], TRUE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_up], TRUE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_ord_down], TRUE );
		break;
	case k_button_filter_but_lp:
		if  (! data->set)  break;
		filter_mode = CLC_CREFIL_BUT_LP;
		cu_set_string( xmv_w[k_widget_filter_edit_text], "---" );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_text], TRUE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_text], TRUE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_down], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_up], TRUE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_down], TRUE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_up], TRUE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_ord_down], TRUE );
		break;
	case k_button_filter_but_hp:
		if  (! data->set)  break;
		filter_mode = CLC_CREFIL_BUT_HP;
		cu_set_string( xmv_w[k_widget_filter_edit_text], "---" );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_text], TRUE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_text], TRUE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_up], TRUE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_down], TRUE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_down], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_up], TRUE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_ord_down], TRUE );
		break;
	case k_button_filter_displace:
		if  (! data->set)  break;
		filter_mode = CLC_CREFIL_NONE;
		cu_set_string( xmv_w[k_widget_filter_edit_text], "S+DISPL" );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_text], TRUE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_text], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_hi_down], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_up], TRUE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_lo_down], TRUE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_order_up], FALSE );
		XtSetSensitive( xmv_w[k_widget_filter_butpar_ord_down], FALSE );
		break;
	case k_button_filter_butpar_lo_up:
		cl_manage_filter_bounds( xmv_w, CLC_BUTPAR_LO_UP, &status );
		break;
	case k_button_filter_butpar_lo_down:
		cl_manage_filter_bounds( xmv_w, CLC_BUTPAR_LO_DOWN, &status );
		break;
	case k_button_filter_butpar_hi_up:
		cl_manage_filter_bounds( xmv_w, CLC_BUTPAR_HI_UP, &status );
		break;
	case k_button_filter_butpar_hi_down:
		cl_manage_filter_bounds( xmv_w, CLC_BUTPAR_HI_DOWN, &status );
		break;
	case k_button_filter_butpar_order_up:
		cl_manage_filter_bounds( xmv_w, CLC_BUTPAR_ORDER_UP, &status );
		break;
	case k_button_filter_butpar_order_down:
		cl_manage_filter_bounds( xmv_w, CLC_BUTPAR_ORDER_DOWN, &status );
		break;
	case k_button_filter_ok:
		if  (filter_mode != CLC_CREFIL_NONE)  {
			cl_create_filter( xmv_w, filter_mode, &status );
			if  (Severe(&status))  break;
		} /*endif*/
		strcpy( xmv_cmd_filter.name, cu_get_string(
			xmv_w[k_widget_filter_edit_text]) );
		if  (strcmp(xmv_cmd_filter.name,"S+DISPL") == 0)  {
			strcpy( tmpstr,
				cu_get_string( xmv_w[k_widget_filter_butpar_lo_text]) );
			i = strlen( tmpstr ) - 1;
			if  (tmpstr[i] == 's')  tmpstr[i] = 'S';
			i = 0;
			while  (tmpstr[i] == ' ' && i < strlen(tmpstr))
				i++;
			strcat( xmv_cmd_filter.name, tmpstr+i );
			if  (GpGetInt(cGpI_debug_level) > 3)
				printf( "SHM-dbg4: %s\n", xmv_cmd_filter.name );
		} /*endif*/
		XtUnmanageChild( xmv_w[k_widget_filter_box] );
		XmUpdateDisplay( xmv_appshell );
		cl2_display_busy( xmv_w, TRUE );
		mx_filter( &xmv_cmd_filter, xmv_w[k_widget_draw],
			&xmv_dspctrl, &status );
		if  (*xmv_sortstr != '\0')  {
			cl4_sort_traces( xmv_w[k_widget_draw], &xmv_dspctrl, xmv_sortstr,
				xmv_cpar, &status );
			mg_print_sortinfo( xmv_w[k_widget_draw], xmv_sortinfo );
		} /*endif*/
		mg_print_filter( xmv_w[k_widget_draw], xmv_cmd_filter.name );
		cl2_display_busy( xmv_w, FALSE );
		cl_set_simfilter_active( xmv_w, TRUE );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Filter Execution" );
		break;
	case k_button_filter_cancel:
		mg_print_lastcmd( xmv_w[k_widget_draw], "Filter Cancelled" );
		XtUnmanageChild( xmv_w[k_widget_filter_box] );
		break;
	case k_button_filter_autocut_0:
		xmv_cmd_filter.autocut = 0.0;
		break;
	case k_button_filter_autocut_5sec:
		xmv_cmd_filter.autocut = 5.0;
		break;
	case k_button_filter_autocut_20sec:
		xmv_cmd_filter.autocut = 20.0;
		break;
	case k_button_filter_autocut_1min:
		xmv_cmd_filter.autocut = 60.0;
		break;
	case k_button_filter_autocut_3min:
		xmv_cmd_filter.autocut = 180.0;
		break;
	case k_button_filter_autocut_5min:
		xmv_cmd_filter.autocut = 300.0;
		break;
	case k_button_filter_autocut_10min:
		xmv_cmd_filter.autocut = 600.0;
		break;
	case k_button_phase_type_other:
		xmv_cpar->source_type = CUC_TYPE_OTHER;
		break;
	case k_button_phase_type_local:
		xmv_cpar->source_type = CUC_TYPE_LOCAL_QUAKE;
		break;
	case k_button_phase_type_regio:
		xmv_cpar->source_type = CUC_TYPE_REGIO_QUAKE;
		break;
	case k_button_phase_type_tele:
		xmv_cpar->source_type = CUC_TYPE_TELE_QUAKE;
		break;
	case k_button_phase_type_nuclear:
		xmv_cpar->source_type = CUC_TYPE_NUCLEAR;
		break;
	case k_button_phase_type_blast:
		xmv_cpar->source_type = CUC_TYPE_BLAST;
		break;
	case k_button_phase_type_mining:
		xmv_cpar->source_type = CUC_TYPE_MINING;
		break;
	case k_button_multipli_1_3:
		cu_do_multiplication( 1./3., &status );
		break;
	case k_button_multipli_1_2:
		cu_do_multiplication( 1./2., &status );
		break;
	case k_button_multipli_1:
		cu_do_multiplication( 1., &status );
		break;
	case k_button_multipli_2:
		cu_do_multiplication( 2., &status );
		break;
	case k_button_multipli_3:
		cu_do_multiplication( 3., &status );
		break;
	case k_button_multipli_4:
		cu_do_multiplication( 4., &status );
		break;
	case k_button_multipli_edit_mul:
		cu_do_multiplication( CUC_MULTIPLI_EDIT_MUL, &status );
		break;
	case k_button_multipli_edit_div:
		cu_do_multiplication( CUC_MULTIPLI_EDIT_DIV, &status );
		break;
	case k_button_analyst_ok:
		mx_analyst_name( xmv_w, &xmv_dspctrl, &status );
		break;
	case k_button_infsource_ok:
		strcpy( xmv_cpar->source,
			cu_get_string(xmv_w[k_widget_infsource_name_text]) );
		XtUnmanageChild( xmv_w[k_widget_infsource_box] );
		break;
	case k_button_param_set_1:
	case k_button_param_set_2:
	case k_button_param_set_3:
		cu_get_param_values( xmv_w, xmv_cpar );
		xmv_par.parno = *tag - k_button_param_set_1;
		xmv_cpar = xmv_par.par + xmv_par.parno;
		xmv_cpar->soft_change = TRUE;
		cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
		xmv_cpar->soft_change = FALSE;
		break;
	case k_button_fk_okauto:
	case k_button_fk_ok:
		XtUnmanageChild( xmv_w[k_widget_fk_box] );
		XmUpdateDisplay( xmv_appshell );
		cl3_perform_fk( xmv_w, fk_unit_deg, (*tag == k_button_fk_ok),
			"", &status );
		if  (*tag != k_button_fk_okauto)
			break;
	case k_button_fk_get_values:
		XtUnmanageChild( xmv_w[k_widget_fk_box] );
		cl3_fk_get_values( xmv_cpar, &status );
		xmv_cpar->soft_change = TRUE;
		cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
		xmv_cpar->soft_change = FALSE;
		break;
	case k_button_fk_cancel:
		XtUnmanageChild( xmv_w[k_widget_fk_box] );
		break;
	case k_button_fk_unit_deg:
		fk_unit_deg = TRUE;
		break;
	case k_button_fk_unit_km:
		fk_unit_deg = FALSE;
		break;
	case k_button_speccmd_hide:
		XtUnmanageChild( xmv_w[k_widget_speccmd_box] );
		break;
	case k_button_speccmd_1:
	case k_button_speccmd_1+1:
	case k_button_speccmd_1+2:
	case k_button_speccmd_1+3:
	case k_button_speccmd_1+4:
	case k_button_speccmd_1+5:
	case k_button_speccmd_1+6:
	case k_button_speccmd_1+7:
	case k_button_speccmd_1+8:
	case k_button_speccmd_1+9:
	case k_button_speccmd_1+10:
	case k_button_speccmd_1+11:
	case k_button_speccmd_1+12:
	case k_button_speccmd_1+13:
	case k_button_speccmd_1+14:
	case k_button_speccmd_1+15:
		str = cu_get_string(
			xmv_w[k_widget_speccmd_1_text+(*tag)-k_button_speccmd_1] );
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, str );
		mg_print_lastcmd( xmv_w[k_widget_draw], str );
		break;
	case k_button_vespa_ok:
		mg_print_lastcmd( xmv_w[k_widget_draw], "Vespagram" );
		cl4_do_vespa( xmv_w, &xmv_dspctrl, xmv_cpar->b_azimuth, TRUE, &status );
		XtSetSensitive( xmv_w[k_widget_vespa_export_button], TRUE );
		break;
	case k_button_vespa_cancel:
		XtUnmanageChild( xmv_w[k_widget_vespa_input_box] );
		break;
	case k_button_vespa_undo:
		cl4_do_vespa( xmv_w, &xmv_dspctrl, xmv_cpar->b_azimuth, FALSE, &status );
		XtUnmanageChild( xmv_w[k_widget_vespa_input_box] );
		break;
	case k_button_vespa_power_1:
		cu_set_string( xmv_w[k_widget_vespa_power_text], "1" );
		break;
	case k_button_vespa_power_2:
		cu_set_string( xmv_w[k_widget_vespa_power_text], "2" );
		break;
	case k_button_vespa_power_3:
		cu_set_string( xmv_w[k_widget_vespa_power_text], "3" );
		break;
	case k_button_vespa_power_5:
		cu_set_string( xmv_w[k_widget_vespa_power_text], "5" );
		break;
	case k_button_vespa_power_10:
		cu_set_string( xmv_w[k_widget_vespa_power_text], "10" );
		break;
	case k_button_vespa_slostep_1:
		cu_set_string( xmv_w[k_widget_vespa_slostep_text], "0.2" );
		break;
	case k_button_vespa_slostep_2:
		cu_set_string( xmv_w[k_widget_vespa_slostep_text], "0.5" );
		break;
	case k_button_vespa_slostep_3:
		cu_set_string( xmv_w[k_widget_vespa_slostep_text], "1.0" );
		break;
	case k_button_vespa_slohi_1:
		cu_set_string( xmv_w[k_widget_vespa_slohi_text], "5.0" );
		break;
	case k_button_vespa_slohi_2:
		cu_set_string( xmv_w[k_widget_vespa_slohi_text], "8.0" );
		break;
	case k_button_vespa_slohi_3:
		cu_set_string( xmv_w[k_widget_vespa_slohi_text], "12.0" );
		break;
	case k_button_vespa_export:
		cl4_vespa_export( xmv_w, &status );
		break;
	case k_button_locsat_ok:
		cu_get_param_values( xmv_w, xmv_cpar );
		XtUnmanageChild( xmv_w[k_widget_locsat_box] );
		XmUpdateDisplay( xmv_appshell );
		cl3_locsat_interface( xmv_w, &xmv_par, &status );
		xmv_cpar->soft_change = TRUE;
		cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
		xmv_cpar->soft_change = FALSE;
		break;
	case k_button_locsat_hypo:
		cu_get_param_values( xmv_w, xmv_cpar );
		XtUnmanageChild( xmv_w[k_widget_locsat_box] );
		XmUpdateDisplay( xmv_appshell );
		/* set depth to free or fixed value */
		str = cu_get_string( xmv_w[k_widget_locsat_depth_text] );
		if  (strcmp(str,"free") == 0)  {
			xmv_cpar->depth_type = CUC_DEPTH_FREE;
			xmv_cpar->depth = 5.0;
		} else {
			xmv_cpar->depth_type = CUC_DEPTH_PRESET;
			sscanf( str, "%f", &(xmv_cpar->depth) );
		} /*endif*/
		/* write table prefix to filename in $SH_SCRATCH */
		str = cu_get_string( xmv_w[k_widget_locsat_prefix_text] );
		sprintf( cmd, "echo %s >$SH_SCRATCH/table_prefix.txt", str );
		system( cmd );
		cl3_call_extproc( CL3C_PLUGIN_LOCPROC, xmv_w, &xmv_dspctrl,
			&xmv_par, &xmv_phase, &status );
		xmv_cpar->soft_change = TRUE;
		cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
		xmv_cpar->soft_change = FALSE;
		break;
	case k_button_locsat_cancel:
		XtUnmanageChild( xmv_w[k_widget_locsat_box] );
		break;
	case k_button_locsat_depth_free:
		cu_set_string( xmv_w[k_widget_locsat_depth_text], "free" );
		break;
	case k_button_locsat_depth_0:
		cu_set_string( xmv_w[k_widget_locsat_depth_text], "0" );
		break;
	case k_button_locsat_depth_1:
		cu_set_string( xmv_w[k_widget_locsat_depth_text], "1" );
		break;
	case k_button_locsat_depth_10:
		cu_set_string( xmv_w[k_widget_locsat_depth_text], "10" );
		break;
	case k_button_locsat_depth_15:
		cu_set_string( xmv_w[k_widget_locsat_depth_text], "15" );
		break;
	case k_button_locsat_depth_20:
		cu_set_string( xmv_w[k_widget_locsat_depth_text], "20" );
		break;
	case k_button_locsat_depth_33:
		cu_set_string( xmv_w[k_widget_locsat_depth_text], "33" );
		break;
	case k_button_locsat_auto_yes:
	case k_button_locsat_auto_no:
		break;
	case k_button_locsat_output_yes:
	case k_button_locsat_output_no:
		break;
	case k_button_setup_hide:
		XtUnmanageChild( xmv_w[k_widget_setup_box] );
		break;
	case k_button_setup_hc_fast:
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl,
			"@SHM_CMD_SETHC sd_xwd.csh" );
		break;
	case k_button_setup_hc_qual:
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl,
			"@SHM_CMD_SETHC sd_screendump.csh" );
		break;
	case k_button_setup_maxcrsr_crosshair:
		GpSetInt( cGpI_max_cursor_form, MGC_CRSR_CROSSHAIR );
		break;
	case k_button_setup_maxcrsr_waveform:
		GpSetInt( cGpI_max_cursor_form, MGC_CRSR_WAVEFORM );
		break;
	case k_button_setup_maxcrsr_negwave:
		GpSetInt( cGpI_max_cursor_form, MGC_CRSR_WAVEFORM_NEG );
		break;
	case k_button_setup_maxcrsr_hilbert:
		GpSetInt( cGpI_max_cursor_form, MGC_CRSR_WAVEFORM_HILB );
		break;
	case k_button_setup_maxcrsr_neghilb:
		GpSetInt( cGpI_max_cursor_form, MGC_CRSR_WAVEFORM_NEGHILB );
		break;
	case k_button_setup_norm_c:
		GpSetBoolean( cGpB_auto_scaling, FALSE );
		GpSetInt( cGpI_trace_normalisation, cGp_NORM_CONST );
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "shm_cmd_norm c" );
		break;
	case k_button_setup_norm_af:
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "shm_cmd_norm af" );
		GpSetBoolean( cGpB_auto_scaling, FALSE );
		GpSetInt( cGpI_trace_normalisation, cGp_NORM_AW );
		break;
	case k_button_setup_norm_aw:
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "shm_cmd_norm aw" );
		GpSetBoolean( cGpB_auto_scaling, FALSE );
		GpSetInt( cGpI_trace_normalisation, cGp_NORM_AW );
		break;
	case k_button_setup_norm_sf:
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "shm_cmd_norm sf" );
		GpSetBoolean( cGpB_auto_scaling, TRUE );
		GpSetInt( cGpI_trace_normalisation, cGp_NORM_SW );
		break;
	case k_button_setup_norm_sw:
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "shm_cmd_norm sw" );
		GpSetBoolean( cGpB_auto_scaling, TRUE );
		GpSetInt( cGpI_trace_normalisation, cGp_NORM_SW );
		break;
	case k_button_setup_topdown_top:
		GpSetBoolean( cGpB_top_down_order, TRUE );
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "rd" );
		break;
	case k_button_setup_topdown_down:
		GpSetBoolean( cGpB_top_down_order, FALSE );
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "rd" );
		break;
	case k_button_setup_filtyp_recursive:
		GpSetChar( cGpC_filter_type, 'R' );
#ifdef XXX
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl,
			"shmsetup use_rec_filters true" );
#endif
		break;
	case k_button_setup_filtyp_fft:
		GpSetChar( cGpC_filter_type, 'F' );
#ifdef XXX
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl,
			"shmsetup use_rec_filters false" );
#endif
		break;
	case k_button_setup_deffil_none:
		GpSetString( cGpS_default_filter, "", NULL );
		break;
	case k_button_setup_deffil_standard_bp:
		GpSetString( cGpS_default_filter, "S+STANDARD_BP", NULL );
		break;
	case k_button_setup_deffil_wwssn_sp:
		GpSetString( cGpS_default_filter, "S+G_WWSSN_SP", NULL );
		break;
	case k_button_setup_deffil_but_bp:
		GpSetString( cGpS_default_filter, "BUT_BP", NULL );
		break;
	case k_button_setup_evtview_full:
		GpSetString( cGpS_evtview_proc, "", NULL );
		break;
	case k_button_setup_evtview_brief:
		GpSetString( cGpS_evtview_proc, "ev2view", NULL );
		break;
	case k_button_setup_phasename_full:
		GpSetBoolean( cGpB_full_phase_names, TRUE );
		break;
	case k_button_setup_phasename_brief:
		GpSetBoolean( cGpB_full_phase_names, FALSE );
		break;
	case k_button_refstat_ok:
		XtUnmanageChild( xmv_w[k_widget_refstat_box] );
		XmUpdateDisplay( xmv_appshell );
		GpSetString( cGpS_refstation,
			cu_get_string(xmv_w[k_widget_refstat_edit_text]), NULL );
		break;
	case k_button_refstat_station_1:
	case k_button_refstat_station_1+1:
	case k_button_refstat_station_1+2:
	case k_button_refstat_station_1+3:
	case k_button_refstat_station_1+4:
	case k_button_refstat_station_last:
		i = (*tag) - k_button_refstat_station_1;
		cl4_read_refstation( i+1, tmpstr, &status );
		if  (status == BC_NOERROR)
			cu_set_string( xmv_w[k_widget_refstat_edit_text], tmpstr );
		break;
	case k_button_phase_attrib_ext:
		mg_print_lastcmd( xmv_w[k_widget_draw], "More Attribs" );
		if  (XtIsManaged(xmv_w[k_widget_event_attrib_box]))  {
			XtUnmanageChild( xmv_w[k_widget_event_attrib_box] );
		} else {
			XtManageChild( xmv_w[k_widget_event_attrib_box] );
		} /*endif*/
		break;
	case k_button_attrib_ok:
		XtUnmanageChild( xmv_w[k_widget_event_attrib_box] );
		break;
	case k_button_evpublic_ignore:
		xmv_cpar->flags &= ~CUC_F_EVENT_TELEX_ALL;
		xmv_cpar->flags |= CUC_F_EVENT_IGNORE;
		break;
	case k_button_evpublic_telex1:
		xmv_cpar->flags &= ~CUC_F_EVENT_TELEX_ALL;
		xmv_cpar->flags &= ~CUC_F_EVENT_IGNORE;
		break;
	case k_button_evpublic_telexall:
		xmv_cpar->flags |= CUC_F_EVENT_TELEX_ALL;
		xmv_cpar->flags &= ~CUC_F_EVENT_IGNORE;
		break;
	case k_button_read_grsn_fbox:
		/* just use button state */
		break;
	case k_button_opick_ok:
		if  (XtIsManaged(xmv_w[k_widget_opick_box]))  {
			XtUnmanageChild( xmv_w[k_widget_opick_box] );
		} else {
			XtManageChild( xmv_w[k_widget_opick_box] );
		} /*endif*/
		break;
	case k_button_opick_thresh_1:
		cu_set_string( xmv_w[k_widget_opick_thresh_text], "20" );
		break;
	case k_button_opick_thresh_2:
		cu_set_string( xmv_w[k_widget_opick_thresh_text], "10" );
		break;
	case k_button_opick_thresh_3:
		cu_set_string( xmv_w[k_widget_opick_thresh_text], "5" );
		break;
	case k_button_opick_thresh_4:
		cu_set_string( xmv_w[k_widget_opick_thresh_text], "2" );
		break;
	case k_button_opick_duration_1:
		cu_set_string( xmv_w[k_widget_opick_duration_text], "0.2" );
		break;
	case k_button_opick_duration_2:
		cu_set_string( xmv_w[k_widget_opick_duration_text], "0.3" );
		break;
	case k_button_opick_duration_3:
		cu_set_string( xmv_w[k_widget_opick_duration_text], "0.5" );
		break;
	case k_button_opick_duration_4:
		cu_set_string( xmv_w[k_widget_opick_duration_text], "1.0" );
		break;
	case k_button_opick_duration_5:
		cu_set_string( xmv_w[k_widget_opick_duration_text], "2.0" );
		break;
	case k_button_opick_break_1:
		cu_set_string( xmv_w[k_widget_opick_break_text], "0.00" );
		break;
	case k_button_opick_break_2:
		cu_set_string( xmv_w[k_widget_opick_break_text], "0.05" );
		break;
	case k_button_opick_break_3:
		cu_set_string( xmv_w[k_widget_opick_break_text], "0.10" );
		break;
	case k_button_opick_break_4:
		cu_set_string( xmv_w[k_widget_opick_break_text], "0.15" );
		break;
	case k_button_spcsetup_ok:
		if  (XtIsManaged(xmv_w[k_widget_spcsetup_box]))
			XtUnmanageChild( xmv_w[k_widget_spcsetup_box] );
		spc_get_dialog_values( xmv_w, &status );
		break;
	case k_button_spcsetup_cancel:
		if  (XtIsManaged(xmv_w[k_widget_spcsetup_box]))
			XtUnmanageChild( xmv_w[k_widget_spcsetup_box] );
		break;
	case k_button_adrm_configure:
		cl4_adrm_configure( xmv_w, &status );
		break;
	case k_button_adrm_cancel:
		if  (XtIsManaged(xmv_w[k_widget_adrm_selection_box]))
			XtUnmanageChild( xmv_w[k_widget_adrm_selection_box] );
		break;
	case k_button_adrm_ok:
		if  (XtIsManaged(xmv_w[k_widget_adrm_selection_box]))
			XtUnmanageChild( xmv_w[k_widget_adrm_selection_box] );
		XmUpdateDisplay( xmv_appshell );
		cl2_display_busy( xmv_w, TRUE );
		ok = xmv_cmd_readg.keep;  /* store */
		cl4_adrm_request( xmv_w, xmv_cmd_readg.start, xmv_cmd_readg.seclth,
			tmpstr, &status );
		if  (*tmpstr != '\0' && status == cBcNoError)  {
			/* data file created, read in */
			xmh_read_gsefile( tmpstr, &status );
		} /*endif*/
		xmv_cmd_readg.keep = ok;  /* restore */
		cl2_display_busy( xmv_w, FALSE );
		break;
	case k_button_plugin_cancel:
		if  (XtIsManaged(xmv_w[k_widget_add_plugin_box]))
			XtUnmanageChild( xmv_w[k_widget_add_plugin_box] );
		break;
	case k_button_plugin_add:
		cl2_display_busy( xmv_w, TRUE );
		if  (XtIsManaged(xmv_w[k_widget_add_plugin_box]))
			XtUnmanageChild( xmv_w[k_widget_add_plugin_box] );
		cl3_add_extproc( xmv_w, &status );
		cl2_display_busy( xmv_w, FALSE );
		break;
	case k_button_param_setslow:
		PiSbSetSlowness( &(xmv_cpar->slowbox),
			cu_get_string(xmv_w[k_widget_phase_name_text]), xmv_cpar->b_slowness,
			xmv_cpar->b_azimuth, xmv_cpar->l_slowness, xmv_cpar->l_azimuth,
			&status );
		mg_print_lastcmd( xmv_w[k_widget_draw], "SetSlow" );
		printf( "*SHM: stored slo:%5.2f az:%5.1f to %s\n", xmv_cpar->b_slowness,
			xmv_cpar->b_azimuth, cu_get_string(xmv_w[k_widget_phase_name_text]) );
		break;
	default:
		printf( "*SHM: xm_call_activate: this cannot happen\n " );
	} /*endswitch*/

	mg_set_cursor( MGC_XCRSR_NORMAL );
	/* XtSetSensitive( xmv_w[k_widget_main], TRUE ); */

	if  (Severe(&status))  {
		cu_alert( status );
		/* printf( "*SHM: xm_call_activate: status %d\n", status ); */
	} /*endif*/

} /* end of xm_call_activate */



/*--------------------------------------------------------------------------*/



static void xm_call_menu_select( Widget widget, int *tag, XmAnyCallbackStruct *data )

/* callback routine of menu entries
 *
 * parameters of routine
 * Widget     widget;       input; widget ID
 * int        *tag;         input; entry number
 * XmAnyCallbackStruct *data not used
 */
{
	/* local variables */
	STATUS   locstat;              /* local status */
	char     str[BC_LINELTH+1];    /* scratch string */
	static TSyBoolean show3traces=FALSE; /* display 3 traces in zoom window */
	TSyBoolean reset_xcursor;      /* reset xcursor ? */

	/* executable code */

	xmh_log_action( "e", *tag );

	locstat = BC_NOERROR;
	mg_set_cursor( MGC_XCRSR_BUSY );
	reset_xcursor = TRUE;
	/* XtSetSensitive( xmv_w[k_widget_main], FALSE ); */

	switch  (*tag)  {
	case k_entry_command:
		HELPTEXT( "command" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Command Box" );
		if  (XtIsManaged(xmv_w[k_widget_command_box]))  {
			XtUnmanageChild( xmv_w[k_widget_command_box] );
		} else {
			XtManageChild( xmv_w[k_widget_command_box] );
		} /*endif*/
		break;
	case k_entry_help:
		mg_print_lastcmd( xmv_w[k_widget_draw], "Help Text" );
		if  (XtIsManaged(xmv_w[k_widget_help_box]))  {
			XtUnmanageChild( xmv_w[k_widget_help_box] );
			xmv_help = FALSE;
		} else {
			XtManageChild( xmv_w[k_widget_help_box] );
			xmv_help = TRUE;
			HELPTEXT( "help" );
		} /*endif*/
		break;
	case k_entry_cmd_speccmd:
		HELPTEXT( "speccmd" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Command Box" );
		if  (XtIsManaged(xmv_w[k_widget_speccmd_box]))  {
			XtUnmanageChild( xmv_w[k_widget_speccmd_box] );
		} else {
			XtManageChild( xmv_w[k_widget_speccmd_box] );
		} /*endif*/
		break;
	case k_entry_cmd_del_horiz:
		HELPTEXT( "del-horiz" );
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "del _comp~(z)" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Delete Horizontals" );
		/* set xmv_phasetrc to NULL in case this trace has been deleted */
		xmv_phasetrc = NULL;
		break;
	case k_entry_cmd_screendump:
	case k_entry_calib_screendump:
	case k_entry_spct_screendump:
	case k_entry_pm_screendump:
		HELPTEXT( "screendump" );
		cl2_display_busy( xmv_w, TRUE );
		XmUpdateDisplay( xmv_appshell );
		sprintf( str, "%s %s", GpGetString(cGpS_screendump_proc),
			cl_unique_name() );
		system( str );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Screendump" );
		cl2_display_busy( xmv_w, FALSE );
		break;
	case k_entry_cmd_userdef:
		strcpy( str, shd_inputs );
		strcat( str, "userdef.events" );
		xm_exec_userdef( str );
		break;
	case k_entry_quit:
		HELPTEXT( "quit" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Quit" );
		if  (xmv_par.state == CUC_PARSTATE_PROCESS)  {
			locstat = CUE_SAVE_FIRST;
			break;
		} /*endif*/
		if  (MrmCloseHierarchy(xmv_hier) != MrmSUCCESS)  {
			fprintf( stderr, "*SHM: Unable to close UID hierarchy.\n" );
			exit( 1 );
		} /*endif*/
		xmh_log_action( "\n--end-of-SHM\n", 0 );
		se_terminate();
		SqlDeleteScratchFile();
		exit( 0 );
	case k_entry_read_grsn:
		HELPTEXT( "read-seed" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Read" );
		if  (XtIsManaged(xmv_w[k_widget_read_grsn]))  {
			XtUnmanageChild( xmv_w[k_widget_read_grsn] );
		} else {
			XtManageChild( xmv_w[k_widget_read_grsn] );
		} /*endif*/
		break;
	case k_entry_readgse:
		HELPTEXT( "read-gse" );
		locstat = BC_NOERROR;
		cl_file_select_init( xmv_w[k_widget_filesel], CLC_PATH_GSE, &locstat );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Read GSE" );
		break;
	case k_entry_readgse2:
		HELPTEXT( "read-gse2" );
		locstat = BC_NOERROR;
		cl_file_select_init( xmv_w[k_widget_filesel], CLC_PATH_GSE2, &locstat );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Read GSE2" );
		break;
	case k_entry_readah:
		HELPTEXT( "read-ah" );
		locstat = BC_NOERROR;
		cl_file_select_init( xmv_w[k_widget_filesel], CLC_PATH_AH, &locstat );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Read AH" );
		break;
	case k_entry_readq:
		HELPTEXT( "read-q" );
		locstat = BC_NOERROR;
		cl_file_select_init( xmv_w[k_widget_filesel], CLC_PATH_Q, &locstat );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Read Q" );
		break;
	case k_entry_readrfmt:
		HELPTEXT( "read-rfmt" );
		locstat = BC_NOERROR;
		cl_file_select_init( xmv_w[k_widget_filesel], CLC_PATH_RFMT, &locstat );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Read Other" );
		break;
	case k_entry_write_mseed:
		HELPTEXT( "write-mseed" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Write MiniSEED" );
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "writeseed" );
		break;
	case k_entry_write_gse:
		HELPTEXT( "write-gse" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Write GSE1.0" );
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "writegse;;" );
		break;
	case k_entry_stw:
		HELPTEXT( "stw" );
		mx_stw( xmv_w[k_widget_draw], xmv_w[k_widget_single_draw], &xmv_dspctrl );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Set Time Wdw" );
		break;
	case k_entry_dtw:
		HELPTEXT( "dtw" );
		mg_do_drag( xmv_w[k_widget_draw], MGC_DRAG_CLEAR, 0, 0 );
		locstat = BC_NOERROR;
		mg_plot_drag_window( xmv_w[k_widget_single_draw],
			xmv_onset_acc==CUC_ACCURACY_DISPLAY, &locstat );
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "dtw" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Del Time Wdw" );
		break;
	case k_entry_wdw_move_right:
		HELPTEXT( "wdw-move-r" );
		sprintf( str, "shm_cmd_movetw %4.2f", GpGetFloat(cGpF_move_wdw_step) );
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, str );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Move Window Right" );
		mg_do_drag( xmv_w[k_widget_draw], MGC_DRAG_CLEAR, 0, 0 );
		break;
	case k_entry_wdw_move_left:
		HELPTEXT( "wdw-move-l" );
		sprintf( str, "shm_cmd_movetw %5.2f", -GpGetFloat(cGpF_move_wdw_step) );
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, str );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Move Window Left" );
		mg_do_drag( xmv_w[k_widget_draw], MGC_DRAG_CLEAR, 0, 0 );
		break;
	case k_entry_wdw_grow_right:
		HELPTEXT( "wdw-grow-r" );
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "shm_cmd_growtw right" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Grow Window Right" );
		mg_do_drag( xmv_w[k_widget_draw], MGC_DRAG_CLEAR, 0, 0 );
		break;
	case k_entry_wdw_grow_left:
		HELPTEXT( "wdw-grow-l" );
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "shm_cmd_growtw left" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Grow Window Left" );
		mg_do_drag( xmv_w[k_widget_draw], MGC_DRAG_CLEAR, 0, 0 );
		break;
	case k_entry_wdw_3traces:
		show3traces = !show3traces;
		mg_show_3_traces( show3traces );
		mg_print_lastcmd( xmv_w[k_widget_draw], "3 Zoom Traces" );
		break;
	case k_entry_parameters:
		HELPTEXT( "param-box" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Parameter Box" );
		if  (XtIsManaged(xmv_w[k_widget_param_box]))  {
			XtUnmanageChild( xmv_w[k_widget_param_box] );
		} else {
			XtManageChild( xmv_w[k_widget_param_box] );
		} /*endif*/
		break;
	case k_entry_dump_params:
		HELPTEXT( "dump-params" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Show Current Params" );
		cu_get_param_values( xmv_w, xmv_cpar );
		cl3_dump_parameters( &xmv_par, "sh_temp_par.txt", FALSE, FALSE,
			TRUE, &locstat);
		break;
	case k_entry_final_params:
		HELPTEXT( "final-params" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Show Final Params" );
		cu_get_param_values( xmv_w, xmv_cpar );
		cl3_dump_parameters( &xmv_par, "--automatic--", FALSE, FALSE,
			TRUE, &locstat );
		/* cl3_save_parameters( CUC_SAVEMODE_EVENT, xmv_cpar, &locstat ); */
		cl3_save_parameters( xmv_par.evid, xmv_cpar, &locstat );
		/* xmv_par.state = CUC_PARSTATE_FINAL; */
		xm_set_processing_state( CUC_PARSTATE_FINAL );
		cl3_call_extproc( CL3C_PLUGIN_ENDPROC, xmv_w, &xmv_dspctrl, &xmv_par,
			&xmv_phase, &locstat );
		break;
	case k_entry_cancel_params:
		HELPTEXT( "cancel-params" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Cancel Params" );
		/* xmv_par.state = CUC_PARSTATE_CANCELLED; */
		xm_set_processing_state( CUC_PARSTATE_FINAL );
		break;
	case k_entry_short_info:
		mg_print_lastcmd( xmv_w[k_widget_draw], "Short Info" );
		cu_print_param_values( xmv_cpar );
		break;
	case k_entry_trc_delete:
		HELPTEXT( "trc-delete" );
		/* mx_trclist_command( xmv_w[k_widget_draw], xmv_w[k_widget_single_draw],
			&xmv_dspctrl, "del %s", &locstat ); */
		cl4_delete_traces();
		cl_set_simfilter_active( xmv_w, TRUE );
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "rd" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Delete Traces" );
		/* set xmv_phasetrc to NULL in case this trace has been deleted */
		xmv_phasetrc = NULL;
		break;
	case k_entry_trc_del_badqual:
		HELPTEXT( "trc-dec-badqual" );
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "del _qual(y)" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Delete Bad Qual" );
		xmv_phasetrc = NULL;
		break;
	case k_entry_trc_keep_r:
		HELPTEXT( "trc-keep-R" );
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "del _comp(z)" );
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "del _comp(t)" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Keep R" );
		/* set xmv_phasetrc to NULL in case this trace has been deleted */
		xmv_phasetrc = NULL;
		break;
	case k_entry_trc_keep_t:
		HELPTEXT( "trc-keep-T" );
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "del _comp(z)" );
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "del _comp(r)" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Keep T" );
		/* set xmv_phasetrc to NULL in case this trace has been deleted */
		xmv_phasetrc = NULL;
		break;
	case k_entry_trc_demean:
		HELPTEXT( "trc-demean" );
		mx_trclist_command( xmv_w[k_widget_draw], xmv_w[k_widget_single_draw],
			&xmv_dspctrl, "DEMEAN %s", &locstat );
		cl_set_simfilter_active( xmv_w, TRUE );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Demean Traces" );
		break;
	case k_entry_trc_despike:
		HELPTEXT( "trc-despike" );
		mx_trclist_command( xmv_w[k_widget_draw], xmv_w[k_widget_single_draw],
			&xmv_dspctrl, "despike %s 50.0", &locstat );
		cl_set_simfilter_active( xmv_w, TRUE );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Despike Traces" );
		break;
	case k_entry_trc_trend:
		HELPTEXT( "trc-trend" );
		mx_trclist_command( xmv_w[k_widget_draw], xmv_w[k_widget_single_draw],
			&xmv_dspctrl, "trend %s", &locstat );
		cl_set_simfilter_active( xmv_w, TRUE );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Remove Trend" );
		break;
	case k_entry_trc_refml:
		HELPTEXT( "trc-refml" );
		mx_trclist_refml( &locstat );
		mg_clear_selections( xmv_w[k_widget_draw] );
		cl_set_simfilter_active( xmv_w, TRUE );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Ref ml" );
		break;
	case k_entry_trc_hide:
		HELPTEXT( "trc-hide" );
		mx_trclist_command( xmv_w[k_widget_draw], xmv_w[k_widget_single_draw],
			&xmv_dspctrl, "SHM_CMD_HIDE %s", &locstat );
		cl_set_simfilter_active( xmv_w, TRUE );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Hide Traces" );
		break;
	case k_entry_trc_invhide:
		HELPTEXT( "trc-invhide" );
		mx_trclist_command( xmv_w[k_widget_draw], xmv_w[k_widget_single_draw],
			&xmv_dspctrl, "SHM_CMD_HIDE %s inverted", &locstat );
		cl_set_simfilter_active( xmv_w, TRUE );
		mg_print_lastcmd( xmv_w[k_widget_draw], "InvHide Traces" );
		break;
	case k_entry_trc_sort_d:
		HELPTEXT( "sort-d" );
		if  (*xmv_sortstr == '\0')  {
			strcpy( xmv_sortstr, "DISTANCE" );
			strcpy( xmv_sortinfo, "sort:d" );
		} else {
			*xmv_sortstr = '\0';
			*xmv_sortinfo = '\0';
			break;
		} /*endif*/
		cu_get_param_values( xmv_w, xmv_cpar );
		cl4_sort_traces( xmv_w[k_widget_draw], &xmv_dspctrl, xmv_sortstr,
			xmv_cpar, &locstat );
		mg_print_sortinfo( xmv_w[k_widget_draw], xmv_sortinfo );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Sort Dist" );
		break;
	case k_entry_trc_sort_a:
		HELPTEXT( "sort-a" );
		if  (*xmv_sortstr == '\0')  {
			strcpy( xmv_sortstr, "AZIMUTH" );
			strcpy( xmv_sortinfo, "sort:a" );
		} else {
			*xmv_sortstr = '\0';
			*xmv_sortinfo = '\0';
			break;
		} /*endif*/
		cu_get_param_values( xmv_w, xmv_cpar );
		cl4_sort_traces( xmv_w[k_widget_draw], &xmv_dspctrl, xmv_sortstr,
			xmv_cpar, &locstat );
		mg_print_sortinfo( xmv_w[k_widget_draw], xmv_sortinfo );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Sort Azim" );
		break;
	case k_entry_filter:
		HELPTEXT( "filter" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Select Filter" );
		if  (XtIsManaged(xmv_w[k_widget_filter_box]))  {
			XtUnmanageChild( xmv_w[k_widget_filter_box] );
		} else {
			XtManageChild( xmv_w[k_widget_filter_box] );
		} /*endif*/
		break;
	case k_entry_rotate:
		HELPTEXT( "rotate" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Rotate" );
		cl2_display_busy( xmv_w, TRUE );
		cu_get_param_values( xmv_w, xmv_cpar );
		mx_rotation( xmv_w[k_widget_draw], &xmv_dspctrl, xmv_cpar->b_azimuth,
			xmv_cpar->source_lat, xmv_cpar->source_lon, &locstat );
		cl2_display_busy( xmv_w, FALSE );
		break;
	case k_entry_amplper_p:
		HELPTEXT( "amplper-p" );
		xmv_select.waiting = 2;
		xmv_select.processed = 0;
		xmv_select.initiator = CUC_SELTYPE_AMPLPER_P;
		strcpy( xmv_select.select[0].name, "w-start" );
		strcpy( xmv_select.select[1].name, "w-end" );
		strcpy( xmv_select.infotext, CUC_SELTEXT_AMPLPER_P );
		mg_print_status( xmv_w[k_widget_draw], CUC_SELTEXT_AMPLPER_P, TRUE);
		mg_print_lastcmd( xmv_w[k_widget_draw], "A&P peak-peak" );
		mg_set_cursor( MGC_XCRSR_RIGHT );
		reset_xcursor = FALSE;
		break;
	case k_entry_amplper_z:
		HELPTEXT( "amplper-z" );
		xmv_select.waiting = 2;
		xmv_select.processed = 0;
		xmv_select.initiator = CUC_SELTYPE_AMPLPER_Z;
		strcpy( xmv_select.select[0].name, "w-start" );
		strcpy( xmv_select.select[1].name, "w-end" );
		strcpy( xmv_select.infotext, CUC_SELTEXT_AMPLPER_Z );
		mg_print_status( xmv_w[k_widget_draw], CUC_SELTEXT_AMPLPER_Z, TRUE);
		mg_print_lastcmd( xmv_w[k_widget_draw], "A&P zero-peak" );
		mg_set_cursor( MGC_XCRSR_RIGHT );
		reset_xcursor = FALSE;
		break;
	case k_entry_ampl_man:
		HELPTEXT( "ampl-man" );
		xmv_select.waiting = 2;
		xmv_select.processed = 0;
		xmv_select.initiator = CUC_SELTYPE_AMPL_MAN;
		strcpy( xmv_select.select[0].name, "w-start" );
		strcpy( xmv_select.select[1].name, "w-end" );
		strcpy( xmv_select.infotext, CUC_SELTEXT_AMPL_MAN );
		mg_print_status( xmv_w[k_widget_draw], CUC_SELTEXT_AMPL_MAN, TRUE);
		mg_print_lastcmd( xmv_w[k_widget_draw], "Ampl. manually" );
		mg_set_cursor( MGC_XCRSR_RIGHT );
		reset_xcursor = FALSE;
		break;
	case k_entry_per_man:
		HELPTEXT( "period-man" );
		xmv_select.waiting = 2;
		xmv_select.processed = 0;
		xmv_select.initiator = CUC_SELTYPE_PER_MAN;
		strcpy( xmv_select.select[0].name, "w-start" );
		strcpy( xmv_select.select[1].name, "w-end" );
		strcpy( xmv_select.infotext, CUC_SELTEXT_PER_MAN );
		mg_print_status( xmv_w[k_widget_draw], CUC_SELTEXT_PER_MAN, TRUE);
		mg_print_lastcmd( xmv_w[k_widget_draw], "Period manually" );
		mg_set_cursor( MGC_XCRSR_RIGHT );
		reset_xcursor = FALSE;
		break;
	case k_entry_ampl_surface:
		HELPTEXT( "ampl-surface" );
		xmv_select.waiting = 2;
		xmv_select.processed = 0;
		xmv_select.initiator = CUC_SELTYPE_AMPL_SURFACE;
		strcpy( xmv_select.select[0].name, "w-start" );
		strcpy( xmv_select.select[1].name, "w-end" );
		strcpy( xmv_select.infotext, CUC_SELTEXT_AMPL_SURFACE );
		mg_print_status( xmv_w[k_widget_draw], CUC_SELTEXT_AMPL_SURFACE, TRUE);
		mg_print_lastcmd( xmv_w[k_widget_draw], "Surface Ampl. auto" );
		mg_set_cursor( MGC_XCRSR_RIGHT );
		reset_xcursor = FALSE;
		break;
	case k_entry_abort_selection:
		HELPTEXT( "abort-selection" );
		xmv_select.waiting = 0;
		xmv_select.processed = 0;
		xmv_select.initiator = CUC_SELTYPE_NONE;
		mg_print_status( xmv_w[k_widget_draw], xmv_select.infotext, FALSE );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Abort Selection" );
		break;
	case k_entry_calib_accept:
		HELPTEXT( "calib-accept" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Accept Calibration" );
		cal_accept_values( xmv_w[k_widget_calibwdw], xmv_cpar );
		break;
	case k_entry_calib_quit:
		HELPTEXT( "calib-quit" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Quit Calibration" );
		cal_accept_values( xmv_w[k_widget_calibwdw], NULL );
		break;
	case k_entry_flags_setcalib:
		mg_print_lastcmd( xmv_w[k_widget_draw], "Set Calib Flag" );
		xmv_cpar->flags |= CUC_F_EVENT_CALIB;
		break;
	case k_entry_flags_unsetcalib:
		mg_print_lastcmd( xmv_w[k_widget_draw], "Clear Calib Flag" );
		xmv_cpar->flags &= ~CUC_F_EVENT_CALIB;
		break;
	case k_entry_magn_mb:
		HELPTEXT( "magn-mb" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Magnitude mb" );
		cu_get_param_values( xmv_w, xmv_cpar );
		cl2_magnitude( SIC_MAGN_MB, xmv_cpar, &locstat );
		break;
	case k_entry_magn_ms_plain:
		HELPTEXT( "magn-ms-plain" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Magnitude MS (plain)" );
		cu_get_param_values( xmv_w, xmv_cpar );
		cl2_magnitude( SIC_MAGN_MS_PLAIN, xmv_cpar, &locstat );
		break;
	case k_entry_magn_ms_c_na:
		HELPTEXT( "magn-ms-c-na" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Magnitude MS c-na" );
		cu_get_param_values( xmv_w, xmv_cpar );
		cl2_magnitude( SIC_MAGN_MS_C_NA, xmv_cpar, &locstat );
		break;
	case k_entry_magn_ms_c_eu:
		HELPTEXT( "magn-ms-c-eu" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Magnitude MS c-eu" );
		cu_get_param_values( xmv_w, xmv_cpar );
		cl2_magnitude( SIC_MAGN_MS_C_EU, xmv_cpar, &locstat );
		break;
	case k_entry_magn_ms_c_o:
		HELPTEXT( "magn-ms-c-o" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Magnitude MS c-o" );
		cu_get_param_values( xmv_w, xmv_cpar );
		cl2_magnitude( SIC_MAGN_MS_C_O, xmv_cpar, &locstat );
		break;
	case k_entry_magn_ms_o:
		HELPTEXT( "magn-ms-o" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Magnitude MS o" );
		cu_get_param_values( xmv_w, xmv_cpar );
		cl2_magnitude( SIC_MAGN_MS_O, xmv_cpar, &locstat );
		break;
	case k_entry_magn_ml:
		HELPTEXT( "magn-ml" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Magnitude ml" );
		if  (strcmp(xmv_cmd_filter.name,"S+WOODAND") != 0)  {
			/* locstat = CUE_NO_WOODAND; */
			/* break; */
			/* make it instead of complaining */
			int     item;      /* button to emulate */
			XmToggleButtonCallbackStruct data;
			item = k_button_filter_woodand;
			data.set = TRUE;
			xm_call_activate( (Widget)0, &item, &data );
			item = k_button_filter_ok;
			xm_call_activate( (Widget)0, &item, NULL );
		} /*endif*/
		xmv_select.waiting = 2;
		xmv_select.processed = 0;
		xmv_select.initiator = CUC_SELTYPE_MAGN_ML;
		strcpy( xmv_select.select[0].name, "w-start-ml" );
		strcpy( xmv_select.select[1].name, "w-end-ml" );
		strcpy( xmv_select.infotext, CUC_SELTEXT_MAGN_ML );
		mg_print_status( xmv_w[k_widget_draw], CUC_SELTEXT_MAGN_ML, TRUE);
		cu_get_param_values( xmv_w, xmv_cpar );
		mg_set_cursor( MGC_XCRSR_RIGHT );
		reset_xcursor = FALSE;
		break;
	case k_entry_magn_del_ml:
		mg_print_lastcmd( xmv_w[k_widget_draw], "delete ml" );
		cl4_del_magnitude( xmv_w[k_widget_draw], cPiMagnMl );
		cl_set_simfilter_active( xmv_w, TRUE );
		break;
	case k_entry_gencomment:
		HELPTEXT( "comment" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Enter Comment" );
		cl2_modify_comment( &(xmv_cpar->comment), &locstat );
		break;
	case k_entry_info_source:
		HELPTEXT( "info-source" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Info Source" );
		if  (XtIsManaged(xmv_w[k_widget_infsource_box]))  {
			XtUnmanageChild( xmv_w[k_widget_infsource_box] );
		} else {
			XtManageChild( xmv_w[k_widget_infsource_box] );
		} /*endif*/
		break;
	case k_entry_params_save:
		HELPTEXT( "save-params" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Save Status" );
		cu_get_param_values( xmv_w, xmv_cpar );
		cl3_save_parameters( CUC_SAVEMODE_A, xmv_cpar, &locstat );
		break;
	case k_entry_params_restore:
		HELPTEXT( "params-restore" );
		cl3_restore_parameters( "a", xmv_cpar, &locstat );
		xmv_cpar->soft_change = TRUE;
		cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
		xmv_cpar->soft_change = FALSE;
		mg_tracedisplay( xmv_w[k_widget_draw], &xmv_dspctrl, &locstat );
		/* mg_plot_phases( xmv_w[k_widget_draw], &locstat ); */
		mg_print_lastcmd( xmv_w[k_widget_draw], "Restore Status" );
		break;
	case k_entry_params_recover:
		HELPTEXT( "params-recover" );
		cl_file_select_init( xmv_w[k_widget_filesel], CLC_PATH_SAVE, &locstat );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Recover Status" );
		break;
	case k_entry_params_evt:
		HELPTEXT( "params-evt" );
		cl_file_select_init( xmv_w[k_widget_filesel], CLC_PATH_EVT, &locstat );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Recover Evt-File" );
		break;
	case k_entry_locsat:
		HELPTEXT( "locsat" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "LocSAT" );
		if  (XtIsManaged(xmv_w[k_widget_locsat_box]))  {
			XtUnmanageChild( xmv_w[k_widget_locsat_box] );
		} else {
			XtManageChild( xmv_w[k_widget_locsat_box] );
		} /*endif*/
		break;
	case k_entry_opick_setup:
		HELPTEXT( "opick_setup" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "OnsetPick Setup" );
		if  (XtIsManaged(xmv_w[k_widget_opick_box]))  {
			XtUnmanageChild( xmv_w[k_widget_opick_box] );
		} else {
			XtManageChild( xmv_w[k_widget_opick_box] );
		} /*endif*/
		break;
	case k_entry_ext_location:
		{ float xslo;   /* dummy value */
		HELPTEXT( "ext-location" );
		cu_get_param_values( xmv_w, xmv_cpar );
		si_ext_location( xmv_cpar->source_lat, xmv_cpar->source_lon,
			xmv_cpar->depth, GpGetString(cGpS_refstation), &(xmv_cpar->distance),
			&(xmv_cpar->b_azimuth), &xslo, &locstat );
		xmv_cpar->dist_unit = CUC_DIST_UNIT_DEG;
		/* xmv_cpar->b_slowness_err = 0.0; */
		xmv_cpar->b_azimuth_err = 0.0;
		xmv_cpar->soft_change = TRUE;
		cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
		xmv_cpar->soft_change = FALSE;
		mg_print_lastcmd( xmv_w[k_widget_draw], "External Loc" );
		break;
		}
	case k_entry_match_location:
		{ char station[cBcShortStrLth+1];  /* station name */
		  char agency[cBcShortStrLth+1];   /* lookup agency name */
		  char locphase[cPiMaxPhaseLth+1]; /* local phase name */
		  float xslo;                      /* dummy value */
		HELPTEXT( "match-location" );
		if  (xmv_phasetrc == NULL)  {
			*station = '\0';
			locstat = CUE_TOUCH_PHASE;
			break;
		} else {
			locstat = cBcNoError;
			db_gets( xmv_phasetrc, ES_STATION, cBcShortStrLth, station, &locstat );
			if  (locstat != cBcNoError)  {
				locstat = CUE_TOUCH_PHASE;
				break;
			} /*endif*/
		} /*endif*/
		cl2_display_busy( xmv_w, TRUE );
		if  (*station == '\0')  strcpy( station, GpGetString(cGpS_refstation) );
		strcpy( locphase, xmv_phase.name );
		if  (strcmp(locphase,"L") == 0)  strcpy( locphase, "LR" );
		si_lookup_agency( xmv_phase.onset, agency );
		si_match_location( agency, locphase, xmv_phase.onset,
			station, xmv_cpar->b_slowness, xmv_cpar->b_azimuth, TRUE,
			&(xmv_cpar->source_lat), &(xmv_cpar->source_lon),
			&(xmv_cpar->depth), xmv_cpar->origin, xmv_cpar->source, &locstat );
		if  (locstat == cBcNoError)  {
			/*strcpy( xmv_cpar->source, "neic-a" );*/
			si_ext_location( xmv_cpar->source_lat, xmv_cpar->source_lon,
				xmv_cpar->depth, GpGetString(cGpS_refstation),
				&(xmv_cpar->distance), &(xmv_cpar->b_azimuth), &xslo, &locstat );
			xmv_cpar->dist_unit = CUC_DIST_UNIT_DEG;
			mb_ferindex( xmv_cpar->source_lat, xmv_cpar->source_lon,
				&(xmv_cpar->reg_id), &locstat );
			/*cl4_loc_from_dist( xmv_cpar, &locstat );*/
			xmv_cpar->soft_change = TRUE;
			cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
			xmv_cpar->soft_change = FALSE;
		} /*endif*/
		cl2_display_busy( xmv_w, FALSE );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Match Location" );
		break;
		}
	case k_entry_identify_phase:
		{ char station[cBcShortStrLth+1];  /* station name */
		  float xslo;                      /* dummy value */
		  char idphase[cBcShortStrLth+1];  /* phase found */
		  int cnt;                         /* phase rename counter */
		HELPTEXT( "identify-phase" );
		if  (xmv_phasetrc == NULL)  {
			*station = '\0';
			locstat = CUE_TOUCH_PHASE;
			break;
		} else {
			db_gets( xmv_phasetrc, ES_STATION, cBcShortStrLth, station, NULL );
		} /*endif*/
		cl2_display_busy( xmv_w, TRUE );
		if  (*station == '\0')  strcpy( station, GpGetString(cGpS_refstation) );
		si_identify_phase( xmv_phase.onset,
			station, xmv_cpar->b_slowness, xmv_cpar->b_azimuth, idphase,
			&(xmv_cpar->source_lat), &(xmv_cpar->source_lon),
			&(xmv_cpar->depth), xmv_cpar->origin, xmv_cpar->source, &locstat );
		if  (locstat == cBcNoError)  {
			xmv_cpar->depth_type = CUC_DEPTH_RELIABLE;
			if  (strcmp(xmv_phase.name,idphase) != 0)  {
				mg_plot_phases( xmv_w[k_widget_draw], xmv_dspctrl.show_phase_acc,
					&locstat );
				PiRenamePhase( xmv_phase.name, idphase, &cnt );
				mg_plot_phases( xmv_w[k_widget_draw], xmv_dspctrl.show_phase_acc,
					&locstat );
				strcpy( xmv_phase.name, idphase );
			} /*endif*/
			si_ext_location( xmv_cpar->source_lat, xmv_cpar->source_lon,
				xmv_cpar->depth, GpGetString(cGpS_refstation),
				&(xmv_cpar->distance), &(xmv_cpar->b_azimuth), &xslo, &locstat );
			xmv_cpar->dist_unit = CUC_DIST_UNIT_DEG;
			/* cl4_loc_from_dist( xmv_cpar, &locstat ); */
			mb_ferindex( xmv_cpar->source_lat, xmv_cpar->source_lon,
				&(xmv_cpar->reg_id), &locstat );
			mb_fername( xmv_cpar->reg_id, BC_LINELTH, xmv_cpar->regname, &locstat );
			xmv_cpar->soft_change = TRUE;
			cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
			xmv_cpar->soft_change = FALSE;
		} /*endif*/
		cl2_display_busy( xmv_w, FALSE );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Identify Phase" );
		break;
		}
	case k_entry_source_region:
		HELPTEXT( "source-region" );
		cu_get_param_values( xmv_w, xmv_cpar );
#		ifdef XXX
		if  (xmv_cpar->dist_unit != CUC_DIST_UNIT_DEG)  {
			if  (GpGetInt(cGpI_debug_level) > 2)
				printf( "SHM-dbg3: get_location: distance unit is km\n" );
			break;
		} /*endif*/
		si_get_location( xmv_cpar->distance, xmv_cpar->b_azimuth,
			GpGetString(cGpS_refstation), &(xmv_cpar->source_lat),
			&(xmv_cpar->source_lon), &(xmv_cpar->reg_id),
			xmv_cpar->regname, &locstat );
		xmv_cpar->table_number = CUC_REGTABLE_FLINNENG;
#		endif
		cl4_loc_from_dist( xmv_cpar, &locstat );
		xmv_cpar->soft_change = TRUE;
		cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
		xmv_cpar->soft_change = FALSE;
		mg_print_lastcmd( xmv_w[k_widget_draw], "Src Region" );
		break;
	case k_entry_phase_difference:
		HELPTEXT( "phase-diff" );
		cl4_phase_difference( GpGetString(cGpS_diff_phase_list), xmv_cpar->depth,
			&(xmv_cpar->distance), &locstat );
		xmv_cpar->dist_unit = CUC_DIST_UNIT_DEG;
		xmv_cpar->soft_change = TRUE;
		cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
		xmv_cpar->soft_change = FALSE;
		mg_print_lastcmd( xmv_w[k_widget_draw], "Phase Diff" );
		break;
	case k_entry_deltheo:
		HELPTEXT( "del-theo" );
		/* delete all theoretical phases from all traces */
		cu_delete_all_phases( xmv_w[k_widget_draw],
			xmv_w[k_widget_single_draw], "--theo--", TRUE );
		break;
	case k_entry_fk:
		HELPTEXT( "fk" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "FK" );
		if  (XtIsManaged(xmv_w[k_widget_fk_box]))  {
			XtUnmanageChild( xmv_w[k_widget_fk_box] );
		} else {
			cl3_preset_fk_values( xmv_w, xmv_cmd_filter.name );
			XtManageChild( xmv_w[k_widget_fk_box] );
		} /*endif*/
		break;
	case k_entry_vespa:
		HELPTEXT( "vespa" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Vespa" );
		XtSetSensitive( xmv_w[k_widget_vespa_export_button], FALSE );
		if  (XtIsManaged(xmv_w[k_widget_vespa_input_box]))  {
			XtUnmanageChild( xmv_w[k_widget_vespa_input_box] );
		} else {
			XtManageChild( xmv_w[k_widget_vespa_input_box] );
		} /*endif*/
		break;
	case k_entry_setup:
		HELPTEXT( "setup-box" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Setup Box" );
		if  (XtIsManaged(xmv_w[k_widget_setup_box]))  {
			XtUnmanageChild( xmv_w[k_widget_setup_box] );
		} else {
			XtManageChild( xmv_w[k_widget_setup_box] );
		} /*endif*/
		/* it's not a very good idea to put this here: */
		mg_init_tracedisplay( xmv_w[k_widget_draw], -1, -1,
			GpGetInt(cGpI_top_margin), -1 );
		break;
	case k_entry_configure:
		HELPTEXT( "configure" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Configure" );
		xm_call_configure();
		break;
	case k_entry_refstation:
		HELPTEXT( "refstation" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Ref. Station" );
		if  (XtIsManaged(xmv_w[k_widget_refstat_box]))  {
			XtUnmanageChild( xmv_w[k_widget_refstat_box] );
		} else {
			XtManageChild( xmv_w[k_widget_refstat_box] );
		} /*endif*/
		break;
	case k_entry_mparam_sn_auto:
		HELPTEXT( "signoise-auto" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "S/N auto" );
		cl4_signoise_set_windows( xmv_w,
			cu_get_string(xmv_w[k_widget_phase_name_text]),
			xmv_cmd_filter.name, &locstat );
		if  (SySevere(&locstat))  break;
		cl4_signoise_values( cu_get_string(xmv_w[k_widget_phase_name_text]),
			&locstat );
		break;
	case k_entry_mparam_sn_repeat:
		HELPTEXT( "signoise-repeat" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "S/N repeat" );
		cl4_signoise_values( cu_get_string(xmv_w[k_widget_phase_name_text]),
			&locstat );
		break;
	case k_entry_mparam_sn_delete:
		HELPTEXT( "signoise-delete" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "S/N delete" );
		cl4_delete_signoise_values(
			cu_get_string(xmv_w[k_widget_phase_name_text]), &locstat );
		cu_delete_all_phases( xmv_w[k_widget_draw], xmv_w[k_widget_single_draw],
			"--pseudo--", TRUE );
		break;
	case k_entry_mparam_sn_clear:
		HELPTEXT( "delete-pseudos" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "del pseudos" );
		cu_delete_all_phases( xmv_w[k_widget_draw], xmv_w[k_widget_single_draw],
			"--pseudo--", TRUE );
		break;
	case k_entry_spectrum:
		HELPTEXT( "spectrum" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Spectrum" );
		spc_initialize( xmv_w[k_widget_spectrum_box],
			xmv_w[k_widget_spctwdw], &locstat );
		spc_insert_spectrum( xmv_w[k_widget_spctwdw], &locstat );
		break;
	case k_entry_spectrogram:
		HELPTEXT( "spectrogram" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Spectrogram" );
		spc_spectrogram( &locstat );
		break;
	case k_entry_spct_insert:
		HELPTEXT( "spectrum-insert" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Insert Spectrum" );
		spc_insert_spectrum( xmv_w[k_widget_spctwdw], &locstat );
		break;
	case k_entry_spct_fitline:
		HELPTEXT( "spectrum-fitline" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Fit Line" );
		spc_start_fitline();
		break;
	case k_entry_spct_quit:
		HELPTEXT( "spectrum-quit" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Quit Spectrum" );
		spc_close_window();
		break;
	case k_entry_spct_autoscale:
		HELPTEXT( "spectrum-autoscale" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Spc Lo-Ampl Up" );
		spc_change_display( cSpcAutoscale, &locstat );
		break;
	case k_entry_spct_amplo_up:
		HELPTEXT( "spectrum-amplo_up" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Spc Lo-Ampl Up" );
		spc_change_display( cSpcAmpLoUp, &locstat );
		break;
	case k_entry_spct_amplo_down:
		HELPTEXT( "spectrum-amplo_down" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Spc Lo-Ampl Down" );
		spc_change_display( cSpcAmpLoDown, &locstat );
		break;
	case k_entry_spct_amphi_up:
		HELPTEXT( "spectrum-amphi_up" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Spc Hi-Ampl Up" );
		spc_change_display( cSpcAmpHiUp, &locstat );
		break;
	case k_entry_spct_amphi_down:
		HELPTEXT( "spectrum-amphi_down" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Spc Hi-Ampl Down" );
		spc_change_display( cSpcAmpHiDown, &locstat );
		break;
	case k_entry_spct_frqlo_up:
		HELPTEXT( "spectrum-frqlo_up" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Spc Lo-Freq Up" );
		spc_change_display( cSpcFrqLoUp, &locstat );
		break;
	case k_entry_spct_frqlo_down:
		HELPTEXT( "spectrum-frqlo_down" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Spc Lo-Freq Down" );
		spc_change_display( cSpcFrqLoDown, &locstat );
		break;
	case k_entry_spct_frqhi_up:
		HELPTEXT( "spectrum-frqhi_up" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Spc Hi-Freq Up" );
		spc_change_display( cSpcFrqHiUp, &locstat );
		break;
	case k_entry_spct_frqhi_down:
		HELPTEXT( "spectrum-frqhi_down" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Spc Hi-Freq Down" );
		spc_change_display( cSpcFrqHiDown, &locstat );
		break;
	case k_entry_spct_mode_powspc:
		HELPTEXT( "spectrum-mode_powspc" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Spc Mode PowSpec" );
		spc_recompute_spectra( cSpcModePowSpc, &locstat );
		break;
	case k_entry_spct_mode_fft:
		HELPTEXT( "spectrum-mode_fft" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Spc Mode FFT" );
		spc_recompute_spectra( cSpcModeFft, &locstat );
		break;
	case k_entry_spct_mode_fft2:
		HELPTEXT( "spectrum-mode_fft2" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Spc Mode FFT^2" );
		spc_recompute_spectra( cSpcModeFft2, &locstat );
		break;
	case k_entry_spct_clear:
		HELPTEXT( "spectrum-clear" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Spc Clear" );
		break;
	case k_entry_spct_setup:
		HELPTEXT( "spectrum-setup" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Spc Setup" );
		spc_set_dialog_values( xmv_w );
		if  (XtIsManaged(xmv_w[k_widget_spcsetup_box]))  {
			XtUnmanageChild( xmv_w[k_widget_spcsetup_box] );
		} else {
			XtManageChild( xmv_w[k_widget_spcsetup_box] );
		} /*endif*/
		break;
	case k_entry_pmotion:
		HELPTEXT( "pmotion" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Part.Motion" );
		pmm_initialize( xmv_w[k_widget_pmotion_box],
			xmv_w[k_widget_pmwdw], &locstat );
		xmv_pm_on = TRUE;
		break;
	case k_entry_pm_quit:
		HELPTEXT( "pm-quit" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Quit Part.Motion" );
		pmm_close_window();
		xmv_pm_on = FALSE;
		break;
	case k_entry_pm_lincol:
		HELPTEXT( "pm-line-col" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "PM Line Col." );
		pmm_set_mode( cPmmModeLineColored );
		break;
	case k_entry_pm_linmono:
		HELPTEXT( "pm-line-mono" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "PM Line Mono." );
		pmm_set_mode( cPmmModeLineMono );
		break;
	case k_entry_pm_plot1:
		HELPTEXT( "pm-plot1" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "PM Plot NE" );
		pmm_set_mode( cPmmModePlot1 );
		break;
	case k_entry_pm_plot3:
		HELPTEXT( "pm-plot3" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "PM Plot 3" );
		pmm_set_mode( cPmmModePlot3 );
		break;
	case k_entry_pm_zoom_on:
		HELPTEXT( "pm-zoom-on" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "PM Zoom On" );
		pmm_set_mode( cPmmModeZoomOn );
		break;
	case k_entry_pm_zoom_off:
		HELPTEXT( "pm-zoom-off" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "PM Zoom Off" );
		pmm_set_mode( cPmmModeZoomOff );
		break;
	case k_entry_polarfil:
		HELPTEXT( "polarfil" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Polar.Filter" );
		pol_initialize( xmv_w[k_widget_polarfil_box],
			xmv_w[k_widget_polwdw], &locstat );
		xmv_polar_on = TRUE;
		break;
	case k_entry_pol_quit:
		HELPTEXT( "pol-quit" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Quit Polar.Filter" );
		pol_close_window();
		xmv_polar_on = FALSE;
		break;
	case k_entry_pol_inc_cohlth:
		pol_incpar( POL_ID_COHLTH, 1 );
		break;
	case k_entry_pol_dec_cohlth:
		pol_incpar( POL_ID_COHLTH, -1 );
		break;
	case k_entry_pol_inc_pow_linfil:
		pol_incpar( POL_ID_POW_LINFIL, 1 );
		break;
	case k_entry_pol_dec_pow_linfil:
		pol_incpar( POL_ID_POW_LINFIL, -1 );
		break;
	case k_entry_calib:
		cal_initialize( xmv_w[k_widget_calibration_box],
			xmv_w[k_widget_calibwdw], &locstat );
		if  (Severe(&locstat))  break;
		cal_display_calib( xmv_w[k_widget_calibwdw], xmv_cpar, &locstat );
		break;
	case k_entry_planewave:
		mg_print_lastcmd( xmv_w[k_widget_draw], "Plane Wave" );
		cl2_display_busy( xmv_w, TRUE );
		cu_get_param_values( xmv_w, xmv_cpar );
		cu_localization(
			cu_get_string(xmv_w[k_widget_phase_name_text]),
			CUF_DOLOC_SLOAZ, xmv_cpar, &locstat );
		xmv_cpar->soft_change = TRUE;
		cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
		xmv_cpar->soft_change = FALSE;
		cu_print_param_values( xmv_cpar );
		cl2_display_busy( xmv_w, FALSE );
		break;
	case k_entry_align:
		mx_handle_beam( xmv_w[k_widget_draw], xmv_w[k_widget_single_draw],
			FALSE, &xmv_dspctrl, xmv_cpar,
			cu_get_string(xmv_w[k_widget_phase_name_text]), &locstat );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Align" );
		break;
	case k_entry_beam:
		cu_get_param_values( xmv_w, xmv_cpar );
		mx_handle_beam( xmv_w[k_widget_draw], xmv_w[k_widget_single_draw],
			TRUE, &xmv_dspctrl, xmv_cpar, "", &locstat );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Beam" );
		break;
	case k_entry_fixbeam:
		cl4_fix_beam( xmv_w, &xmv_dspctrl, xmv_cmd_filter.name, &locstat );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Fix Beam" );
		xmv_cmd_readg.keep = TRUE;
		xmv_keeptrc_auto = TRUE;
		XmToggleButtonSetState( xmv_w[k_widget_read_grsn_keep], TRUE, FALSE );
		break;
	case k_entry_autopick1:
		cl_auto_beam_pick( xmv_w[k_widget_draw], xmv_w[k_widget_single_draw],
			GpGetString(cGpS_auto_phase), TRUE, &locstat );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Min/Max Pick" );
		break;
	case k_entry_corrpick:
		cl_correlation_pick( xmv_w[k_widget_draw], xmv_w[k_widget_single_draw],
			cu_get_string(xmv_w[k_widget_phase_name_text]), &locstat );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Correlation Pick" );
		break;
	case k_entry_onsetpick:
		cl4_onset_pick( xmv_w, &xmv_dspctrl,
			cu_get_string(xmv_w[k_widget_phase_name_text]), TRUE, &locstat );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Onset Pick" );
		break;
	case k_entry_residcorr:
		cl4_resid_corr( TRUE, xmv_cpar->b_slowness, xmv_cpar->b_azimuth,
			&locstat );
		cu_set_exec_flag( CUC_F_EXEC_RESIDCORR, TRUE );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Residual Correction" );
		break;
	case k_entry_residdel:
		cl4_resid_corr( FALSE, 0.0, 0.0, &locstat );
		cu_set_exec_flag( CUC_F_EXEC_RESIDCORR, FALSE );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Residual Delete" );
		break;
	case k_entry_eproc1:
	case k_entry_eproc2:
	case k_entry_eproc3:
	case k_entry_eproc4:
	case k_entry_eproc5:
	case k_entry_eproc6:
	case k_entry_eproc7:
	case k_entry_eproc8:
	case k_entry_eproc9:
	case k_entry_eproc10:
	case k_entry_eproc11:
	case k_entry_eproc12:
	case k_entry_eproc13:
	case k_entry_eproc14:
	case k_entry_eproc15:
	case k_entry_eproc16:
	case k_entry_eproc17:
	case k_entry_eproc18:
	case k_entry_eproc19:
	case k_entry_eproc20:
		cl3_call_extproc( (*tag)-k_entry_eproc1+1, xmv_w, &xmv_dspctrl,
			&xmv_par, &xmv_phase, &locstat );
		break;
	case k_entry_add_plugin:
		cl2_display_busy( xmv_w, TRUE );
		mg_print_lastcmd( xmv_w[k_widget_draw], "Add Plugin" );
		cl3_request_extproc( xmv_w, &locstat );
		cl2_display_busy( xmv_w, FALSE );
		break;
	case k_entry_adrm_request:
		HELPTEXT( "adrm_request" );
		mg_print_lastcmd( xmv_w[k_widget_draw], "AutoDRM Request" );
		if  (XtIsManaged(xmv_w[k_widget_adrm_selection_box]))  {
			XtUnmanageChild( xmv_w[k_widget_adrm_selection_box] );
		} else {
			XtManageChild( xmv_w[k_widget_adrm_selection_box] );
		} /*endif*/
		break;
	case k_entry_locate_tele:
		mg_print_lastcmd( xmv_w[k_widget_draw], "Localize" );
		cl2_display_busy( xmv_w, TRUE );
		cu_get_param_values( xmv_w, xmv_cpar );
		cu_localization(
			cu_get_string(xmv_w[k_widget_phase_name_text]),
			CUF_DOLOC_DISTANCE|CUF_DOLOC_ORIGIN, xmv_cpar, &locstat );
		xmv_cpar->soft_change = TRUE;
		cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
		xmv_cpar->soft_change = FALSE;
		cu_print_param_values( xmv_cpar );
		cl2_display_busy( xmv_w, FALSE );
		break;
	case k_entry_key_arrow_up:
	case k_entry_key_arrow_down:
	case k_entry_key_arrow_left:
	case k_entry_key_arrow_right:
		xm_move_drag_window( *tag );
		break;
	default:
		printf( "*SHM: illegal menu entry number\n" );
	} /*endswitch*/

	if  (reset_xcursor)
		mg_set_cursor( MGC_XCRSR_NORMAL );
	/* XtSetSensitive( xmv_w[k_widget_main], TRUE ); */
	if  (Severe(&locstat))
		cu_alert( locstat );

} /* end of xm_call_menu_select */



/*--------------------------------------------------------------------------*/



static void xm_call_scale( Widget widget, int *tag, XmScaleCallbackStruct *data )

/* manages time slider
 *
 * parameters of routine
 * Widget     widget;     input; scale widget
 * int        *tag;       input; scale number
 * XmScaleCallbackStruct *data; output; slider value
 */
{
	/* local variables */
	int      minlth;              /* slider value */
	char     str[BC_LINELTH+1];   /* number string */
	STATUS   locstat;             /* local status */
	int      dspwidth, dspheight; /* display size (drawing area) in pixel */
	int      newwidth, newheight; /* modified values */
	int      n;                   /* number of arguments */
	Arg      arglist[3];          /* argument list */
	int      u_draw_area_width;   /* drawing area width */
	int      u_draw_area_height;  /* drawing area height */

	/* executable code */

	minlth = data->value;
	switch  (*tag)  {
	case k_scale_read_grsn_length:
		sprintf( str, "%4.1f", (float)minlth );
		cu_set_string( xmv_w[k_widget_read_grsn_length], str );
		break;
	case k_scale_param_ctrl_zoom:
		xmv_dspctrl.zoom = pow( GpGetFloat(cGpF_trace_zoom_base),
			(float)minlth/GpGetFloat(cGpF_trace_zoom_exp) );
		locstat = BC_NOERROR;
		mg_tracedisplay( xmv_w[k_widget_draw], &xmv_dspctrl, &locstat );
		if  (Severe(&locstat))
			printf( "*SHM: xm_call_scale: redraw status %d\n", locstat );
		break;
	case k_scale_filter_autocut:
		xmv_cmd_filter.autocut = (float)minlth;
		break;
	case k_scale_setup_dh:
		u_draw_area_width = GpGetInt( cGpI_draw_area_width );
		u_draw_area_height = GpGetInt( cGpI_draw_area_height );
		dspwidth = u_draw_area_width;
		dspheight = u_draw_area_height;
		newheight = (int)((float)dspheight * pow(GpGetFloat(cGpF_area_zoom_base),
			(float)minlth/GpGetFloat(cGpF_area_zoom_exp)) );
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: new height %d\n", newheight );
		xmv_prevent_resize++;
		n = 0;
		XtSetArg( arglist[n], XmNheight, newheight );  n++;
		XtSetValues( xmv_w[k_widget_draw], arglist, 1 );
		locstat = cBcNoError;
		pix_resize_window_buffer( XtDisplay(xmv_w[k_widget_draw]),
			XtWindow(xmv_w[k_widget_draw]), &locstat );
		mg_tracedisplay( xmv_w[k_widget_draw], &xmv_dspctrl, &locstat );
		break;
	case k_scale_setup_dw:
		u_draw_area_width = GpGetInt( cGpI_draw_area_width );
		u_draw_area_height = GpGetInt( cGpI_draw_area_height );
		dspwidth = u_draw_area_width;
		dspheight = u_draw_area_height;
		newwidth = (int)((float)dspwidth * pow(GpGetFloat(cGpF_area_zoom_base),
			(float)minlth/GpGetFloat(cGpF_area_zoom_exp)) );
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: new width %d\n", newwidth );
		xmv_prevent_resize++;
		n = 0;
		XtSetArg( arglist[n], XmNwidth, newwidth );  n++;
		XtSetValues( xmv_w[k_widget_draw], arglist, 1 );
		locstat = cBcNoError;
		pix_resize_window_buffer( XtDisplay(xmv_w[k_widget_draw]),
			XtWindow(xmv_w[k_widget_draw]), &locstat );
		mg_tracedisplay( xmv_w[k_widget_draw], &xmv_dspctrl, &locstat );
		break;
	default:
		printf( "*SHM: xm_call_scale: this cannot happen\n" );
	} /*endswitch*/

} /* end of xm_call_scale */



/*--------------------------------------------------------------------------*/



static void xm_call_drawing_input( Widget widget, int *tag, 
	XmDrawingAreaCallbackStruct *data )

/* callback for input to drawing area window
 *
 * parameters of routine
 * Widget     widget;      input; widget ID
 * int        *tag;        input; 
 * DrawingAreaCallbackStruct *data;
 */
{
	/* local variables */
	static float last_time;       /* last time */
	float    time;                /* selected time */
	int      trcno;               /* trace number */
	BOOLEAN  ok;                  /* position ok */
	char     cmd[BC_LINELTH+1];   /* command line */

	/* executable code */

	if  (data->reason != XmCR_INPUT)  {
		printf( "*SHM: strange input\n" );
		return;
	} /*endif*/

	printf( "input: \n" );

#ifdef XXX
	mg_get_time_and_trace( data->event->xbutton.x, data->event->xbutton.y,
		&time, &trcno, &ok );

	if  (!ok || trcno == 0)  {
		if  (data->event->type == ButtonPress)
			mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "dtw");
		return;
	} /*endif*/

	if  (data->event->type == ButtonPress)  {
		/* mg_mark_trace( widget, trcno, time ); */
		last_time = time;
		return;
	} else if  (data->event->type == ButtonRelease)  {
		sprintf( cmd, "stw %f %f", last_time, time );
		mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, cmd );
	} /*endif*/
#endif

} /* end of xm_call_drawing_input */



/*--------------------------------------------------------------------------*/



void xm_action_motion( Widget w, XEvent *ev, String *params, Cardinal *parno )

/* action routine for pointer motion
 *
 * parameters of routine
 * Widget     w;      input; widget ID
 * XEvent     *ev;    input; current event
 * ...   (don't know much about it)
 */
{
	/* local variables */
	static TSyBoolean time_btn_pressed=FALSE;     /* Time Button is pressed */
	static TSyBoolean phase_btn_pressed=FALSE;    /* Phase Button is pressed */
	static int        curr_crsr=MGC_CRSR_NOCURSOR;     /* current cursor */
	static int        lastwdw;              /* last window */
	static Widget     lastwidget;           /* last widget */
	static Time       last_time=0;          /* last time of selection */
	static TSyBoolean to_be_deleted=FALSE;  /* phase should be deleted */
	static TSyBoolean select_active;        /* trace selection activated */
	static TSyBoolean select_on=FALSE;      /* select or deselect */
	static TSyBoolean close_phase_found;    /* close phase found */
	static int        rubber_mode=MGC_RUBBER_END;    /* draw rubber band */
	static TPiPhase   phase_backup;         /* backup phase used for b-phases */
	TSyStatus         status;               /* return status */
	int               wdwno;                /* window number */
	TPiPhase          close_phase;          /* close phase */
	TPiPhase          *close_phase_ptr;     /* pointer to close phase or NULL */
	TPiPhase          bck_phase;            /* backup storage phase info */
	TPiPhaseList      *plist;               /* pointer to phase list */
	TSyBoolean        crsr_ok;              /* cursor display ok ? */
	int               trcnum;               /* trace number */

	/* executable code */

	status = BC_NOERROR;

	if  (w == xmv_w[k_widget_calibwdw])  {
		cal_handle_xevent( w, ev );
		return;
	} else if  (w == xmv_w[k_widget_spctwdw])  {
		spc_handle_xevent( w, ev, &status );
		if  (Severe(&status))  cu_alert( status );
		return;
	} /*endif*/

	wdwno = (w == xmv_w[k_widget_draw]) ? MGC_WDW_MAIN : MGC_WDW_SINGLE;
	if  (wdwno != lastwdw)  {
		if  (curr_crsr != MGC_CRSR_NOCURSOR)
			mg_draw_cursor( lastwidget, lastwdw, &xmv_dspctrl, MGC_CRSR_OFF,
				0, 0, NULL );
		lastwdw = wdwno;
		lastwidget = w;
	} /*endif*/

	if  (ev->type == MotionNotify)  {
		if  (time_btn_pressed)  {
			if  (!XtIsManaged(xmv_w[k_widget_single_draw_box]))
				XtManageChild( xmv_w[k_widget_single_draw_box] );
			mg_do_drag( w, MGC_DRAG_CONTINUE, ev->xmotion.x,
				ev->xmotion.y );
			mg_plot_drag_window( xmv_w[k_widget_single_draw],
				xmv_onset_acc==CUC_ACCURACY_DISPLAY, &status );
			if  (Severe(&status))  cu_alert( status );
			if  (xmv_pm_on)  pmm_draw_pmotion( xmv_dspctrl.zoom );
			if  (xmv_polar_on)  pol_draw_poltraces( xmv_dspctrl.zoom, &status );
			if  (Severe(&status))  cu_alert( status );
		} else if  (phase_btn_pressed)  {
			if  (!XtIsManaged(xmv_w[k_widget_phase_box]))
				XtManageChild( xmv_w[k_widget_phase_box] );
			mg_do_phase( w, wdwno, MGC_PHASE_START, ev->xmotion.x,
				ev->xmotion.y, xmv_phase.onset, &trcnum, NULL );
			xmv_phasetrc = mg_trcptr( trcnum );
			mg_draw_cursor( w, wdwno, &xmv_dspctrl, MGC_CRSR_CONTINUE,
				ev->xmotion.x, ev->xmotion.y, NULL );
		} else if  (ev->xmotion.state & XMC_PHASE_BUTTON_MASK)  {
			/* Select traces by moving over name with phase button pressed. */
			/* This is not covered by the above case, since 'phase_btn_pressed' */
			/* is selected only if cursor is within trace area.                 */
			if  (wdwno == MGC_WDW_MAIN)  {
				if  (select_on)  {
					mg_select_trace( xmv_w[k_widget_draw], ev->xbutton.x, ev->xbutton.y );
				} else {
					mg_deselect_trace( xmv_w[k_widget_draw], ev->xbutton.x, ev->xbutton.y );
				} /*endif*/
				cl_set_simfilter_active( xmv_w, !ts_some_trace_selected() );
			} /*endif*/
		} else {
			mg_draw_cursor( w, wdwno, &xmv_dspctrl, MGC_CRSR_CONTINUE,
				ev->xmotion.x, ev->xmotion.y, NULL );
			if  (rubber_mode > MGC_RUBBER_END)
				mg_rubber_line( w, wdwno, rubber_mode, ev->xmotion.x,
					ev->xmotion.y );
		} /*endif*/
		mg_print_time( xmv_w[k_widget_draw], wdwno, ev->xmotion.x,
			ev->xmotion.y );
	} else if  (ev->type == ButtonPress) {
		switch  (ev->xbutton.button)  {
		case XMC_TIME_BUTTON:
			if  (wdwno == MGC_WDW_SINGLE)  {
				xm_move_drag_window( k_entry_key_arrow_up );
				return;
			} /*endif*/
			mg_do_phase( (Widget)0, MGC_WDW_LAST, MGC_PHASE_CLEAR,
				0, 0, NULL, NULL, NULL );
			mg_draw_cursor( w, wdwno, &xmv_dspctrl, MGC_CRSR_OFF, 0, 0, NULL );
			time_btn_pressed = TRUE;
			if  (!XtIsManaged(xmv_w[k_widget_single_draw_box]))
				XtManageChild( xmv_w[k_widget_single_draw_box] );
			mg_do_drag( w, MGC_DRAG_START, ev->xbutton.x,
				ev->xbutton.y );
			mg_do_drag( w, MGC_DRAG_CONTINUE, ev->xbutton.x,
				ev->xbutton.y );
			mg_plot_drag_window( xmv_w[k_widget_single_draw],
				xmv_onset_acc==CUC_ACCURACY_DISPLAY, &status );
			if  (Severe(&status))  cu_alert( status );
			if  (xmv_pm_on)  pmm_draw_pmotion( xmv_dspctrl.zoom );
			if  (xmv_polar_on)  pol_draw_poltraces( xmv_dspctrl.zoom, &status );
			if  (Severe(&status))  cu_alert( status );
			break;
		case XMC_PHASE_BUTTON:
			/* if there is a selection on the station, mark it and return */
			if  (wdwno == MGC_WDW_MAIN &&
				mg_toggle_trace_selection(xmv_w[k_widget_draw],
				ev->xbutton.x,ev->xbutton.y))  {
				select_active = TRUE;
				select_on = !select_on;
				cl_set_simfilter_active( xmv_w, !ts_some_trace_selected() );
				break;
			} /*endif*/
			/* now this will create or change a phase */
			phase_btn_pressed = TRUE;
			/* Creating a phase changes processing state from inital to process */
			xm_set_processing_state( CUC_PARSTATE_PROCESS );
			/* save current phase to be able to restore it after pseudo-phase */
			phase_backup = xmv_phase;
			strcpy( phase_backup.name,
				cu_get_string(xmv_w[k_widget_phase_name_text]) );
			if  (!XtIsManaged(xmv_w[k_widget_phase_box]))
				XtManageChild( xmv_w[k_widget_phase_box] );
			/* decrease rubber mode if active */
			if  (rubber_mode > MGC_RUBBER_END)
				if  (--rubber_mode == MGC_RUBBER_END)
					mg_rubber_line( w, wdwno, rubber_mode, 0, 0 );
			/* reset close_phase name and check for an active selection */
			close_phase.name[0] = '\0';
			close_phase_ptr = (xmv_select.waiting == 0) ? &close_phase : NULL;
			/* an active selection prevents the 'close_phase' mechanism */
			mg_do_phase( w, wdwno, MGC_PHASE_START, ev->xbutton.x,
				ev->xbutton.y, xmv_phase.onset, &trcnum, close_phase_ptr );
			xmv_phasetrc = mg_trcptr( trcnum );
			close_phase_found = (close_phase.name[0] != '\0');
			if  (close_phase_found)  {
				status = cBcNoError;
				PmRemovePhase( xmv_phasetrc, &close_phase, &status );
				/* mg_do_phase has in this case already deleted the phase from */
				/* memory but not removed from screen (bit stupid, I know).    */
				/* So this has be done separately. */
				mg_mark_one_phase( xmv_w[k_widget_draw],
					xmv_w[k_widget_single_draw], &close_phase,
					xmv_phasetrc, xmv_onset_acc==CUC_ACCURACY_DISPLAY );
				cu_phase_box_defaults( xmv_w, &close_phase );
				if  (close_phase.name[0] != 'b')  bck_phase = xmv_phase;
				xmv_phase = close_phase;
			} else {
				/* A new phase is selected, don't take old amplitude values */
				xmv_phase.ampl = 0.0;
				xmv_phase.ampl_time = 0.0;
				xmv_phase.ampl_displ = 0.0;
				xmv_phase.ampl_veloc = 0.0;
				xmv_phase.period = 0.0;
				xmv_phase.magnitude = 0.0;
				xmv_phase.magn_source = cPiMagnUndefined;
				xmv_phase.ap_source = cPiApsrcUndefined;
				xmv_phase.bb.bbampl = 0.0;
				xmv_phase.bb.bbampl_time = 0.0;
				xmv_phase.bb.bbperiod = 0.0;
				xmv_phase.bb.mbb = 0.0;
				xmv_phase.onset_acc_l = cPiAccEmpty;
				xmv_phase.onset_acc_r = cPiAccEmpty;
			} /*endif*/
			if  (Abs(ev->xbutton.time - last_time)
				< GpGetInt(cGpI_double_click_time))
				to_be_deleted = TRUE;
			last_time = ev->xbutton.time;
			break;
		case XMC_CURSOR_BUTTON:
			if  (++curr_crsr > GpGetInt(cGpI_max_cursor_form))
				curr_crsr = MGC_CRSR_FIRST;
			if  (curr_crsr <= MGC_CRSR_WAVEFORM)
				mg_draw_cursor( w, wdwno, &xmv_dspctrl, MGC_CRSR_CLEAR, 0, 0, NULL);
			do  {
				mg_draw_cursor( w, wdwno, &xmv_dspctrl, curr_crsr,
					ev->xbutton.x, ev->xbutton.y, &crsr_ok );
				if  (!crsr_ok)
					if  (++curr_crsr > GpGetInt(cGpI_max_cursor_form))
						curr_crsr = MGC_CRSR_FIRST;
			}  while  (!crsr_ok);
			break;
		} /*endswitch*/
	} else if  (ev->type == ButtonRelease) {
		if  (ev->xbutton.button == XMC_TIME_BUTTON)  {
			if  (wdwno == MGC_WDW_SINGLE)  return;
			time_btn_pressed = FALSE;
#			ifdef XXX
			if  (!XtIsManaged(xmv_w[k_widget_single_draw_box]))
				XtManageChild( xmv_w[k_widget_single_draw_box] );
			mg_do_drag( w, MGC_DRAG_END, ev->xbutton.x, ev->xbutton.y );
			mg_plot_drag_window( xmv_w[k_widget_single_draw], &status );
			if  (Severe(&status))  cu_alert( status );
#			endif
		} else if  (ev->xbutton.button == XMC_PHASE_BUTTON)  {
			if  (select_active)  {
				select_active = FALSE;
			} else if  (xmv_phasetrc == NULL)  {
				/* ignore this */
			} else if  (xmv_select.waiting > 0)  {
				cl2_process_selection( xmv_w, &xmv_dspctrl, wdwno, xmv_cpar,
					&xmv_select, &xmv_phase, xmv_phasetrc, &status );
				if  (Severe(&status))  cu_alert( status );
			} else {
				strcpy( xmv_phase.name, cu_get_string(
					xmv_w[k_widget_phase_name_text]) );
				xmv_phase.source = cPiSourceManually;
				strcpy( xmv_phase.filter, xmv_cmd_filter.name );
				if  (to_be_deleted)  {
					to_be_deleted = FALSE;
					mg_do_phase( w, wdwno, MGC_PHASE_CLEAR, 0, 0, NULL, NULL, NULL );
				} else {
					cu_accept_phase( xmv_w[k_widget_draw],
						xmv_w[k_widget_single_draw], &xmv_phase, xmv_phasetrc,
						xmv_dspctrl.show_phase_acc, &status );
					/* prompt for onset accuracy if this is switched on and if */
					/* no dummy phase is created and no phase is moved or deleted */
					if  (xmv_onset_acc > CUC_ACCURACY_NONE
						&& xmv_phase.name[0] != 'b'
						&& xmv_phase.name[0] != 'w' && !close_phase_found
						&& status == BC_NOERROR)  {
						xmv_select.waiting = 2;
						xmv_select.processed = 0;
						xmv_select.initiator = CUC_SELTYPE_ONSET_ACC;
						/* the following phase structure will be changed when */
						/* the initiator is called */
						xmv_select.addparam = (void *)PmFindPhase( xmv_phasetrc,
							xmv_phase.name );
						strcpy( xmv_select.select[0].name, "w-acc-l" );
						strcpy( xmv_select.select[1].name, "w-acc-r" );
						strcpy( xmv_select.infotext, CUC_SELTEXT_ONSET_ACC );
						mg_set_cursor( MGC_XCRSR_RIGHT );
						mg_print_status( xmv_w[k_widget_draw],
							CUC_SELTEXT_ONSET_ACC, TRUE );
						mg_rubber_line( w, wdwno, MGC_RUBBER_START,
							ev->xbutton.x, ev->xbutton.y );
						rubber_mode = MGC_RUBBER_CONT_L;
					} /*endif*/
					/* restore phase info if pseudo-phase was created */
					if  (phase_backup.name[0] != '\0' && xmv_phase.name[0] == 'b')  {
						xmv_phase = phase_backup;
						cu_phase_box_defaults( xmv_w, &xmv_phase );
					} /*endif*/
				} /*endif*/
			} /*endif*/
			phase_btn_pressed = FALSE;
		} /*endif*/
	} else if  (ev->type == KeyPress) {
		{
		/* this is a workaround for accelerators not working on recent Suse  *
       * Linux implementations                                             */
#define MODIF_SHIFT 0x11
#define MODIF_LOCK  0x12
#define MODIF_CTRL  0x14
#define MODIF_ALT   0x18
		int tag;
		Modifiers mret;
		KeySym keysym;
		XtTranslateKeycode( ev->xkey.display, ev->xkey.keycode, ev->xkey.state,
			&mret, &keysym );
		if  (ev->xkey.state == MODIF_CTRL)  {
			switch  ((char)keysym)  {
			case '0':   tag = k_entry_eproc10;        break;
			case '1':   tag = k_entry_eproc1;         break;
			case '2':   tag = k_entry_eproc2;         break;
			case '3':   tag = k_entry_eproc3;         break;
			case '4':   tag = k_entry_eproc4;         break;
			case '5':   tag = k_entry_eproc5;         break;
			case '6':   tag = k_entry_eproc6;         break;
			case '7':   tag = k_entry_eproc7;         break;
			case '8':   tag = k_entry_eproc8;         break;
			case '9':   tag = k_entry_eproc9;         break;
			case 'a':   tag = k_entry_readah;         break;
			case 'b':   tag = k_entry_magn_mb;        break;
			case 'd':   tag = k_entry_deltheo;        break;
			case 'e':   tag = k_button_param_theo_ext;break;
			case 'f':   tag = k_entry_fk;             break;
			case 'g':   tag = k_entry_readgse;        break;
			case 'h':   tag = k_entry_cmd_screendump; break;
			case 'i':   tag = k_entry_pol_inc_cohlth; break;
			case 'j':   tag = k_entry_pol_inc_pow_linfil; break;
			case 'k':   tag = k_entry_command;        break;
			case 'l':   tag = k_entry_pol_dec_cohlth; break;
			case 'm':   tag = k_entry_magn_ms_plain;  break;
			case 'n':   tag = k_entry_mparam_sn_auto; break;
			case 'o':   tag = k_entry_cmd_del_horiz;  break;
			case 'p':   tag = k_entry_final_params;   break;
			case 'q':   tag = k_entry_cancel_params;  break;
			case 'r':   tag = k_entry_pol_dec_pow_linfil; break;
			case 's':   tag = k_entry_cmd_speccmd;    break;
			case 't':   tag = k_button_param_theo;    break;
			case 'v':   tag = k_entry_vespa;          break;
			case 'x':   tag = k_entry_help;           break;
			case 'y':   tag = k_entry_trc_sort_d;     break;
			case 'z':   tag = k_entry_quit;           break;
			default:    tag = 0;
				if  (GpGetInt(cGpI_debug_level) > 3)
					printf( "SHM-dbg4: pressed key %d, keysym %ld\n",
						ev->xkey.keycode, keysym );
			} /*endswitch*/
		} else {
			switch  ((char)keysym)  {
			case '0':   tag = k_entry_cmd_userdef;        break;
			case '1':   tag = k_entry_magn_ml;        break;
			case '3':   tag = k_entry_wdw_3traces;    break;
			case '5':   tag = k_entry_pmotion;        break;
			case '8':   tag = k_entry_spectrogram;    break;
			case '9':   tag = k_entry_spectrum;       break;
			case 'C':   tag = k_entry_configure;      break;
			case 'a':   tag = k_entry_locsat;         break;
			case 'b':   tag = k_entry_beam;           break;
			case 'c':   tag = k_entry_calib;          break;
			case 'd':   tag = k_entry_dtw;            break;
			case 'e':   tag = k_button_param_depth;   break;
			case 'f':   tag = k_entry_filter;         break;
			case 'g':   tag = k_entry_wdw_grow_right; break;
			case 'h':   tag = k_entry_ext_location;   break;
			case 'i':   tag = k_entry_align;          break;
			case 'k':   tag = k_entry_wdw_grow_left;  break;
			case 'l':   tag = k_entry_locate_tele;    break;
			case 'm':   tag = k_entry_wdw_move_right; break;
			case 'n':   tag = k_entry_wdw_move_left;  break;
			case 'o':   tag = k_entry_dump_params;    break;
			case 'p':   tag = k_entry_corrpick;       break;
			case 'q':   tag = k_entry_abort_selection;break;
			case 'r':   tag = k_entry_read_grsn;      break;
			case 's':   tag = k_entry_stw;            break;
			case 't':   tag = k_entry_rotate;         break;
			case 'u':   tag = k_entry_autopick1;      break;
			case 'v':   tag = k_entry_gencomment;     break;
			case 'w':   tag = k_entry_planewave;      break;
			case 'x':   tag = k_entry_amplper_p;      break;
			case 'y':   tag = k_entry_info_source;    break;
			case 'z':   tag = k_entry_amplper_z;      break;
			default:
				if  (keysym == GpGetInt(cGpI_keysym_arrow_up))  {
					tag = k_entry_key_arrow_up;
				} else if  (keysym == GpGetInt(cGpI_keysym_arrow_down))  {
					tag = k_entry_key_arrow_down;
				} else if  (keysym == GpGetInt(cGpI_keysym_arrow_left))  {
					tag = k_entry_key_arrow_left;
				} else if  (keysym == GpGetInt(cGpI_keysym_arrow_right))  {
					tag = k_entry_key_arrow_right;
				} else {
					tag = 0;
					if  (GpGetInt(cGpI_debug_level) > 2)
						printf( "SHM-dbg3: pressed key %d, keysym %ld\n",
							ev->xkey.keycode, keysym );
				} /*endif*/
			} /*endswitch*/
		} /*endif*/
		if  (GpGetInt(cGpI_debug_level) > 2)  printf( "SHM-dbg3: tag: %d\n", tag );
		if  (tag > 0)  xm_call_menu_select( w, &tag, NULL );
		}
	} else {
		if  (GpGetInt(cGpI_debug_level) > 0)
			printf( "SHM-dbg1: unhandled SHM event of type %d\n", ev->type );
	} /*endif*/

} /* end of xm_action_motion */



/*--------------------------------------------------------------------------*/



static void xm_call_text_input( Widget widget, int *tag,
	XmTextVerifyCallbackStruct *data )

/* callback on text edit actions
 *
 * parameters of routine
 * Widget     widget;       input; text widget ID
 * int        *tag;         input; widget number
 * XmTextVerifyCallbackStruct *data;
 */
{
	/* local variables */
	int      wno = *tag;              /* widget number */
	char     *cp;                     /* text pointer */
	char     text[BC_LINELTH+1];      /* editable text */
	int      slen;                    /* string length */
	int      pos;                     /* text position */

	/* executable code */

	/* check source string on special entries */
	if  ((widget == xmv_w[k_widget_param_azimuth_text]
		|| widget == xmv_w[k_widget_param_distance_text]
		|| widget == xmv_w[k_widget_param_origin_d_text]
		|| widget == xmv_w[k_widget_param_origin_t_text]
		|| widget == xmv_w[k_widget_param_lat_text]
		|| widget == xmv_w[k_widget_param_lon_text])
		&& xmv_cpar->source[0] == '\0'
		&& !xmv_cpar->soft_change)  {
		if  (!XtIsManaged(xmv_w[k_widget_infsource_box]))
			XtManageChild( xmv_w[k_widget_infsource_box] );
	} /*endif*/

	/* reset phase buttons on new phase */
	if  (widget == xmv_w[k_widget_phase_name_text])  {
		if  (XmToggleButtonGetState(xmv_w[k_widget_phase_spec_i]))  {
			XmToggleButtonSetState( xmv_w[k_widget_phase_spec_i], FALSE, TRUE );
			XmToggleButtonSetState( xmv_w[k_widget_phase_spec_e], TRUE, TRUE );
		} /*endif*/
		if  (XmToggleButtonGetState(xmv_w[k_widget_phase_reliab_no]))  {
			XmToggleButtonSetState( xmv_w[k_widget_phase_reliab_no], FALSE, TRUE );
			XmToggleButtonSetState( xmv_w[k_widget_phase_reliab_yes], TRUE, TRUE );
		} /*endif*/
		if  (!XmToggleButtonGetState(xmv_w[k_widget_phase_sign_0]))  {
			XmToggleButtonSetState( xmv_w[k_widget_phase_sign_0], TRUE, TRUE );
			XmToggleButtonSetState( xmv_w[k_widget_phase_sign_p], FALSE, TRUE );
			XmToggleButtonSetState( xmv_w[k_widget_phase_sign_m], FALSE, TRUE );
		} /*endif*/
	} /*endif*/

	if  (widget == xmv_w[k_widget_param_depth_text] && !xmv_cpar->soft_change)
		xmv_cpar->depth_type = CUC_DEPTH_ESTIMATED;

	if  (data->text->length == 1 && data->startPos == data->endPos)  {
#		ifdef XXX
		if  (*(data->text->ptr) == 'x')  {
			*text = '\0';
			cu_set_string( widget, text );
			return;
		} /*endif*/
#		endif
		cp = cu_get_string( widget );
		slen = strlen( cp );
		if  (slen > BC_LINELTH)  {
			printf( "*SHM: xm_call_text_input: text overflow ***\n" );
			return;
		} /*endif*/
		strcpy( text, cp );
		pos = data->startPos;
		if  (text[pos] == '\0')  return;  /* then appended to end */
		if  (pos >= 0 && pos < slen)  {
			strcpy( text+pos, text+pos+1 );
			cu_set_string( widget, text );
		} /*endif*/
	} /*endif*/

} /* end of xm_call_text_input */



/*--------------------------------------------------------------------------*/



static void xm_call_file_selection( Widget widget, int *tag,
	XmFileSelectionBoxCallbackStruct *data )

/* callback on file selection via FileSelectionBox
 *
 * parameters of routine
 * Widget     widget;     widget id of file selector
 * int        *tag;       input; not used here
 * XmFileSelectionBoxCallbackStruct *data; input; data returned by SelectionBox
 */
{
	/* local variables */
	STATUS   status;                  /* return status */
	int      mode;                    /* which kind of file */
	char     selfile[BC_FILELTH+1];   /* selected file */
	int      slen;                    /* string length */
	int      i, j;                    /* counters */
	char     syscmd[cBcVeryLongStrLth+1]; /* shell command */
	char     tmpfile[cBcFileLth+1];   /* scratch file */
	char     *env;                    /* pointer to environment */
	long     eventid;                 /* event ID for evt recovery */

	/* executable code */

	mg_set_cursor( MGC_XCRSR_BUSY );

	cl_file_select_callback( xmv_w, data, BC_FILELTH, selfile, &mode );

	status = BC_NOERROR;
	if  (mode == CLC_PATH_GSE || mode == CLC_PATH_GSE2 || mode == CLC_PATH_AH
		|| mode == CLC_PATH_Q || mode == CLC_PATH_RFMT)  {
		/* reset all parameters */
		PiClearAllPhases();
		cu_reset_phase( &xmv_phase );
		strcpy( xmv_phase.name, GpGetString(cGpS_auto_phase) );
		cu_phase_box_defaults( xmv_w, &xmv_phase );
		xmv_cmd_filter.autocut = 5.0;  /* shv_globals not yet implemented */
		mx_filter_box_defaults( xmv_w, &xmv_cmd_filter );
		xmv_dspctrl.zoom = 1.0;
		mx_clear_rotation();
		cu_reset_paramsets( &xmv_par );
		xmv_cpar = xmv_par.par;
		xmv_cpar->soft_change = TRUE;
		cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
		xmv_cpar->soft_change = FALSE;
		cl2_param_box_defaults( xmv_w, xmv_cpar /*, &xmv_dspctrl*/ );
		/* end of reset */
		mg_print_filter( xmv_w[k_widget_draw], "" );
		if  (mode == CLC_PATH_GSE)  {
			xmv_cmd_readg.format = MXC_FORMAT_GSE;
		} else if  (mode == CLC_PATH_GSE2)  {
			xmv_cmd_readg.format = MXC_FORMAT_GSE2;
		} else if  (mode == CLC_PATH_Q)  {
			xmv_cmd_readg.format = MXC_FORMAT_Q;
		} else if  (mode == CLC_PATH_RFMT)  {
			/* create tmp filename */
			sprintf( tmpfile, "%s%sRFMT.GSE", shd_scratch, id_shv );
			if  (GpGetInt(cGpI_debug_level) > 1)
				printf( "*SHM: reformatting file %s (tmp=%s)\n", selfile, tmpfile );
			/* reformat input file */
			sprintf( syscmd, "%sshm_exec_rfmt_other %s %s %s",
				GpGetString(cGpS_defpath_extprog), GpGetString(cGpS_reformat_proc),
				selfile, tmpfile );
			if  (GpGetInt(cGpI_debug_level) > 1)
				printf( "SHM-dbg2: executing >%s<\n", syscmd );
			system( syscmd );
			/* tell readg what to read */
			strcpy( selfile, tmpfile );
			xmv_cmd_readg.format = MXC_FORMAT_GSE2;
		} else {
			xmv_cmd_readg.format = MXC_FORMAT_AH;
		} /*endif*/
		strcpy( xmv_cmd_readg.filename, selfile );
		mx_readg( &xmv_cmd_readg, xmv_w[k_widget_draw],
			xmv_w[k_widget_single_draw], &xmv_dspctrl,
			FALSE, NULL, &status );
		/* delete scratch file if read from 'other' format */
		if  (mode == CLC_PATH_RFMT)  {
			sprintf( syscmd, "\\rm %s\n", tmpfile );
			system( syscmd );
		} /*endif*/
		if  (strcmp(GpGetString(cGpS_default_filter),"BUT_BP") == 0)  {
			cl_create_filter( xmv_w, CLC_CREFIL_BUT_BP, &status );
			if  (Severe(&status))  {
				mg_set_cursor( MGC_XCRSR_NORMAL );
				cu_alert( status );
				return;
			} /* endif*/
			strcpy( xmv_cmd_filter.name, cu_get_string(
				xmv_w[k_widget_filter_edit_text]) );
		} else {
			strcpy( xmv_cmd_filter.name, GpGetString(cGpS_default_filter) );
		} /*endif*/
		if  (xmv_cmd_filter.name[0] != '\0')  {
			cl2_display_busy( xmv_w, TRUE );
			mx_filter( &xmv_cmd_filter, xmv_w[k_widget_draw],
				&xmv_dspctrl, &status );
			cl2_display_busy( xmv_w, FALSE );
			mg_print_filter( xmv_w[k_widget_draw], xmv_cmd_filter.name );
		} /*endif*/
		/* mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "rd" ); */
		if  (*tag == k_button_read_grsn_readnew)  {
			cu_reset_phase( &xmv_phase );
			cu_reset_paramsets( &xmv_par );
			xmv_cpar = xmv_par.par;
			xmv_cpar->soft_change = TRUE;
			cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
			xmv_cpar->soft_change = FALSE;
		} /*endif*/
	} else if  (mode == CLC_PATH_SAVE)  {
		cl3_restore_parameters( selfile, xmv_cpar, &status );
		if  (Severe(&status))  {
			mg_set_cursor( MGC_XCRSR_NORMAL );
			cu_alert( status );
			return;
		} /*endif*/
		xmv_cpar->soft_change = TRUE;
		cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
		xmv_cpar->soft_change = FALSE;
		mg_tracedisplay( xmv_w[k_widget_draw], &xmv_dspctrl, &status );
		if  (Severe(&status))  {
			mg_set_cursor( MGC_XCRSR_NORMAL );
			cu_alert( status );
			return;
		} /*endif*/
		/* try to get event ID from filename */
		slen = strlen( selfile );
		for  (i=slen-13; i<slen-4; i++)
			if  (!isdigit(selfile[i]))  {
				mg_set_cursor( MGC_XCRSR_NORMAL );
				return;
			} /*endif*/
		if  (selfile[slen-14] == '1')  {
			if  (sscanf(selfile+slen-14,"%10d",&i) != 1)  {
				mg_set_cursor( MGC_XCRSR_NORMAL );
				return;
			} /*endif*/
		} else {
			if  (sscanf(selfile+slen-13,"%9d",&i) != 1)  {
				mg_set_cursor( MGC_XCRSR_NORMAL );
				return;
			} /*endif*/
		} /*endif*/
		xmv_par.evid = i;
	} else if  (mode == CLC_PATH_EVT)  {
		cl3_restore_from_evt( selfile, xmv_cpar, &eventid, &status );
		if  (GpGetBoolean(cGpB_recover_evid))  xmv_par.evid = eventid;
		if  (Severe(&status))  {
			mg_set_cursor( MGC_XCRSR_NORMAL );
			cu_alert( status );
			return;
		} /*endif*/
		xmv_cpar->soft_change = TRUE;
		cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
		xmv_cpar->soft_change = FALSE;
		mg_tracedisplay( xmv_w[k_widget_draw], &xmv_dspctrl, &status );
		if  (Severe(&status))  {
			mg_set_cursor( MGC_XCRSR_NORMAL );
			cu_alert( status );
			return;
		} /*endif*/
	} /*endif*/

	mg_set_cursor( MGC_XCRSR_NORMAL );

} /* end of xm_call_file_selection */



/*--------------------------------------------------------------------------*/



static void xmh_read_gsefile( char datafile[], TSyStatus *status )

/* reads in GSE data file, append to display.
 * The code of this route was copied from the above routine
 * 'xm_call_file_selection' and shrunk to the case of reading GSE2 files.
 *
 * parameters of routine
 * char       datafile[]; input; name of data file
 */
{
	/* local variables */
	int      mode;                    /* which kind of file */
	char     selfile[BC_FILELTH+1];   /* selected file */
	int      slen;                    /* string length */
	int      i, j;                    /* counters */
	char     syscmd[cBcVeryLongStrLth+1]; /* shell command */
	char     tmpfile[cBcFileLth+1];   /* scratch file */
	char     *env;                    /* pointer to environment */

	/* executable code */

	mg_set_cursor( MGC_XCRSR_BUSY );

	/* reset all parameters */
#ifdef XXX
	PiClearAllPhases();
	cu_reset_phase( &xmv_phase );
	strcpy( xmv_phase.name, GpGetString(cGpS_auto_phase) );
	cu_phase_box_defaults( xmv_w, &xmv_phase );
	xmv_cmd_filter.autocut = 5.0;  /* shv_globals not yet implemented */
	mx_filter_box_defaults( xmv_w, &xmv_cmd_filter );
	xmv_dspctrl.zoom = 1.0;
	mx_clear_rotation();
	cu_reset_paramsets( &xmv_par );
	xmv_cpar = xmv_par.par;
	xmv_cpar->soft_change = TRUE;
	cu_set_param_values( xmv_w, xmv_cpar, xmv_par.parno );
	xmv_cpar->soft_change = FALSE;
	cl2_param_box_defaults( xmv_w, xmv_cpar /*, &xmv_dspctrl*/ );
	/* end of reset */
#endif
	mg_print_filter( xmv_w[k_widget_draw], "" );
	xmv_cmd_readg.format = MXC_FORMAT_GSE2;
	strcpy( xmv_cmd_readg.filename, datafile );
	xmv_cmd_readg.keep = TRUE;
	mx_readg( &xmv_cmd_readg, xmv_w[k_widget_draw],
		xmv_w[k_widget_single_draw], &xmv_dspctrl,
		FALSE, NULL, status );
	if  (strcmp(GpGetString(cGpS_default_filter),"BUT_BP") == 0)  {
		cl_create_filter( xmv_w, CLC_CREFIL_BUT_BP, status );
		if  (Severe(status))  {
			mg_set_cursor( MGC_XCRSR_NORMAL );
			return;
		} /* endif*/
		strcpy( xmv_cmd_filter.name, cu_get_string(
			xmv_w[k_widget_filter_edit_text]) );
	} else {
		strcpy( xmv_cmd_filter.name, GpGetString(cGpS_default_filter) );
	} /*endif*/
	if  (xmv_cmd_filter.name[0] != '\0')  {
		cl2_display_busy( xmv_w, TRUE );
		mx_filter( &xmv_cmd_filter, xmv_w[k_widget_draw],
			&xmv_dspctrl, status );
		cl2_display_busy( xmv_w, FALSE );
		mg_print_filter( xmv_w[k_widget_draw], xmv_cmd_filter.name );
	} /*endif*/
	/* mx_exec_sh( xmv_w[k_widget_draw], &xmv_dspctrl, "rd" ); */
	mg_set_cursor( MGC_XCRSR_NORMAL );

} /* end of xmh_read_gsefile */



/*--------------------------------------------------------------------------*/



static void xmh_log_action( char inf[], int num )

/* logs action info to file cGpS_motif_log.  If inf=="--reset--"
 * the file is rewound.
 *
 * parameters of routine
 * char       inf[];          input; info string
 * int        num;            input; ID number
 */
{
	/* local variables */
	static int log_cnt=0; /* log counter */
	FILE     *fp;         /* pointer to file */
	char     *mlog;       /* name of motif log file */

	/* executable code */

	mlog = GpGetString( cGpS_motif_log );
	if  (*mlog == '\0')  return;

	if  (inf != NULL)
		if  (strcmp(inf,"--reset--") == 0)  {
			log_cnt = 0;
			/* sy_fdelete( shv_global.motif_log ); */
			fp = sy_fopen( mlog, "a" );
			if  (fp == NULL)  return;
			fprintf( fp, "\n\nnew SHM session\n\n" );
			sy_fclose( fp );
			return;
		} /*endif*/

	fp = sy_fopen( mlog, "a" );
	if  (fp == NULL)  return;

	if  (++log_cnt % 15 == 0)  fprintf( fp, "\n" );
	if  (inf != NULL)
		if  (*inf != '\0')
			fprintf( fp, "%s", inf );
	fprintf( fp, "[%d]", num );
	sy_fclose( fp );

} /* end of xmh_log_action */



/*--------------------------------------------------------------------------*/



#define EV(e) ((strcmp(event,e) == 0))



static void xm_exec_userdef( char fname[] )

/* performs sequence of user defined commands
 *
 * parameters of routine
 * char       fname[];       input; name of file with commands
 */
{
	/* local variables */
	FILE     *fp;                 /* pointer to input file */
	char     line[BC_LINELTH+1];  /* current line of file */
	char     event[BC_LINELTH+1]; /* name of event */
	int      item;                /* event item */
	BOOLEAN  m;                   /* menu or button */

	/* executable code */

	fp = sy_fopen( fname, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "*SHM: couldn't open userdef file %s\n", fname );
		return;
	} /*endif*/

	while  (fgets(line,BC_LINELTH,fp) != NULL)  {
		if  (*line == '\n' || *line == '!')  continue;
		sscanf( line, "%s", event );
		if       EV("command")   {item=k_entry_command; m=TRUE;}
		else if  EV("quit")      {item=k_entry_quit; m=TRUE;}
		else if  EV("read_grsn") {item=k_entry_read_grsn; m=TRUE;}
		/*  :  */
		/*  :  */
		else if  EV("read_grsn_nexttime")  {item=k_button_read_grsn_nexttime; m=FALSE;}
		else if  EV("read_grsn_prevtime")  {item=k_button_read_grsn_prevtime; m=FALSE;}
		else if  EV("read_grsn_mindn")     {item=k_button_read_grsn_mindn; m=FALSE;}
		else if  EV("read_grsn_read")      {item=k_button_read_grsn_read; m=FALSE;}
		else if  EV("read_grsn_readnew")   {item=k_button_read_grsn_readnew; m=FALSE;}
		else  {fprintf( stderr, "*SHM: userdef: unkown keyword %s\n", event ); continue;}
		if  (m)  {
			xm_call_menu_select( (Widget)0, &item, NULL );
		} else {
			xm_call_activate( (Widget)0, &item, NULL );
		} /*endif*/
	} /*endwhile*/

	sy_fclose( fp );

} /* end of xm_exec_userdef */



#undef EV



/*--------------------------------------------------------------------------*/



static void xm_read_requested_data( Widget w[], STATUS *status )

/* reads requested data
 *
 * parameters of routine
 * Widget     w[];         input; widget array
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	int      item;                  /* item to activate */
	char     old_dev[BC_FILELTH+1]; /* initial device name */
	char     *cptr;                 /* pointer to char */

	/* executable code */

	/* store initial values */
	cptr = cu_get_string( w[k_widget_read_grsn_device] );
	if  (cptr != NULL)  strcpy( old_dev, cptr );

	/* set new values */
	if  (getenv("SFD") != NULL)  {
		cu_set_string( w[k_widget_read_grsn_device], "SFD" );
	} else {
		cu_set_string( w[k_widget_read_grsn_device], "SH_SCRATCH" );
	} /*endif*/

	/* read data */
	item = k_button_read_grsn_read;
	xm_call_activate( (Widget)0, &item, NULL );

	/* restore initial values */
	cu_set_string( w[k_widget_read_grsn_device], old_dev );

} /* end of xm_read_requested_data */



/*--------------------------------------------------------------------------*/



static void xm_configure_windows( Widget wa[] )

/* configures windows
 *
 * no parameters
 */
{
	/* local variables */
	int      x, y, w, h, border;         /* window configuration */
	Widget   widget;                     /* current widget */

	/* executable code */

	widget = wa[k_widget_main];
	x = GpGetInt( cGpI_window_main_x );
	y = GpGetInt( cGpI_window_main_y );
	w = GpGetInt( cGpI_window_main_w );
	h = GpGetInt( cGpI_window_main_h );
	border = GpGetInt( cGpI_window_border );
	if  (XtIsManaged(widget))  XtUnmanageChild( widget );
	if  (w > 0 && h > 0) XtConfigureWidget( widget, x, y, w, h, border );
	XtManageChild( widget );
	XtMoveWidget( XtParent(widget), x, y );

	widget = wa[k_widget_single_draw_box];
	x = GpGetInt( cGpI_single_trace_box_x );
	y = GpGetInt( cGpI_single_trace_box_y );
	w = GpGetInt( cGpI_single_trace_box_w );
	h = GpGetInt( cGpI_single_trace_box_h );
	border = GpGetInt( cGpI_window_border );
	if  (XtIsManaged(widget))  XtUnmanageChild( widget );
	if  (w > 0 && h > 0) XtConfigureWidget( widget, x, y, w, h, border );
	XtManageChild( widget );
	XtMoveWidget( XtParent(widget), x, y );

	widget = wa[k_widget_phase_box];
	x = GpGetInt( cGpI_phase_box_x );
	y = GpGetInt( cGpI_phase_box_y );
	w = GpGetInt( cGpI_phase_box_w );
	h = GpGetInt( cGpI_phase_box_h );
	border = GpGetInt( cGpI_window_border );
	if  (XtIsManaged(widget))  XtUnmanageChild( widget );
	if  (w > 0 && h > 0) XtConfigureWidget( widget, x, y, w, h, border );
	XtManageChild( widget );
	XtMoveWidget( XtParent(widget), x, y );

	widget = wa[k_widget_param_box];
	x = GpGetInt( cGpI_parameter_box_x );
	y = GpGetInt( cGpI_parameter_box_y );
	w = GpGetInt( cGpI_parameter_box_w );
	h = GpGetInt( cGpI_parameter_box_h );
	border = GpGetInt( cGpI_window_border );
	if  (XtIsManaged(widget))  XtUnmanageChild( widget );
	if  (w > 0 && h > 0) XtConfigureWidget( widget, x, y, w, h, border );
	XtManageChild( widget );
	XtMoveWidget( XtParent(widget), x, y );

} /* end of xm_configure_windows */



/*--------------------------------------------------------------------------*/



static void xm_call_configure( void )

/* calls configure script and rereads SHM_USER_STARTUP.SHC
 *
 * parameters of routine
 * none
 */
{
	/* local variables */
	char     parfile[cBcFileLth+1];          /* parameter file */
	char     *env;                           /* pointer to environment */
	char     editcmd[cBcLongStrLth+1];       /* edit command */
	char     analyst_save[cGp_TEXTVARLTH+1]; /* analysts initials save storage */

	/* executable code */

	/* string length check has been done in the inital GpReadParfile already */

	/* get name of parameter file */
	env = (char *)getenv( "SH_USER_PARAM" );
	if  (env == NULL)  {
		env = (char *)getenv( "SH_INPUTS" );
		if  (env == NULL)  {
			fprintf( stderr, "SHM: this cannot happen, SH_INPUTS not defined\n" );
			exit( 1 );
		} /*endif*/
		strcpy( parfile, env );
		strcat( parfile, "/shm-config.txt" );
	} else {
		strcpy( parfile, env );
	} /*endif*/

	/* create edit command and execute it */
	if  (strlen(GpGetString(cGpS_texteditor))+strlen(parfile)+1 > cBcLongStrLth) {
		fprintf( stderr, "not very likely: edit command for parameter file too long\n" );
		exit( 1 );
	} /*endif*/
	strcpy( editcmd, GpGetString(cGpS_texteditor) );
	strcat( editcmd, " " );
	strcat( editcmd, parfile );
	system( editcmd );

	/* read possibly modified parameter file */
	strcpy( analyst_save, GpGetString(cGpS_analyst) );
	GpReadParfile();
	GpSetString( cGpS_analyst, analyst_save, NULL );

} /* end of xm_call_configure */



/*--------------------------------------------------------------------------*/



static void xm_fetch_widgets( void )

/* fetches all widgets
 *
 * no parameters
 */
{
	/* local variables */
	MrmType  wc_main;                /* main window class */
	MrmType  wc_cmd;                 /* command box class */
	MrmType  wc_draw;                /* drawing area class */
	MrmType  wc_grsn;                /* read grsn class */
	MrmType  wc_single;              /* single draw class */
	MrmType  wc_phase;               /* phase box class */
	MrmType  wc_param;               /* parameter box class */
	MrmType  class;                  /* class of widget */

	/* executable code */

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "fetching widgets ...\n" );
	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[main]" );
	if  (MrmFetchWidget(xmv_hier,"window_main",xmv_appshell,
		&xmv_w[k_widget_main],&wc_main) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch top level widget\n" );
	} /*endif*/
	XtManageChild( xmv_w[k_widget_main] );

	XtRealizeWidget( xmv_appshell );

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[single]" );
	if  (MrmFetchWidget(xmv_hier,"single_trace_box",xmv_appshell,
		xmv_w+k_widget_single_draw_box,&wc_single) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch single_draw widget\n" );
	} /*endif*/
	XtManageChild( xmv_w[k_widget_single_draw_box] );

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[command]" );
	if  (MrmFetchWidget(xmv_hier,"command_box",xmv_appshell,
		xmv_w+k_widget_command_box,&wc_cmd) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch command widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[read_dialog]" );
	if  (MrmFetchWidget(xmv_hier,"read_grsn_box",xmv_appshell,
		xmv_w+k_widget_read_grsn,&wc_grsn) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch read-grsn widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[calibration]" );
	if  (MrmFetchWidget(xmv_hier,"calibration_window_box",xmv_appshell,
		xmv_w+k_widget_calibration_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch calibration_window_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[spectrum]" );
	if  (MrmFetchWidget(xmv_hier,"spectrum_window_box",xmv_appshell,
		xmv_w+k_widget_spectrum_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch spectrum_window_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[pmotion]" );
	if  (MrmFetchWidget(xmv_hier,"pmotion_window_box",xmv_appshell,
		xmv_w+k_widget_pmotion_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch pmotion_window_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[polarfil]" );
	if  (MrmFetchWidget(xmv_hier,"polarfil_window_box",xmv_appshell,
		xmv_w+k_widget_polarfil_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch polarfil_window_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[phase_box]" );
	if  (MrmFetchWidget(xmv_hier,"phase_box",xmv_appshell,
		xmv_w+k_widget_phase_box,&wc_phase) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch phase_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[parbox]" );
	if  (MrmFetchWidget(xmv_hier,"parameter_box",xmv_appshell,
		xmv_w+k_widget_param_box,&wc_param) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch param_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[filter]" );
	if  (MrmFetchWidget(xmv_hier,"filter_selection_box",xmv_appshell,
		xmv_w+k_widget_filter_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch filter_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[alert]" );
	if  (MrmFetchWidget(xmv_hier,"alert_box",xmv_appshell,
		xmv_w+k_widget_alert_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch alert_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[multipli]" );
	if  (MrmFetchWidget(xmv_hier,"multiplication_box",xmv_appshell,
		xmv_w+k_widget_multiplication_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch multiplication_box widget\n" );
	} /*endif*/

#ifdef XXX
	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[hypoe]" );
	if  (MrmFetchWidget(xmv_hier,"hypoellipse_box",xmv_appshell,
		xmv_w+k_widget_hypoellipse_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch hypoellipse_box widget\n" );
	} /*endif*/
#endif

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[locsat]" );
	if  (MrmFetchWidget(xmv_hier,"locsat_box",xmv_appshell,
		xmv_w+k_widget_locsat_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch locsat_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[onsetpick]" );
	if  (MrmFetchWidget(xmv_hier,"onsetpick_box",xmv_appshell,
		xmv_w+k_widget_opick_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch onsetpick_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[infosource]" );
	if  (MrmFetchWidget(xmv_hier,"infsource_box",xmv_appshell,
		xmv_w+k_widget_infsource_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch infsource_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[fk_input]" );
	if  (MrmFetchWidget(xmv_hier,"fk_input_box",xmv_appshell,
		xmv_w+k_widget_fk_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch fk_input_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[theo_phase]" );
	if  (MrmFetchWidget(xmv_hier,"theo_phase_box",xmv_appshell,
		xmv_w+k_widget_theo_phase_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch theo_phase_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[help_box]" );
	if  (MrmFetchWidget(xmv_hier,"help_box",xmv_appshell,
		xmv_w+k_widget_help_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch help_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[speccmd_box]" );
	if  (MrmFetchWidget(xmv_hier,"speccmd_box",xmv_appshell,
		xmv_w+k_widget_speccmd_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch speccmd_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[vespa_input_box]" );
	if  (MrmFetchWidget(xmv_hier,"vespa_input_box",xmv_appshell,
		xmv_w+k_widget_vespa_input_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch vespa_input_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[setup_box]" );
	if  (MrmFetchWidget(xmv_hier,"setup_box",xmv_appshell,
		xmv_w+k_widget_setup_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch setup_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[refstat_box]" );
	if  (MrmFetchWidget(xmv_hier,"refstation_box",xmv_appshell,
		xmv_w+k_widget_refstat_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch refstat_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[event_attrib_box]" );
	if  (MrmFetchWidget(xmv_hier,"event_attrib_box",xmv_appshell,
		xmv_w+k_widget_event_attrib_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch event_attrib_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[spcsetup_box]" );
	if  (MrmFetchWidget(xmv_hier,"spcsetup_box",xmv_appshell,
		xmv_w+k_widget_spcsetup_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch spcsetup_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[adrm_request]" );
	if  (MrmFetchWidget(xmv_hier,"adrm_selection_box",xmv_appshell,
		xmv_w+k_widget_adrm_selection_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch adrm_selection_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "[add_plugin]" );
	if  (MrmFetchWidget(xmv_hier,"add_plugin_box",xmv_appshell,
		xmv_w+k_widget_add_plugin_box,&class) != MrmSUCCESS)  {
		fprintf( stderr, "*SHM: can't fetch add_plugin_box widget\n" );
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)  printf( "\n" );

} /* end of xm_fetch_widgets */



/*--------------------------------------------------------------------------*/



void xm_set_processing_state( int state )

/* Sets processing state, controls Read Again and Read New,
 * Enables/Disabled Quit
 *
 * parameters if routine
 * int        state;     input; new state
 */
{
	/* local variables */
	static int  last_state=CUC_PARSTATE_UNDEFINED;     /* last status */

	/* executable code */

	if  (state == last_state)  return;

	if  (state == CUC_PARSTATE_INITIAL)  {
		XtSetSensitive( xmv_w[k_widget_read_grsn_read_again], TRUE );
		XtSetSensitive( xmv_w[k_widget_read_grsn_read_new], TRUE );
		XtSetSensitive( xmv_w[k_widget_button_quit], TRUE );
		xmv_par.state = CUC_PARSTATE_INITIAL;
	} else if  (state == CUC_PARSTATE_FINAL)  {
		XtSetSensitive( xmv_w[k_widget_read_grsn_read_again], FALSE );
		XtSetSensitive( xmv_w[k_widget_read_grsn_read_new], TRUE );
		XtSetSensitive( xmv_w[k_widget_button_quit], TRUE );
		xmv_par.state = CUC_PARSTATE_INITIAL;
	} else {
		XtSetSensitive( xmv_w[k_widget_read_grsn_read_again], TRUE );
		XtSetSensitive( xmv_w[k_widget_read_grsn_read_new], FALSE );
		XtSetSensitive( xmv_w[k_widget_button_quit], FALSE );
		xmv_par.state = CUC_PARSTATE_PROCESS;
	} /*endif*/

	last_state = state;

} /* end of xm_set_read_state */



/*--------------------------------------------------------------------------*/



void xm_set_x_resources( Display *display )

/* Sets X resources to user defined colours
 *
 * no parameters passed
 */
{
	/* local variables */
	static XrmDatabase xrm_database;          /* X resource database */
	char     col0[cBcShortStrLth+1];          /* background colour 0 */
	char     col1[cBcShortStrLth+1];          /* background colour 1 */
	char     col2[cBcShortStrLth+1];          /* background colour 2 */
	char     fg[cBcShortStrLth+1];            /* foreground colour 2 */

	/* executable code */

	strcpy( col0, "#F7DEBD" );
	strcpy( col1, "#F2B16E" );
	strcpy( col2, "#63282A" );
	strcpy( fg,   "#63282A" );

	/* dialog foreground */
	fg[0] = '#';
	xm_hex_colour( GpGetFloat(cGpF_colour_dialog_fg_red),   fg+1, fg+2 );
	xm_hex_colour( GpGetFloat(cGpF_colour_dialog_fg_green), fg+3, fg+4 );
	xm_hex_colour( GpGetFloat(cGpF_colour_dialog_fg_blue),  fg+5, fg+6 );
	fg[7] = '\0';

	/* dialog background 0 (bright) */
	col0[0] = '#';
	xm_hex_colour( GpGetFloat(cGpF_colour_dialog_bg0_red),   col0+1, col0+2 );
	xm_hex_colour( GpGetFloat(cGpF_colour_dialog_bg0_green), col0+3, col0+4 );
	xm_hex_colour( GpGetFloat(cGpF_colour_dialog_bg0_blue),  col0+5, col0+6 );
	col0[7] = '\0';

	/* dialog background 1 (middle) */
	col1[0] = '#';
	xm_hex_colour( GpGetFloat(cGpF_colour_dialog_bg1_red),   col1+1, col1+2 );
	xm_hex_colour( GpGetFloat(cGpF_colour_dialog_bg1_green), col1+3, col1+4 );
	xm_hex_colour( GpGetFloat(cGpF_colour_dialog_bg1_blue),  col1+5, col1+6 );
	col1[7] = '\0';

	/* dialog background 2 (dark) */
	col2[0] = '#';
	xm_hex_colour( GpGetFloat(cGpF_colour_dialog_bg2_red),   col2+1, col2+2 );
	xm_hex_colour( GpGetFloat(cGpF_colour_dialog_bg2_green), col2+3, col2+4 );
	xm_hex_colour( GpGetFloat(cGpF_colour_dialog_bg2_blue),  col2+5, col2+6 );
	col2[7] = '\0';

	xrm_database = XtDatabase( display );
	XrmPutStringResource( &xrm_database, "SHM*background", col0 );
	XrmPutStringResource( &xrm_database, "SHM*foreground", fg );
	XrmPutStringResource( &xrm_database, "SHM*borderColor", col0 );
	XrmPutStringResource( &xrm_database, "SHM*XmBulletinBoard.background", col1 );
	XrmPutStringResource( &xrm_database, "SHM*XmRowColumn.background", col1 );
	XrmPutStringResource( &xrm_database, "SHM*XmForm.background", col1 );
	XrmPutStringResource( &xrm_database, "SHM*XmToggleButton.background", col1 );
	XrmPutStringResource( &xrm_database, "SHM*XmPushButton.background", col0 );
	XrmPutStringResource( &xrm_database, "SHM*XmLabel.background", col1 );
	XrmPutStringResource( &xrm_database, "SHM*XmScale.background", col1 );
	XrmPutStringResource( &xrm_database, "SHM*XmCommand.background", col1 );
	/*XrmPutStringResource( &xrm_database, "SHM*XmOptionMenu.background", col1 );*/

	XrmPutStringResource( &xrm_database, "SHM*read_grsn_main_button_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*read_grsn_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*add_plugin_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*plugin_main_button_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*analyst_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*analyst_main_button_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*adrm_selection_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*adrm_main_button_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*event_attrib_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*attrib_main_button_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*filter_selection_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*filter_main_button_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*fk_input_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*fk_main_button_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*infsource_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*infsource_main_button_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*locsat_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*locsat_main_button_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*multiplication_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*multipli_default_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*onsetpick_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*opick_main_button_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*parameter_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*param_main_button_box1.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*phase_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*phase_main_button_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*refstation_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*refstat_main_button_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*setup_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*setup_main_button_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*spcsetup_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*spcsetup_main_button_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*speccmd_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*speccmd_main_button_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*theo_phase_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*theo_phase_main_button_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*vespa_input_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*vespa_main_button_box.background", col2 );
	XrmPutStringResource( &xrm_database, "SHM*menu_main.background", col0 );
	XrmPutStringResource( &xrm_database, "SHM*menu_calibration.background", col0 );
	XrmPutStringResource( &xrm_database, "SHM*menu_spectrum.background", col0 );

} /* end of xm_set_x_resources */



/*--------------------------------------------------------------------------*/



void xm_hex_colour( float col, char *a, char *b )

/* returns hex code (2 chars) of colour coding
 *
 * parameters of routine
 * int        col;       input; colour part (R or G or B) between 0.0 and 1.0
 * char       *a, *b;    output; two single characters (byte hexcode)
 */
{
	/* local variables */
	int      val;        /* value */
	int      nibble;     /* nibble value */

	/* executable code */

	val = Nint(col * 255.0);

	/* hi nibble */
	nibble = (val >> 4) & 0x000F;
	if  (nibble < 10)  {
		*a = '0' + (char)nibble;
	} else {
		*a = 'A' + (char)(nibble-10);
	} /*endif*/

	/* lo nibble */
	nibble = val & 0x000F;
	if  (nibble < 10)  {
		*b = '0' + (char)nibble;
	} else {
		*b = 'A' + (char)(nibble-10);
	} /*endif*/

} /* end of xm_hex_colour */



/*--------------------------------------------------------------------------*/



void xm_move_drag_window( int key_entry )

/* moves drag window after arrow keys
 *
 * parameters of routine
 * int        key_entry;    input; which key pressed
 */
{
	/* local variables */
	int      x1, y1, x2, y2;   /* position of drag box */
	TSyBoolean valid;          /* drag box on? */
	int      x, y;             /* new position */
	int      tmp;              /* scratch */
	float    reltime;          /* trace time (not used here) */
	int      trcno;            /* trace number (not used here) */
	TSyStatus status;          /* return status */

	/* executable code */

	status = cBcNoError;

	mg_get_last_drag_box( &valid, &x1, &y1, &x2, &y2 );
	if  (!valid)  return;

	/* compute new drag box position */
	x = (x1+x2)/2;
	y = (y1+y2)/2;
	if  (key_entry == k_entry_key_arrow_left)  {
		tmp = x2 - x1;
		if  (tmp < 0)  tmp = -tmp;
		tmp /= 5;
		if  (tmp == 0)  tmp = 1;
		x -= tmp;
	} else if  (key_entry == k_entry_key_arrow_right)  {
		tmp = x2 - x1;
		if  (tmp < 0)  tmp = -tmp;
		tmp /= 5;
		if  (tmp == 0)  tmp = 1;
		x += tmp;
	} else if  (key_entry == k_entry_key_arrow_up)  {
		tmp = y2 - y1;
		if  (tmp < 0)  tmp = -tmp;
		y -= tmp;
	} else {
		tmp = y2 - y1;
		if  (tmp < 0)  tmp = -tmp;
		y += tmp;
	} /*endif*/

	/* check if new position is still within traces */
	mg_get_time_and_trace( x, y, &reltime, &trcno, &valid );
	if  (!valid)  return;

	if  (!XtIsManaged(xmv_w[k_widget_single_draw_box]))
		XtManageChild( xmv_w[k_widget_single_draw_box] );
	mg_do_drag( xmv_w[k_widget_draw], MGC_DRAG_START, x, y );
	mg_do_drag( xmv_w[k_widget_draw], MGC_DRAG_CONTINUE, x, y );
	mg_plot_drag_window( xmv_w[k_widget_single_draw],
		xmv_onset_acc==CUC_ACCURACY_DISPLAY, &status );
	if  (Severe(&status))  cu_alert( status );
	if  (xmv_pm_on)  pmm_draw_pmotion( xmv_dspctrl.zoom );
	if  (xmv_polar_on)  pol_draw_poltraces( xmv_dspctrl.zoom, &status );
	if  (Severe(&status))  cu_alert( status );

} /* end of xm_move_drag_window */



/*--------------------------------------------------------------------------*/
