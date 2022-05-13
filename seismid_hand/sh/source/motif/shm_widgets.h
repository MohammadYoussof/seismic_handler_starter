
/* file shm_widgets.h
 *      ============
 *
 * version 57, 11-Jan-2007
 *
 * widgets numbers
 * K. Stammler, 29-Jun-93
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


/* widget numbers */
#define k_widget_main 1
#define k_widget_draw 2
#define k_widget_command_box 3
#define k_widget_read_grsn 4
#define k_widget_read_grsn_date 5
#define k_widget_read_grsn_time 6
#define k_widget_read_grsn_length 7
#define k_widget_read_grsn_device 8
#define k_widget_read_grsn_1hz 9
#define k_widget_read_grsn_20hz 10
#define k_widget_read_grsn_80hz 11
#define k_widget_read_grsn_station 12
   /* reserved 32 entries -> 12-43 */
#define k_widget_read_grsn_comp_z 44
#define k_widget_read_grsn_comp_n 45
#define k_widget_read_grsn_comp_e 46
#define k_widget_read_grsn_length_scale 47
#define k_widget_single_draw_box 48
#define k_widget_single_draw 49
#define k_widget_phase_box 50
#define k_widget_phase_sign_p 51
#define k_widget_phase_sign_m 52
#define k_widget_phase_sign_0 53
#define k_widget_phase_reliab_yes 54
#define k_widget_phase_reliab_no 55
#define k_widget_phase_name_text 56
#define k_widget_phase_spec_e 57
#define k_widget_phase_spec_i 58
#define k_widget_param_box 59
#define k_widget_param_azimuth_text 60
#define k_widget_param_distance_text 61
#define k_widget_param_depth_text 62
#define k_widget_param_slowness_text 63
#define k_widget_param_origin_d_text 64
#define k_widget_param_origin_t_text 65
#define k_widget_param_lat_text 66
#define k_widget_param_lon_text 67
#define k_widget_param_ctrl_zoom 70
#define k_widget_param_ctrl_norm 71
#define k_widget_param_ctrl_zoom_scale 72
#define k_widget_read_grsn_eventfile_text 73
#define k_widget_read_grsn_eventno_text 74
#define k_widget_filter_none 75
#define k_widget_filter_other 76
#define k_widget_filter_wwssn_sp 77
#define k_widget_filter_wwssn_lp 78
#define k_widget_filter_lrsm_sp 79
#define k_widget_filter_lrsm_lp 80
#define k_widget_filter_kirnos 81
#define k_widget_filter_woodand 82
#define k_widget_filter_standard_bp 83
#define k_widget_filter_but_bp 84
#define k_widget_filter_but_lp 85
#define k_widget_filter_but_hp 86
#define k_widget_filter_sro_lp 87
#define k_widget_filter_displace 88
   /* reserved 89 */
#define k_widget_filter_box 90
#define k_widget_filter_edit_text 91
#define k_widget_filter_autocut_scale 92
#define k_widget_alert_box 93
#define k_widget_multiplication_box 94
#define k_widget_multipli_edit_text 95
#define k_widget_multipli_value_label 96
#define k_widget_phase_type_other 97
#define k_widget_phase_type_local 98
#define k_widget_phase_type_regio 99
#define k_widget_phase_type_tele 100
#define k_widget_phase_type_nuclear 101
#define k_widget_phase_type_blast 102
#define k_widget_phase_type_mining 103
#define k_widget_filesel 104
#define k_widget_filter_butpar_lo_text 105
#define k_widget_filter_butpar_hi_text 106
#define k_widget_filter_butpar_order_text 107
#define k_widget_calibration_box 108
#define k_widget_calibwdw 109
#define k_widget_hypoellipse_box 110
#define k_widget_hypo_depth_text 111
#define k_widget_hypo_use_s_yes 112
#define k_widget_hypo_use_s_no 113
#define k_widget_phase_weight_option 114
/* #define k_widget_phase_weight_0 115 */
/* #define k_widget_phase_weight_1 116 */
/* #define k_widget_phase_weight_2 117 */
/* #define k_widget_phase_weight_3 118 */
/* #define k_widget_phase_weight_4 119 */
#define k_widget_analyst_box 120
#define k_widget_analyst_name_text 121
#define k_widget_filter_autocut_option 122
#define k_widget_phase_qual_0 123
	/* reserved 124-133 */
#define k_widget_infsource_box 134
#define k_widget_infsource_name_text 135
#define k_widget_menu_main 136
#define k_widget_menu_calib 137
#define k_widget_filter_autocut_0 138
#define k_widget_filter_autocut_5sec 139
#define k_widget_filter_autocut_20sec 140
#define k_widget_filter_autocut_1min 141
#define k_widget_filter_autocut_3min 142
#define k_widget_filter_autocut_5min 143
#define k_widget_filter_autocut_10min 144
	/* reserved 145-148 */
#define k_widget_param_locq_option 149
#define k_widget_param_locq_undefined 150
#define k_widget_param_locq_tooweak 151
#define k_widget_param_locq_incoherent 152
#define k_widget_param_locq_nobearing 153
#define k_widget_param_locq_region 154
#define k_widget_param_locq_reliable 155
#define k_widget_phase_type_option 156
#define k_widget_param_set_option 157
#define k_widget_param_set_1 158
#define k_widget_param_set_2 159
#define k_widget_param_set_3 160
#define k_widget_read_grsn_grf 161
#define k_widget_read_grsn_grsn 162
#define k_widget_fk_frqlo_text 163
#define k_widget_fk_frqhi_text 164
#define k_widget_fk_slowness_text 165
#define k_widget_fk_resol_text 166
#define k_widget_fk_colnum_text 167
#define k_widget_fk_box 168
#define k_widget_fk_unit_deg 169
#define k_widget_fk_unit_km 170
#define k_widget_theo_phase_box 171
#define k_widget_theo_phase_P 172
#define k_widget_theo_phase_pP 173
#define k_widget_theo_phase_sP 174
#define k_widget_theo_phase_PP 175
#define k_widget_theo_phase_PS 176
#define k_widget_theo_phase_PKPab 177
#define k_widget_theo_phase_PKPbc 178
#define k_widget_theo_phase_PKPdf 179
#define k_widget_theo_phase_pPKPab 180
#define k_widget_theo_phase_pPKPbc 181
#define k_widget_theo_phase_pPKPdf 182
#define k_widget_theo_phase_sPKPab 183
#define k_widget_theo_phase_sPKPbc 184
#define k_widget_theo_phase_sPKPdf 185
#define k_widget_theo_phase_PcP 186
#define k_widget_theo_phase_PcS 187
#define k_widget_theo_phase_PKKPdf 188
#define k_widget_theo_phase_PKiKP 189
#define k_widget_theo_phase_PPP 190
#define k_widget_theo_phase_Pdiff 191
#define k_widget_theo_phase_S 192
#define k_widget_theo_phase_pS 193
#define k_widget_theo_phase_sS 194
#define k_widget_theo_phase_SS 195
#define k_widget_theo_phase_SP 196
#define k_widget_theo_phase_SKSac 197
#define k_widget_theo_phase_SKSdf 198
#define k_widget_theo_phase_SKPab 199
#define k_widget_theo_phase_SKPdf 200
#define k_widget_theo_phase_sSKSac 201
#define k_widget_theo_phase_sSKSdf 202
#define k_widget_theo_phase_ScS 203
#define k_widget_theo_phase_ScP 204
#define k_widget_theo_phase_SKKSac 205
#define k_widget_theo_phase_SKKSdf 206
#define k_widget_theo_phase_SSS 207
#define k_widget_theo_phase_SKKPdf 208
#define k_widget_theo_phase_Sdiff 209
/* #define k_widget_theo_phase_pSKSac 210 */
/* #define k_widget_theo_phase_pSKSdf 211 */
/* reserved until 220 */
#define k_widget_theo_phase_edit 221
#define k_widget_phase_acc_option 222
#define k_widget_phase_acc_none 223
#define k_widget_phase_acc_query 224
#define k_widget_phase_acc_display 225
#define k_widget_help_box 226
#define k_widget_help_scroll 227
#define k_widget_speccmd_box 228
#define k_widget_speccmd_1_text 229
/* reserved until 248 */
#define k_widget_read_grsn_invhdr 250
#define k_widget_read_grsn_edit_hz 251
#define k_widget_read_grsn_hz_text 252
#define k_widget_vespa_input_box 253
#define k_widget_vespa_slolo_text 254
#define k_widget_vespa_slohi_text 255
#define k_widget_vespa_slostep_text 256
#define k_widget_vespa_power_text 257
#define k_widget_vespa_power_1 258
#define k_widget_vespa_power_2 259
#define k_widget_vespa_power_3 260
#define k_widget_vespa_power_5 261
#define k_widget_vespa_power_10 262
#define k_widget_vespa_slostep_1 263
#define k_widget_vespa_slostep_2 264
#define k_widget_vespa_slostep_3 265
#define k_widget_vespa_slohi_1 266
#define k_widget_vespa_slohi_2 267
#define k_widget_vespa_slohi_3 268
#define k_widget_vespa_ok 269
#define k_widget_vespa_cancel 270
#define k_widget_vespa_undo 271
#define k_widget_read_grsn_all 272
#define k_widget_locsat_box 273
#define k_widget_locsat_depth_text 274
#define k_widget_locsat_auto_yes 275
#define k_widget_locsat_auto_no 276
#define k_widget_locsat_freedom_text 277
#define k_widget_locsat_iterations_text 278
#define k_widget_locsat_prefix_text 279
#define k_widget_setup_box 280
#define k_widget_locsat_output_yes 281
#define k_widget_locsat_output_no 282
#define k_widget_read_grsn_todaysfd 283
#define k_widget_refstat_box 284
#define k_widget_refstat_station_1 285
/* reserved until 289 */
#define k_widget_refstat_station_last 290
#define k_widget_refstat_edit_text 291
#define k_widget_event_attrib_box 292
#define k_widget_event_public_option 293
#define k_widget_vespa_export_button 294
#define k_widget_setup_dh_scale 295
#define k_widget_setup_dw_scale 296
#define k_widget_read_grsn_fbox 297
#define k_widget_opick_box 298
#define k_widget_opick_thresh_text 299
#define k_widget_opick_duration_text 300
#define k_widget_opick_break_text 301
#define k_widget_spectrum_box 302
#define k_widget_spctwdw 303
#define k_widget_menu_spct 304
#define k_widget_spcsetup_box 305
#define k_widget_spcsetup_grid_on 306
#define k_widget_spcsetup_grid_off 307
#define k_widget_spcsetup_overlap_on 308
#define k_widget_spcsetup_overlap_off 309
#define k_widget_spcsetup_numwdw_text 310
#define k_widget_spcsetup_mode_powspc 311
#define k_widget_spcsetup_mode_fft 312
#define k_widget_spcsetup_mode_fft2 313
/* reserved until 317 */
#define k_widget_spcsetup_trunc_off 318
#define k_widget_spcsetup_trunc_on 319
#define k_widget_pmotion_box 320
#define k_widget_menu_pm 321
#define k_widget_pmwdw 322
#define k_widget_read_grsn_keep 323
#define k_widget_param_hypoloc 324
#define k_widget_read_grsn_comp_edit 325
#define k_widget_read_grsn_comp_text 326
#define k_widget_eproc1 327
#define k_widget_eproc2 328
#define k_widget_eproc3 329
#define k_widget_eproc4 330
#define k_widget_eproc5 331
#define k_widget_eproc6 332
#define k_widget_eproc7 333
#define k_widget_eproc8 334
#define k_widget_eproc9 335
#define k_widget_eproc10 336
#define k_widget_eproc11 337
#define k_widget_eproc12 338
#define k_widget_eproc13 339
#define k_widget_eproc14 340
#define k_widget_eproc15 341
#define k_widget_eproc16 342
#define k_widget_eproc17 343
#define k_widget_eproc18 344
#define k_widget_eproc19 345
#define k_widget_eproc20 346
#define k_widget_adrm_1 347
#define k_widget_adrm_2 348
#define k_widget_adrm_3 349
#define k_widget_adrm_4 350
#define k_widget_adrm_5 351
#define k_widget_adrm_6 352
#define k_widget_adrm_7 353
#define k_widget_adrm_8 354
#define k_widget_adrm_9 355
#define k_widget_adrm_10 356
#define k_widget_adrm_11 357
#define k_widget_adrm_12 358
#define k_widget_adrm_13 359
#define k_widget_adrm_14 360
#define k_widget_adrm_15 361
#define k_widget_adrm_selection_box 362
#define k_widget_adrm_reqtime_text 363
#define k_widget_adrm_readlth_text 364
#define k_widget_filter_butpar_hi_down 365
#define k_widget_filter_butpar_hi_up 366
#define k_widget_filter_butpar_lo_down 367
#define k_widget_filter_butpar_lo_up 368
#define k_widget_filter_butpar_ord_down 369
#define k_widget_filter_butpar_order_up 370
#define k_widget_read_grsn_read_again 371
#define k_widget_read_grsn_read_new 372
#define k_widget_button_quit 373
#define k_widget_button_locsat_hypo 374
#define k_widget_polarfil_box 375
#define k_widget_menu_polar 376
#define k_widget_polwdw 377
#define k_widget_fk_decim_text 378
#define k_widget_plugin_1 379
#define k_widget_plugin_2 380
#define k_widget_plugin_3 381
#define k_widget_plugin_4 382
#define k_widget_plugin_5 383
#define k_widget_plugin_6 384
#define k_widget_plugin_7 385
#define k_widget_plugin_8 386
#define k_widget_plugin_9 387
#define k_widget_plugin_10 388
#define k_widget_plugin_11 389
#define k_widget_plugin_12 390
#define k_widget_plugin_13 391
#define k_widget_plugin_14 392
#define k_widget_plugin_15 393
#define k_widget_add_plugin_box 394
