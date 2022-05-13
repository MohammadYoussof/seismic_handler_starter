/********************************************************************************************************************************/
/* Created 17-JUL-1990 14:36:26 by VAX-11 SDL V3.1-7      Source: 11-APR-1990 12:39:36 VWSRESD$:[UIS.SRC]UISUSRDEF.SDL;1 */
/********************************************************************************************************************************/
 
/*** MODULE $UISUSRDEF IDENT X-92 ***/
#define uis$c_lib_native_uis 0
#define uis$c_lib_uisx 1
#define uis$c_efn_synch 30
#define UIS$C_TEXT8 8
#define UIS$C_TEXT16 16
#define UIS$C_TEXT_END_OF_LIST 0
#define UIS$C_TEXT_NOP 1
#define UIS$C_TEXT_SAVE_POSITION 2
#define UIS$C_TEXT_RESTORE_POSITION 3
#define UIS$C_TEXT_NEW_LINE 4
#define UIS$C_TEXT_WRITE 5
#define UIS$C_TEXT_ATB 6
#define UIS$C_TEXT_IGNORE 7
#define UIS$C_TEXT_HPOS_RELATIVE 8
#define UIS$C_TEXT_HPOS_ABSOLUTE 9
#define UIS$C_TEXT_TAB_RELATIVE 10
#define UIS$C_TEXT_TAB_ABSOLUTE 11
#define UIS$C_TEXT_VPOS_RELATIVE 12
#define UIS$C_TEXT_VPOS_ABSOLUTE 13
#define UIS$C_TEXT_MAX_OPERATION 14
#define UIS$C_TEXT_PATH_RIGHT 1
#define UIS$C_TEXT_PATH_LEFT 2
#define UIS$C_TEXT_PATH_DOWN 3
#define UIS$C_TEXT_PATH_UP 4
#define UIS$C_TEXT_FORMAT_NOJUSTIFY 0
#define UIS$C_TEXT_FORMAT_LEFT 1
#define UIS$C_TEXT_FORMAT_RIGHT 2
#define UIS$C_TEXT_FORMAT_JUSTIFY 3
#define UIS$C_TEXT_FORMAT_CENTER 4
#define UIS$C_WIDTH_PIXELS 0
#define UIS$C_WIDTH_WORLD 1
#define UIS$C_TERM_END_OF_LIST 0
#define UIS$C_TERM_COLOR 1
#define UIS$C_TERM_LENGTH 2
#define UIS$C_TERM_WIDTH 3
#define UIS$C_TERM_KB_ATTRIB 4
#define UIS$C_TERM_TYPE 5
#define UIS$C_TERM_OPTIONS_1 6
#define UIS$C_TERM_OPTIONS_2 7
#define UIS$C_TERM_OPTIONS_3 8
#define UIS$C_TERM_OPTIONS_4 9
#define UIS$C_TERM_OPTIONS_5 10
#define UIS$M_TERM_NOKB_BIND 1
#define UIS$M_TERM_NOKB_CREATE 2
struct TERM_KB_ATTRIB_BITS {
    unsigned UIS$V_TERM_NOKB_BIND : 1;
    unsigned UIS$V_TERM_NOKB_CREATE : 1;
    unsigned UIS$V_fill_0 : 6;
    } ;
#define UIS$C_FNT_END_OF_LIST 0
#define UIS$C_FNT_FIRST_CHAR 1
#define UIS$C_FNT_LAST_CHAR 2
#define UIS$C_FNT_FIXED 3
#define UIS$C_FNT_WIDTH 4
#define UIS$C_FNT_WYSIWYG 5
#define UIS$C_FNT_CELLEQRAST 6
#define UIS$C_FNT_VA_FONT 7
#define UIS$C_FNT_FONT_ID 8
#define UIS$C_FNT_WYS_CHEIGHT 9
#define UIS$C_FNT_WYS_ASCENDER 10
#define UIS$C_FNT_WYS_DECENDER 11
#define UIS$C_FNT_WYS_MAX_WIDTH 12
#define UIS$C_FNT_AVERAGE_GUT 13
#define UIS$C_FNT_GUTPERPIX_X 14
#define UIS$C_FNT_GUTPERPIX_Y 15
#define UIS$C_FNT_USER_FONT 16
#define UIS$C_FNT_MAX_REQUEST 17
#define UIS$M_FNTSIZ_WYSIWYG 1
struct GET_FONT_SIZE_BITS {
    unsigned UIS$V_FNTSIZ_WYSIWYG : 1;
    unsigned UIS$V_fill_1 : 7;
    } ;
#define UIS$C_MODE_TRAN 1
#define UIS$C_MODE_COPY 2
#define UIS$C_MODE_COMP 3
#define UIS$C_MODE_COPYN 4
#define UIS$C_MODE_OVER 5
#define UIS$C_MODE_OVERN 6
#define UIS$C_MODE_REPL 7
#define UIS$C_MODE_REPLN 8
#define UIS$C_MODE_ERAS 9
#define UIS$C_MODE_ERASN 10
#define UIS$C_MODE_BIS 11
#define UIS$C_MODE_BIC 12
#define UIS$C_MODE_BISN 13
#define UIS$C_MODE_BICN 14
#define UIS$C_MODE_XOR 15
#define UIS$C_ARC_OPEN 0
#define UIS$C_ARC_PIE 1
#define UIS$C_ARC_CHORD 2
#define PATT$C_BACKGROUND 1
#define PATT$C_FOREGROUND 2
#define PATT$C_VERT1_1 3
#define PATT$C_VERT1_3 4
#define PATT$C_VERT2_2 5
#define PATT$C_VERT3_1 6
#define PATT$C_VERT1_7 7
#define PATT$C_VERT2_6 8
#define PATT$C_VERT4_4 9
#define PATT$C_VERT6_2 10
#define PATT$C_HORIZ1_1 11
#define PATT$C_HORIZ1_3 12
#define PATT$C_HORIZ2_2 13
#define PATT$C_HORIZ3_1 14
#define PATT$C_HORIZ1_7 15
#define PATT$C_HORIZ2_6 16
#define PATT$C_HORIZ4_4 17
#define PATT$C_HORIZ6_2 18
#define PATT$C_GRID4 19
#define PATT$C_GRID8 20
#define PATT$C_UPDIAG1_3 21
#define PATT$C_UPDIAG2_2 22
#define PATT$C_UPDIAG3_1 23
#define PATT$C_UPDIAG1_7 24
#define PATT$C_UPDIAG2_6 25
#define PATT$C_UPDIAG4_4 26
#define PATT$C_UPDIAG6_2 27
#define PATT$C_DOWNDIAG1_3 28
#define PATT$C_DOWNDIAG2_2 29
#define PATT$C_DOWNDIAG3_1 30
#define PATT$C_DOWNDIAG1_7 31
#define PATT$C_DOWNDIAG2_6 32
#define PATT$C_DOWNDIAG4_4 33
#define PATT$C_DOWNDIAG6_2 34
#define PATT$C_BRICK_HORIZ 35
#define PATT$C_BRICK_VERT 36
#define PATT$C_BRICK_DOWNDIAG 37
#define PATT$C_BRICK_UPDIAG 38
#define PATT$C_GREY4_16D 39
#define PATT$C_GREY12_16D 40
#define PATT$C_BASKET_WEAVE 41
#define PATT$C_SCALE_DOWN 42
#define PATT$C_SCALE_UP 43
#define PATT$C_SCALE_RIGHT 44
#define PATT$C_SCALE_LEFT 45
#define PATT$C_FILLER6 46
#define PATT$C_FILLER7 47
#define PATT$C_GREY1_16 48
#define PATT$C_GREY2_16 49
#define PATT$C_GREY3_16 50
#define PATT$C_GREY4_16 51
#define PATT$C_GREY5_16 52
#define PATT$C_GREY6_16 53
#define PATT$C_GREY7_16 54
#define PATT$C_GREY8_16 55
#define PATT$C_GREY9_16 56
#define PATT$C_GREY10_16 57
#define PATT$C_GREY11_16 58
#define PATT$C_GREY12_16 59
#define PATT$C_GREY13_16 60
#define PATT$C_GREY14_16 61
#define PATT$C_GREY15_16 62
#define PATT$C_MAX_PATTERN 63
#define UIS$C_UNENCODED 1
#define UIS$C_CCITT_3_1D 2
#define UIS$C_CCITT_3_2D 3
#define UIS$C_CCITT_4_2D 4
#define WDPL$C_END_OF_LIST 0
#define WDPL$C_PLACEMENT 1
#define WDPL$C_ASSOC_WD 2
#define WDPL$C_WC_POS_X 3
#define WDPL$C_WC_POS_Y 4
#define WDPL$C_ABS_POS_X 5
#define WDPL$C_ABS_POS_Y 6
#define WDPL$C_ATTRIBUTES 7
#define WDPL$C_MAX_CODE 8
#define WDPL$M_TOP 1
#define WDPL$M_BOTTOM 2
#define WDPL$M_LEFT 4
#define WDPL$M_RIGHT 8
#define WDPL$M_INVISIBLE 16
#define WDPL$M_CENTER 32
#define WDPL$M_MAX_PLACEMENT 64
struct placement_bits {
    unsigned WDPL$V_TOP : 1;
    unsigned WDPL$V_BOTTOM : 1;
    unsigned WDPL$V_LEFT : 1;
    unsigned WDPL$V_RIGHT : 1;
    unsigned WDPL$V_INVISIBLE : 1;
    unsigned WDPL$V_CENTER : 1;
    unsigned WDPL$V_MAX_PLACEMENT : 1;
    unsigned WDPL$V_fill_2 : 1;
    } ;
#define WDPL$M_FILLER1 1
#define WDPL$M_NOBANNER 2
#define WDPL$M_NOBORDER 4
#define WDPL$M_NOMENU_ICON 8
#define WDPL$M_NOKB_ICON 16
#define WDPL$M_ALIGNED 32
#define WDPL$M_MAX_ATTRIBUTE 64
struct attribute_bits {
    unsigned WDPL$V_FILLER1 : 1;
    unsigned WDPL$V_NOBANNER : 1;
    unsigned WDPL$V_NOBORDER : 1;
    unsigned WDPL$V_NOMENU_ICON : 1;
    unsigned WDPL$V_NOKB_ICON : 1;
    unsigned WDPL$V_ALIGNED : 1;
    unsigned WDPL$V_MAX_ATTRIBUTE : 1;
    unsigned WDPL$V_fill_3 : 1;
    } ;
#define UIS$M_KB_AUTORPT 1
#define UIS$M_KB_KEYCLICK 2
#define UIS$M_KB_UDF6 4
#define UIS$M_KB_UDF11 8
#define UIS$M_KB_UDF17 16
#define UIS$M_KB_HELPDO 32
#define UIS$M_KB_UDE1 64
#define UIS$M_KB_ARROW 128
#define UIS$M_KB_KEYPAD 256
#define UIS$M_KB_DUMMY 512
#define UIS$M_KB_COMPOSE_LOCK 1024
#define UIS$M_KB_UDMAIN 2048
#define UIS$M_KB_UDCOMPOSE 4096
#define UIS$M_KB_UDTAB_RETURN 8192
#define UIS$M_KB_UDDELETE 16384
#define UIS$M_MAX_KB_ATTRIBUTE 32768
struct KB_ATTRIBUTE_BITS {
    unsigned UIS$V_KB_AUTORPT : 1;      /* Keyboard autorepeat              */
    unsigned UIS$V_KB_KEYCLICK : 1;     /* Keyclick                         */
    unsigned UIS$V_KB_UDF6 : 1;         /* Function keys F6-F10             */
    unsigned UIS$V_KB_UDF11 : 1;        /* Function keys F11-F14            */
    unsigned UIS$V_KB_UDF17 : 1;        /* Function keys F17-F20            */
    unsigned UIS$V_KB_HELPDO : 1;       /* Function keys for HELP and DO    */
    unsigned UIS$V_KB_UDE1 : 1;         /* Function keys E1-E6 (editing pad) */
    unsigned UIS$V_KB_ARROW : 1;        /* Arrow function keys              */
    unsigned UIS$V_KB_KEYPAD : 1;       /* Numeric keypad keys              */
    unsigned UIS$V_KB_DUMMY : 1;        /* Force to match with VWSSYSDEF    */
    unsigned UIS$V_KB_COMPOSE_LOCK : 1; /* Compose as LOCK key *INTERNAL USE ONLY* */
    unsigned UIS$V_KB_UDMAIN : 1;       /* Main keyboard array              */
    unsigned UIS$V_KB_UDCOMPOSE : 1;    /* Compose key up/down              */
    unsigned UIS$V_KB_UDTAB_RETURN : 1; /* Tab and Return keys up/down      */
    unsigned UIS$V_KB_UDDELETE : 1;     /* Delete key up/down               */
    unsigned UIS$V_MAX_KB_ATTRIBUTE : 1; /* First undefined bit             */
    } ;
#define UIS$M_KEY_COMPOSE 134217728
#define UIS$M_KEY_SHIFT 268435456
#define UIS$M_KEY_CTRL 536870912
#define UIS$M_KEY_LOCK 1073741824
#define UIS$M_KEY_DOWN -2147483648
struct kb_state_bits {
    unsigned short int UIS$W_KEY_CODE;
    unsigned UIS$V_KEY_FILLER : 11;
    unsigned UIS$V_KEY_COMPOSE : 1;
    unsigned UIS$V_KEY_SHIFT : 1;
    unsigned UIS$V_KEY_CTRL : 1;
    unsigned UIS$V_KEY_LOCK : 1;
    unsigned UIS$V_KEY_DOWN : 1;
    } ;
#define UIS$C_POINTER_BUTTON_1 400
#define UIS$C_POINTER_BUTTON_2 401
#define UIS$C_POINTER_BUTTON_3 402
#define UIS$C_POINTER_BUTTON_4 403
#define UIS$M_POINTER_BUTTON_3 1
#define UIS$M_POINTER_BUTTON_2 2
#define UIS$M_POINTER_BUTTON_1 4
#define UIS$M_POINTER_BUTTON_4 8
struct button_names {
    unsigned UIS$V_POINTER_BUTTON_3 : 1;
    unsigned UIS$V_POINTER_BUTTON_2 : 1;
    unsigned UIS$V_POINTER_BUTTON_1 : 1;
    unsigned UIS$V_POINTER_BUTTON_4 : 1;
    unsigned UIS$V_fill_4 : 4;
    } ;
#define UIS$M_NOWAIT 1
struct read_char_flags {
    unsigned UIS$V_NOWAIT : 1;
    unsigned UIS$V_fill_5 : 7;
    } ;
#define UIS$M_BIND_POINTER 1
struct pointer_pattern_flags {
    unsigned UIS$V_BIND_POINTER : 1;
    unsigned UIS$V_fill_6 : 7;
    } ;
#define UIS$C_OBJECT_SEGMENT 1
#define UIS$C_OBJECT_PLOT 2
#define UIS$C_OBJECT_TEXT 3
#define UIS$C_OBJECT_ELLIPSE 4
#define UIS$C_OBJECT_IMAGE 5
#define UIS$C_OBJECT_NEW_TEXT_LINE 6
#define UIS$C_OBJECT_LINE 7
#define UIS$C_OBJECT_COMPRESSED_IMAGE 8
#define UIS$m_filler1 1
#define UIS$m_filler2 2
#define UIS$m_filler3 4
#define UIS$M_DL_UPDATE_WINDOW 8
#define UIS$M_DL_MODIFY_LIST 16
#define UIS$M_DL_ENHANCE_LIST 32
struct display_list_mask {
    unsigned UIS$v_filler1 : 1;
    unsigned UIS$v_filler2 : 1;
    unsigned UIS$v_filler3 : 1;
    unsigned UIS$V_DL_UPDATE_WINDOW : 1;
    unsigned UIS$V_DL_MODIFY_LIST : 1;
    unsigned UIS$V_DL_ENHANCE_LIST : 1;
    unsigned UIS$V_fill_7 : 2;
    } ;
#define UIS$M_DL_SAME_SEGMENT 1
struct get_object_mask {
    unsigned UIS$V_DL_SAME_SEGMENT : 1;
    unsigned UIS$V_fill_8 : 7;
    } ;
#define UIS$M_DL_INSERT_AT_BEGIN 1
#define UIS$M_DL_INSERT_AFTER_OBJECT 2
#define UIS$M_DL_INSERT_BEFORE_OBJECT 4
struct insertion_mask {
    unsigned UIS$V_DL_INSERT_AT_BEGIN : 1;
    unsigned UIS$V_DL_INSERT_AFTER_OBJECT : 1;
    unsigned UIS$V_DL_INSERT_BEFORE_OBJECT : 1;
    unsigned UIS$V_fill_9 : 5;
    } ;
#define GER$C_SET_WRITING_MODE 1
#define GER$C_SET_WRITING_INDEX 2
#define GER$C_SET_BACKGROUND_INDEX 3
#define GER$C_SET_CHAR_SPACING 4
#define GER$C_SET_CHAR_SLANT 5
#define GER$C_SET_TEXT_SLOPE 6
#define GER$C_SET_TEXT_PATH 7
#define GER$C_SET_CHAR_TEXTURE 8
#define GER$C_SET_CHAR_UNDERSCORE 9
#define GER$C_SET_CHAR_BLINKING 10
#define GER$C_SET_TEXT_FORMATTING 11
#define GER$C_SET_CHAR_ROTATION 12
#define GER$C_SET_TEXT_MARGINS 13
#define GER$C_SET_LINE_WIDTH 14
#define GER$C_SET_LINE_STYLE 15
#define GER$C_SET_MARKER 16
#define GER$C_SET_FONT 17
#define GER$C_DEFINE_FONT 18
#define GER$C_TEXT 19
#define GER$C_filler5 20
#define GER$C_SET_POSITION 21
#define GER$C_NEW_TEXT_LINE 22
#define GER$C_PLOT 23
#define GER$C_MARKER 24
#define GER$C_ELLIPSE 25
#define GER$C_SET_ARC_TYPE 26
#define GER$C_OPEN_CURVE 27
#define GER$C_CLOSED_CURVE 28
#define GER$C_IMAGE 29
#define GER$C_PRIVATE 30
#define GER$C_BEGIN 31
#define GER$C_END 32
#define GER$C_ALIGN_POSITION 33
#define GER$C_BEGIN_DISPLAY 34
#define GER$C_END_DISPLAY 35
#define GER$C_VERSION 36
#define GER$C_SET_FILL_PATTERN 37
#define GER$C_SET_CLIP 38
#define GER$C_SET_CHAR_ENCODING 39
#define GER$C_FILLER2 40
#define GER$C_FILLER3 41
#define GER$C_SET_CHAR_SIZE 42
#define GER$C_IDENTIFICATION 43
#define GER$C_DATE 44
#define GER$C_NOP 45
#define GER$C_FILLER4 46
#define GER$C_SET_COLORS 47
#define GER$C_SET_INTENSITIES 48
#define GER$C_PRIVATE_ECO 49
#define GER$C_CREATE_COLOR_MAP 50
#define GER$C_DISPLAY_EXTENTS 51
#define GER$C_LINE 52
#define GER$C_SET_IMAGING_PARAMETERS 53
#define GER$C_WRITE_IMAGE 54
#define GER$C_SET_PLANE_MASK 55
#define GER$C_MAX_OPCODE 56
#define GER$K_LENGTH_DIFF -4
#define GER$S_ATTRIBUTE_HEADER 4
#define GER$M_CHAR_SIZE_FILLER1 1
#define GER$M_CHAR_SIZE_X 2
#define GER$M_CHAR_SIZE_Y 4
#define GER$M_CHAR_SIZE_FILLER_2 8
#define GER$M_CHAR_SIZE_ENABLE 1
#define GER$M_CHAR_SIZE_DEF_X 2
#define GER$M_CHAR_SIZE_DEF_Y 4
#define GER$M_CHAR_SIZE_DEF_CHAR 8
#define GER$S_OUTPUT_HEADER 2
#define GER$S_new_text_line 0
#define GER$M_COLOR_MAP_RESIDENT 1
#define GER$m_unused1_map 2
#define GER$M_COLOR_MAP_NO_BIND 4
#define GER$m_unused2_map 65528
#define GER$M_COLOR_MAP_SHARE 65536
#define GER$M_COLOR_MAP_SYSTEM 131072
#define GER$S_end_display 0
struct binary_encodings {
    short int GER$W_TYPE;
    short int GER$W_LENGTH;
    union  {
        long int GER$L_EXTRA_LENGTH;
        struct  {
            short int GER$W_SET_IATB;
            short int GER$W_SET_OATB;
            union  {
                struct  {
                    short int GER$W_ARC_TYPE;
                    } GER$r_arc_type;
                struct  {
                    short int GER$W_BACKGROUND_INDEX;
                    } GER$r_background_index;
                struct  {
                    short int GER$W_CHAR_BLINK_MODE;
                    } GER$r_char_blink;
                struct  {
                    short int GER$W_CHAR_ENCODING_TYPE;
                    } GER$r_char_encoding;
                struct  {
                    float GER$F_CHAR_ROTATION_ANGLE;
                    } GER$r_char_rotation;
                struct  {
                    union  {
                        short int GER$W_CHAR_SIZE_FLAGS;
                        struct  {
                            unsigned GER$V_CHAR_SIZE_FILLER1 : 1;
                            unsigned GER$V_CHAR_SIZE_X : 1;
                            unsigned GER$V_CHAR_SIZE_Y : 1;
                            unsigned GER$V_CHAR_SIZE_FILLER_2 : 1;
                            unsigned GER$V_fill_10 : 4;
                            } GER$r_char_size_obsolete;
                        struct  {
                            unsigned GER$V_CHAR_SIZE_ENABLE : 1;
                            unsigned GER$V_CHAR_SIZE_DEF_X : 1;
                            unsigned GER$V_CHAR_SIZE_DEF_Y : 1;
                            unsigned GER$V_CHAR_SIZE_DEF_CHAR : 1;
                            unsigned GER$V_fill_11 : 4;
                            } GER$r_char_size_bits;
                        } GER$r_char_size_flag_union;
                    short int GER$W_CHAR_SIZE_EXAMPLE;
                    float GER$F_CHAR_SIZE_WIDTH;
                    float GER$F_CHAR_SIZE_HEIGHT;
                    } GER$r_char_size;
                struct  {
                    float GER$F_CHAR_SLANT_ANGLE;
                    } GER$r_char_slant;
                struct  {
                    float GER$F_CHAR_SPACE_DX;
                    float GER$F_CHAR_SPACE_DY;
                    } GER$r_char_spacing;
                struct  {
                    short int GER$W_CHAR_UNDERSCORE_MODE;
                    } GER$r_char_underscore;
                struct  {
                    short int GER$W_CLIP_FLAGS;
                    float GER$F_CLIP_X1;
                    float GER$F_CLIP_Y1;
                    float GER$F_CLIP_X2;
                    float GER$F_CLIP_Y2;
                    } GER$r_clip;
                struct  {
                    short int GER$W_COLOR_COUNT;
                    short int GER$W_COLOR_INDEX;
                    char *GER$G_COLOR_VALUES;
                    } GER$r_color;
                struct  {
                    short int GER$W_FILL_FLAGS;
                    short int GER$W_FILL_INDEX;
                    } GER$r_fill_pattern;
                struct  {
                    short int GER$W_FONT_ID_LENGTH;
                    char *GER$G_FONT_ID_STRING;
                    } GER$r_font;
                struct  {
                    short int GER$W_INTENSITY_COUNT;
                    short int GER$W_INTENSITY_INDEX;
                    char *GER$G_INTENSITY_VALUES;
                    } GER$r_intensity;
                struct  {
                    long int GER$L_LINE_STYLE;
                    } GER$r_line_style;
                struct  {
                    float GER$F_LINE_WIDTH_NC;
                    float GER$F_LINE_WIDTH_DC;
                    short int GER$W_LINE_WIDTH_MODE;
                    } GER$r_line_width;
                struct  {
                    short int GER$W_MARKER_TYPE;
                    } GER$r_marker_type;
                struct  {
                    short int GER$W_TEXT_FORMAT_MODE;
                    } GER$r_text_format;
                struct  {
                    float GER$F_TEXT_MARGIN_X;
                    float GER$F_TEXT_MARGIN_Y;
                    float GER$F_TEXT_MARGIN_DISTANCE;
                    } GER$r_text_margins;
                struct  {
                    short int GER$W_TEXT_PATH_MAJOR;
                    short int GER$W_TEXT_PATH_MINOR;
                    } GER$r_text_path;
                struct  {
                    float GER$F_TEXT_SLOPE_ANGLE;
                    } GER$r_text_slope;
                struct  {
                    short int GER$W_WRITING_MODE;
                    } GER$r_writing_mode;
                struct  {
                    short int GER$W_WRITING_INDEX;
                    } GER$r_writing_index;
                struct  {
                    char GER$B_IMAGING_KERNELS [100];
                    char GER$B_IMAGING_THRESHOLD [4];
                    } GER$r_set_imaging_parameters;
                struct  {
                    long int GER$L_PLANE_MASK;
                    } GER$r_plane_mask;
                } GER$r_attribute_union;
            } GER$r_set_attribute_encodings;
        struct  {
            short int GER$W_OUTPUT_ATB;
            union  {
                struct  {
                    float GER$F_ELLIPSE_X;
                    float GER$F_ELLIPSE_Y;
                    float GER$F_ELLIPSE_WIDTH;
                    float GER$F_ELLIPSE_HEIGHT;
                    float GER$F_ELLIPSE_START_DEG;
                    float GER$F_ELLIPSE_END_DEG;
                    } GER$r_ellipse;
                struct  {
                    float GER$F_IMAGE_X1;
                    float GER$F_IMAGE_Y1;
                    float GER$F_IMAGE_X2;
                    float GER$F_IMAGE_Y2;
                    short int GER$W_IMAGE_WIDTH;
                    short int GER$W_IMAGE_HEIGHT;
                    short int GER$W_IMAGE_BPP;
                    long int GER$L_IMAGE_SIZE;
                    char *GER$G_IMAGE_DATA;
                    } GER$r_image;
                struct  {
                    float GER$F_COMPRESSED_IMAGE_X1;
                    float GER$F_COMPRESSED_IMAGE_Y1;
                    float GER$F_COMPRESSED_IMAGE_X2;
                    float GER$F_COMPRESSED_IMAGE_Y2;
                    long int GER$L_COMPRESSED_IMAGE_WIDTH;
                    long int GER$L_COMPRESSED_IMAGE_HEIGHT;
                    long int GER$L_COMPRESSED_IMAGE_BPP;
                    long int GER$L_COMPRESSED_IMAGE_SIZE;
                    long int GER$L_COMPRESSED_IMAGE_ROTATION;
                    short int GER$W_COMPRESSED_IMAGE_K_VALUE;
                    long int GER$L_COMPRESSED_IMAGE_ENCODE;
                    char *GER$G_COMPRESSED_IMAGE_DATA;
                    } GER$r_compressed_image;
                struct  {
                    short int GER$W_LINE_COUNT;
                    char *GER$G_LINE_DATA;
                    } GER$r_line;
                struct  {
                    float GER$F_MARKER_X;
                    float GER$F_MARKER_Y;
                    } GER$r_marker;
                struct  {
                    short int GER$W_PLOT_COUNT;
                    char *GER$G_PLOT_DATA;
                    } GER$r_plot;
                struct  {
                    short int GER$W_TEXT_ENCODING;
                    short int GER$W_TEXT_LENGTH;
                    char *GER$G_TEXT_DATA;
                    } GER$r_text;
                } GER$r_output_union;
            } GER$r_output_encodings;
        struct  {
            union  {
                struct  {
                    short int GER$W_ALIGN_POS_ATB;
                    float GER$F_ALIGN_POS_X;
                    float GER$F_ALIGN_POS_Y;
                    } GER$r_align_position;
                struct  {
                    float GER$F_DISPLAY_WC_MINX;
                    float GER$F_DISPLAY_WC_MINY;
                    float GER$F_DISPLAY_WC_MAXX;
                    float GER$F_DISPLAY_WC_MAXY;
                    float GER$F_DISPLAY_WIDTH;
                    float GER$F_DISPLAY_HEIGHT;
                    } GER$r_begin_display;
                struct  {
                    union  {
                        long int GER$L_COLOR_MAP_ATTRIBUTES;
                        struct  {
                            unsigned GER$V_COLOR_MAP_RESIDENT : 1;
                            unsigned GER$v_unused1_map : 1;
                            unsigned GER$V_COLOR_MAP_NO_BIND : 1;
                            unsigned GER$v_unused2_map : 13;
                            unsigned GER$V_COLOR_MAP_SHARE : 1;
                            unsigned GER$V_COLOR_MAP_SYSTEM : 1;
                            unsigned GER$V_fill_12 : 6;
                            } GER$r_attr_flags;
                        } GER$r_attr_union;
                    short int GER$W_COLOR_MAP_NAME_SIZE;
                    short int GER$W_COLOR_MAP_SIZE;
                    char GER$T_COLOR_MAP_NAME [15];
                    } GER$r_create_color_map;
                struct  {
                    short int GER$W_DATE_LENGTH;
                    char *GER$T_DATE_STRING;
                    } GER$r_date;
                struct  {
                    float GER$F_EXTENT_MINX;
                    float GER$F_EXTENT_MINY;
                    float GER$F_EXTENT_MAXX;
                    float GER$F_EXTENT_MAXY;
                    } GER$r_display_extents;
                struct  {
                    short int GER$W_IDENTIFICATION_LENGTH;
                    char *GER$T_IDENTIFICATION_STRING;
                    } GER$r_identification;
                struct  {
                    short int GER$W_PRIVATE_FACNUM;
                    short int GER$W_PRIVATE_LENGTH;
                    char *GER$G_PRIVATE_DATA;
                    } GER$r_private;
                struct  {
                    short int GER$W_PRIVATE_ECO_FACNUM;
                    short int GER$W_PRIVATE_ECO_MAJOR;
                    short int GER$W_PRIVATE_ECO_MINOR;
                    short int GER$W_PRIVATE_ECO_ECO;
                    } GER$r_private_eco;
                struct  {
                    float GER$F_TEXT_POS_X;
                    float GER$F_TEXT_POS_Y;
                    } GER$r_set_position;
                struct  {
                    short int GER$W_VERSION_MAJOR;
                    short int GER$W_VERSION_MINOR;
                    short int GER$W_VERSION_ECO;
                    } GER$r_version;
                } GER$r_misc_union;
            } GER$r_misc_encodings;
        } GER$r_value_area_union;
    } ;
#define GER$C_MAJOR_VERSION 3
#define GER$C_MINOR_VERSION 0
#define GER$S_SMALL_HEADER 4
#define GER$S_LARGE_HEADER 8
#define UIS$C_WS_BCOLOR 0
#define UIS$C_WS_FCOLOR 1
#define UIS$C_WS_BLACK 2
#define UIS$C_WS_WHITE 3
#define UIS$C_WS_RED 4
#define UIS$C_WS_GREEN 5
#define UIS$C_WS_BLUE 6
#define UIS$C_WS_CYAN 7
#define UIS$C_WS_YELLOW 8
#define UIS$C_WS_MAGENTA 9
#define UIS$C_WS_GREY25 10
#define UIS$C_WS_GREY50 11
#define UIS$C_WS_GREY75 12
#define UIS$C_MAX_COLOR_ID 13
#define UIS$C_WS_WSG_FG -1
#define UIS$C_WS_WSG_BG -2
#define UIS$C_WS_TERM_FG -3
#define UIS$C_WS_TERM_BG -4
#define UIS$C_WS_CURSOR_FG -5
#define UIS$C_WS_CURSOR_BG -6
#define UIS$C_MIN_COLOR_ID -7
#define UIS$C_COLOR_BASED 1
#define UIS$C_COLOR_GENERAL 2
#define UIS$C_COLOR_EXACT 3
#define UIS$C_DEV_MONO 1
#define UIS$C_DEV_INTENSITY 2
#define UIS$C_DEV_COLOR 3
#define UIS$C_DEV_NONRETRO 1
#define UIS$C_DEV_RETRO 2
#define UIS$C_COLOR_UNDEFINED 49280
#define UIS$C_DEFAULT_RESIZE -1
#define UIS$C_DEFAULT_CLOSE -1
#define UIS$C_DEFAULT_SHRINK_TO_ICON -1
#define UIS$C_DEFAULT_EXPAND_ICON -1
#define UIS$M_ICON_DEF_KB 1
#define UIS$M_ICON_DEF_BODY 2
struct shrink_to_icon_bits {
    unsigned UIS$V_ICON_DEF_KB : 1;
    unsigned UIS$V_ICON_DEF_BODY : 1;
    unsigned UIS$V_fill_13 : 6;
    } ;
#define VCMAL$C_END_OF_LIST 0
#define VCMAL$C_ATTRIBUTES 1
#define VCMAL$C_PARENT_VCM 2
#define VCMAL$C_PARENT_BASE 3
#define VCMAL$C_MAX_CODE 4
#define VCMAL$M_RESIDENT 1
#define VCMAL$M_SUBMAP 2
#define VCMAL$M_NO_BIND 4
#define VCMAL$M_PRCPRM 8
#define VCMAL$M_PERM 16
#define VCMAL$M_DEFAULT 32
#define VCMAL$m_atrfil6 64
#define VCMAL$m_atrfil7 128
#define VCMAL$m_atrfil8 256
#define VCMAL$m_atrfil9 512
#define VCMAL$m_atrfila 1024
#define VCMAL$m_atrfilb 2048
#define VCMAL$m_atrfilc 4096
#define VCMAL$m_atrfild 8192
#define VCMAL$m_atrfile 16384
#define VCMAL$m_atrfilf 32768
#define VCMAL$M_SHARE 65536
#define VCMAL$M_SYSTEM 131072
#define VCMAL$m_namfil2 262144
#define VCMAL$m_namfil3 524288
#define VCMAL$m_namfil4 1048576
#define VCMAL$m_namfil5 2097152
#define VCMAL$m_namfil6 4194304
#define VCMAL$m_namfil7 8388608
#define VCMAL$m_namfil8 16777216
#define VCMAL$m_namfil9 33554432
#define VCMAL$m_namfila 67108864
#define VCMAL$m_namfilb 134217728
#define VCMAL$m_namfilc 268435456
#define VCMAL$m_namfild 536870912
#define VCMAL$m_namfile 1073741824
#define VCMAL$m_namfilf -2147483648
struct vcm_attribute_bits {
    union  {
        union  {
            unsigned short int VCMAL$w_attr_flags;
            unsigned short int VCMAL$w_name_flags;
            struct  {
                unsigned VCMAL$V_RESIDENT : 1;
                unsigned VCMAL$V_SUBMAP : 1;
                unsigned VCMAL$V_NO_BIND : 1;
                unsigned VCMAL$V_PRCPRM : 1;
                unsigned VCMAL$V_PERM : 1;
                unsigned VCMAL$V_DEFAULT : 1;
                unsigned VCMAL$v_atrfil6 : 1;
                unsigned VCMAL$v_atrfil7 : 1;
                unsigned VCMAL$v_atrfil8 : 1;
                unsigned VCMAL$v_atrfil9 : 1;
                unsigned VCMAL$v_atrfila : 1;
                unsigned VCMAL$v_atrfilb : 1;
                unsigned VCMAL$v_atrfilc : 1;
                unsigned VCMAL$v_atrfild : 1;
                unsigned VCMAL$v_atrfile : 1;
                unsigned VCMAL$v_atrfilf : 1;
                unsigned VCMAL$V_SHARE : 1;
                unsigned VCMAL$V_SYSTEM : 1;
                unsigned VCMAL$v_namfil2 : 1;
                unsigned VCMAL$v_namfil3 : 1;
                unsigned VCMAL$v_namfil4 : 1;
                unsigned VCMAL$v_namfil5 : 1;
                unsigned VCMAL$v_namfil6 : 1;
                unsigned VCMAL$v_namfil7 : 1;
                unsigned VCMAL$v_namfil8 : 1;
                unsigned VCMAL$v_namfil9 : 1;
                unsigned VCMAL$v_namfila : 1;
                unsigned VCMAL$v_namfilb : 1;
                unsigned VCMAL$v_namfilc : 1;
                unsigned VCMAL$v_namfild : 1;
                unsigned VCMAL$v_namfile : 1;
                unsigned VCMAL$v_namfilf : 1;
                } VCMAL$r_attr_flags_bits;
            } VCMAL$r_attr_flags_union;
        } VCMAL$r_vcm_attributes_union;
    } ;
