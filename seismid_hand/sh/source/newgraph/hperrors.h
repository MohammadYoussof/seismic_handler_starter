
/* file HPERRORS.H
 *      ==========
 *
 * version 1, 25-Sep-92
 *
 * status code definitions of module HPCH.C
 * K. Stammler, 25-Sep-92
 */


#define HPE_NOERROR    0
#define HPE_OFFSET     3800
#define HPE_ILPAR      (HPE_OFFSET+1)    /* illegal parameter */
#define HPE_ILSTYLE    (HPE_OFFSET+2)    /* illegal style number */
#define HPE_UKITEM     (HPE_OFFSET+3)    /* unknown style attribute */
#define HPE_ILVALUE    (HPE_OFFSET+4)    /* illegal value of attribute */
#define HPE_NOHEADER   (HPE_OFFSET+5)    /* error opening header file */
#define HPE_FOPNWR     (HPE_OFFSET+6)    /* error opening output file */
#define HPE_UKHCITEM   (HPE_OFFSET+7)    /* unknown HC item */
#define HPE_STROVFL    (HPE_OFFSET+8)    /* string overflow */
#define HPE_NOTINIT    (HPE_OFFSET+9)    /* not initialized */
