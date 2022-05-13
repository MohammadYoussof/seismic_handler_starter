
/* file CCERRORS.H
 *      ==========
 *
 * version 2, 24-Sep-92
 *
 * status code definitions of module CCCH.C
 * K. Stammler, 21-AUG-1990
 */


#define CCE_NOERROR    0
#define CCE_OFFSET     3000
#define CCE_ILPAR      (CCE_OFFSET+1)    /* illegal parameter */
#define CCE_ILSTYLE    (CCE_OFFSET+2)    /* illegal style number */
#define CCE_UKITEM     (CCE_OFFSET+3)    /* unknown style attribute */
#define CCE_ILVALUE    (CCE_OFFSET+4)    /* illegal value of attribute */
#define CCE_JOBSUBMIT  (CCE_OFFSET+5)    /* error submitting job */
#define CCE_FOPNWR     (CCE_OFFSET+6)    /* error opening output file */
#define CCE_UKHCITEM   (CCE_OFFSET+7)    /* unknown HC item */
#define CCE_STROVFL    (CCE_OFFSET+8)    /* string overflow */
