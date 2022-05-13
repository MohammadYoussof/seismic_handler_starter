
/* file DJERRORS.H
 *      ==========
 *
 * version 2, 31-JUN-91
 *
 * status code definitions of module DJCH.C
 * K. Stammler, 20-JUN-91
 */


#define DJE_NOERROR    0
#define DJE_OFFSET     3200
#define DJE_ILPAR      (DJE_OFFSET+1)    /* illegal parameter */
#define DJE_ILSTYLE    (DJE_OFFSET+2)    /* illegal style number */
#define DJE_UKITEM     (DJE_OFFSET+3)    /* unknown style attribute */
#define DJE_ILVALUE    (DJE_OFFSET+4)    /* illegal value of attribute */
#define DJE_JOBSUBMIT  (DJE_OFFSET+5)    /* error submitting job */
#define DJE_FOPNWR     (DJE_OFFSET+6)    /* error opening output file */
#define DJE_UKHCITEM   (DJE_OFFSET+7)    /* unknown HC item */
#define DJE_STROVFL    (DJE_OFFSET+8)    /* string overflow */
#define DJE_NOTINIT    (DJE_OFFSET+9)    /* not initialised */
