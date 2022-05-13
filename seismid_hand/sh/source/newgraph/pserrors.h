
/* file PSERRORS.H
 *      ==========
 *
 * version 2, 31-JUN-91
 *
 * status code definitions of module PSCH.C
 * K. Stammler, 20-JUN-91
 */


#define PSE_NOERROR    0
#define PSE_OFFSET     3100
#define PSE_ILPAR      (PSE_OFFSET+1)    /* illegal parameter */
#define PSE_ILSTYLE    (PSE_OFFSET+2)    /* illegal style number */
#define PSE_UKITEM     (PSE_OFFSET+3)    /* unknown style attribute */
#define PSE_ILVALUE    (PSE_OFFSET+4)    /* illegal value of attribute */
#define PSE_JOBSUBMIT  (PSE_OFFSET+5)    /* error submitting job */
#define PSE_FOPNWR     (PSE_OFFSET+6)    /* error opening output file */
#define PSE_UKHCITEM   (PSE_OFFSET+7)    /* unknown HC item */
#define PSE_STROVFL    (PSE_OFFSET+8)    /* string overflow */
