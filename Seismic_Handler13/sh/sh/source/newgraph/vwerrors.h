
/* file VWERRORS.H
 *      ==========
 *
 * version 1, 29-Sep-92
 *
 * error definitions of VWCH.C
 * K. Stammler, 10-SEP-1990
 */

#define VWE_NOERROR    0
#define VWE_OFFSET     4200
#define VWE_ILWDW      (VWE_OFFSET+1)   /* illegal window number */
#define VWE_WOPNTWICE  (VWE_OFFSET+2)   /* window opened twice */
#define VWE_ILSTYLE    (VWE_OFFSET+3)   /* illegal style block */
#define VWE_ILSTYATT   (VWE_OFFSET+4)   /* unknown style attribute */
#define VWE_ILVALUE    (VWE_OFFSET+5)   /* illegal attribute value */
#define VWE_FOPNWR     (VWE_OFFSET+6)   /* error opening output file */
#define VWE_JOBSUBMIT  (VWE_OFFSET+7)   /* error submitting job */
#define VWE_STROVFL    (VWE_OFFSET+8)   /* string overflow */
#define VWE_UKHCITEM   (VWE_OFFSET+9)   /* unknown hardcopy item */
