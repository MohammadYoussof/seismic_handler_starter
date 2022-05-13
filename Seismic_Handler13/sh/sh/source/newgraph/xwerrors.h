
/* file XWERRORS.H
 *      ==========
 *
 * version 3, 3-Sep-93
 *
 * errors codes of modul GRAPHCH.C
 * K. Stammler, 17-Sep-92
 */


#define XWE_NOERROR       0
#define XWE_OFFSET     3200
#define XWE_OPNDSP     (XWE_OFFSET+1)   /* error opening display */
#define XWE_WOPNTWICE  (XWE_OFFSET+2)   /* attempt to open window twice */
#define XWE_ILSTYLE    (XWE_OFFSET+3)   /* illegal style number */
#define XWE_ILSTYATT   (XWE_OFFSET+4)   /* unknown style attribute */
#define XWE_ILWDW      (XWE_OFFSET+5)   /* illegal window number */
#define XWE_COLALLOC   (XWE_OFFSET+6)   /* error allocating colour */
#define XWE_ILCOLOR    (XWE_OFFSET+7)   /* illegal color specification */
#define XWE_ILVALUE    (XWE_OFFSET+8)   /* illegal style value */
#define XWE_UKID       (XWE_OFFSET+9)   /* unknown command ID */
#define XWE_STROVFL    (XWE_OFFSET+10)  /* string overflow */
