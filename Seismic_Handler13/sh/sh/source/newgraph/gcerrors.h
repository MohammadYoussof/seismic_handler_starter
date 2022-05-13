
/* file GCERRORS.H
 *      ==========
 *
 * version 2, 31-AUG-91
 *
 * error codes of module GRAPHCH
 * K. Stammler, 31-AUG-91
 */

#define GCE_NOERROR  0
#define GCE_OFFSET   1600
#define GCE_INITWICE (GCE_OFFSET+1)   /* attempt to initialise twice */
#define GCE_ILLCOO   (GCE_OFFSET+2)   /* illegal world coordinates */
#define GCE_NOWDW    (GCE_OFFSET+3)   /* no more windows */
#define GCE_READ     (GCE_OFFSET+4)   /* error on string input */
#define GCE_ILLWDW   (GCE_OFFSET+5)   /* illegal window coordinates */
#define GCE_FONT     (GCE_OFFSET+6)   /* set font failed */
#define GCE_OPNFIL   (GCE_OFFSET+7)   /* error opening file */
#define GCE_UDID     (GCE_OFFSET+8)   /* redraw file: unknown cmd ID */
#define GCE_STROVFL  (GCE_OFFSET+9)   /* string overflow */
#define GCE_ILSTYLE  (GCE_OFFSET+10)  /* illegal style number */
#define GCE_UKITEM   (GCE_OFFSET+11)  /* unknown style item */
#define GCE_ILVALUE  (GCE_OFFSET+12)  /* illegal value */
