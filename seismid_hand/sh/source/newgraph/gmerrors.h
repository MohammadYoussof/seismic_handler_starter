
/* file GMERRORS.H
 *      ==========
 *
 * version 2, 31-AUG-91
 *
 * error codes of module GEMCH
 * K. Stammler, 31-AUG-91
 */

#define GME_NOERROR  0
#define GME_OFFSET   3300
#define GME_INITWICE (GME_OFFSET+1)   /* attempt to initialise twice */
#define GME_ILLCOO   (GME_OFFSET+2)   /* illegal world coordinates */
#define GME_NOWDW    (GME_OFFSET+3)   /* no more windows */
#define GME_READ     (GME_OFFSET+4)   /* error on string input */
#define GME_ILLWDW   (GME_OFFSET+5)   /* illegal window coordinates */
#define GME_FONT     (GME_OFFSET+6)   /* set font failed */
#define GME_OPNFIL   (GME_OFFSET+7)   /* error opening file */
#define GME_UDID     (GME_OFFSET+8)   /* redraw file: unknown cmd ID */
#define GME_STROVFL  (GME_OFFSET+9)   /* string overflow */
#define GME_ILSTYLE  (GME_OFFSET+10)  /* illegal style number */
#define GME_UKITEM   (GME_OFFSET+11)  /* unknown style item */
#define GME_ILVALUE  (GME_OFFSET+12)  /* illegal value */
#define GME_NORSRC   (GME_OFFSET+13)  /* no resource file found */
