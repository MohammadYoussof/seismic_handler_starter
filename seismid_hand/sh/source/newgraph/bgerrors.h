
/* file BGERRORS.H
 *      ==========
 *
 * version 1, 31-Dec-91
 *
 * error codes of module BGICH
 * K. Stammler, 31-Dec-91
 */

#ifndef __BGERRORS
#define __BGERRORS

#define BGE_NOERROR  0
#define BGE_OFFSET   3600
#define BGE_INITWICE (BGE_OFFSET+1)   /* attempt to initialise twice */
#define BGE_ILLCOO   (BGE_OFFSET+2)   /* illegal world coordinates */
#define BGE_NOWDW    (BGE_OFFSET+3)   /* no more windows */
#define BGE_READ     (BGE_OFFSET+4)   /* error on string input */
#define BGE_ILLWDW   (BGE_OFFSET+5)   /* illegal window coordinates */
#define BGE_FONT     (BGE_OFFSET+6)   /* set font failed */
#define BGE_OPNFIL   (BGE_OFFSET+7)   /* error opening file */
#define BGE_UDID     (BGE_OFFSET+8)   /* redraw file: unknown cmd ID */
#define BGE_STROVFL  (BGE_OFFSET+9)   /* string overflow */
#define BGE_ILSTYLE  (BGE_OFFSET+10)  /* illegal style number */
#define BGE_UKITEM   (BGE_OFFSET+11)  /* unknown style item */
#define BGE_ILVALUE  (BGE_OFFSET+12)  /* illegal value */

#endif  /* __BGERRORS */
