/* dcomp6.f -- translated by f2c (version of 3 February 1990  3:36:42).
   You must link the resulting object file with the libraries:
	-lF77 -lI77 -lm -lc   (in that order)
*/

#include "f2c.h"

#define integer int
#define ftnlen int

/* Table of constant values */

static integer c__127 = 127;

/* *********************************************************************** */
/*                                                                      * */
/*     SUBROUTINE DCOMP6(LB,IBUF,LOUT,IOUT,IERROR)                      * */
/*                                                                      * */
/*     SUBROUTINE DECOMPRESS INTEGER DATA THAT HAS BEEN COMPRESSED      * */
/*     INTO ASCII CHARACTERS AND RETURNS VALUES IN INTEGER *4 FORMAT    * */
/*     SEE SUBROUTINE CMPRS6 FOR COMPRESSION FORMAT                     * */
/*     INPUT - IBUF AN ARRAY OF CONTAINING  LB CHARACTERS.              * */
/*     LOUT SHOULD CONTAIN THE DIMENSION OF THE INTEGER *4 ARRAY IOUT.  * */
/*     ON RETURN ARRAY LOUT WILL BE SET TO CONTAIN THE NUMBER OF INTEGER* */
/*     VALUES WHICH HAVE BEEN PUT IN THE ARRAY IOUT.                    * */
/*     IF THE ARRAY IOUT IS NOT LARGE ENOUGH TO CONTAIN ALL OF THE      * */
/*     DECOMPRESSED VALUES IERROR WILL BE SET TO -1 OTHERWISE IT WILL   * */
/*     SET TO ZERO                                                      * */
/* *********************************************************************** */

/* Subroutine */ int dcomp6_(long *lb, char *ibuf, int *lout, long *iout,
	int *ierror, ftnlen ibuf_len)
{
    /* Initialized data */

    static integer ichar[128] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,2,3,4,5,6,7,8,9,
	    10,11,0,0,0,0,0,0,0,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
	    27,28,29,30,31,32,33,34,35,36,37,0,0,0,0,0,0,38,39,40,41,42,43,44,
	    45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,0,0,0,0,
	    0,0 };
    static struct {
	char e_1[4];
	integer e_2;
	} equiv_1 = { {'1', '2', '3', '4'}, 0 };


    /* System generated locals */
    static integer equiv_0[1];

    /* Local variables */
    static integer imax;
#define test ((char *)&equiv_1)
    static integer mask1, mask2, i, j, k;
#define achar ((char *)equiv_0)
    static char lfeed[1];
    static integer isign, jsign;
    static char cretn[1];
    static integer ibyte, itemp;
#define itest ((integer *)&equiv_1)
    static char aspace[1];
    extern /* Subroutine */ int intand_();
    static integer ioflow, joflow, icount;
#define inn (equiv_0)

    /* Parameter adjustments */
    --ibuf;
    --iout;

    /* Function Body */
    *ierror = 0;
    *cretn = '\r';
    *lfeed = '\n';
    *aspace = ' ';
    imax = *lout;
    isign = 16;
    ioflow = 32;
    mask1 = 15;
    mask2 = 31;
/*     WORK OUT WHICH WAY BYTES ARE STORED IN COMPUTER */
    ibyte = 4;
    if (*itest == 875770417) {
	ibyte = 1;
    }
    icount = 0;
    i = 0;
    j = 0;
/*     START OF DECODING */
L1:
    ++i;
    achar[ibyte - 1] = ibuf[i];
/*       IF A CARRIAGE OR LINE FEED IGNORE, IF A SPACE THEN END OF DATA */

    if (achar[ibyte - 1] == *cretn) {
	goto L1;
    }
    if (achar[ibyte - 1] == *lfeed) {
	goto L1;
    }
    if (achar[ibyte - 1] == *aspace) {
	goto L5;
    }
    ++icount;
/*       STRIP OFF ANY HIGHER ORDER BITS */
    intand_(inn, &c__127, &k);
/*       GET NUMBER REPRESENTION OF INPUT CHARACTER */
    *inn = ichar[k - 1];
/*       GET SIGN BIT */
    intand_(inn, &isign, &jsign);
/*       GET CONTINUATION BIT (IF ANY) */
    intand_(inn, &ioflow, &joflow);
/*       REMOVE BITS WE DONT WANT */
    intand_(inn, &mask1, &itemp);
L2:
    if (joflow == 0) {
	goto L4;
    }
/*         THERE IS ANOTHER BYTE IN THIS SAMPLE */
    itemp <<= 5;
L3:
    ++i;
    achar[ibyte - 1] = ibuf[i];
    if (achar[ibyte - 1] == *cretn) {
	goto L3;
    }
    if (achar[ibyte - 1] == *lfeed) {
	goto L3;
    }
    ++icount;
/*         STRIP OFF ANY HIGHER ORDER BITS */
    intand_(inn, &c__127, &k);
    *inn = ichar[k - 1];
/*         GET CONTINUATION BIT (IF ANY) */
    intand_(inn, &ioflow, &joflow);
    intand_(inn, &mask2, &k);
    itemp += k;
    goto L2;
L4:
    if (jsign != 0) {
	itemp = -itemp;
    }
    ++j;
    if (j > imax) {
	goto L5;
    }
    iout[j] = itemp;
    if (icount < *lb) {
	goto L1;
    }
L5:
    *lout = j;
    if (j > imax) {
	*lout = imax;
	*ierror = -1;
    }
    return 0;
} /* dcomp6_ */

#undef inn
#undef itest
#undef achar
#undef test


/* intand.f -- translated by f2c (version of 3 February 1990  3:36:42).
   You must link the resulting object file with the libraries:
	-lF77 -lI77 -lm -lc   (in that order)
*/

/* #include "f2c.h" */

/* ********************************************************************** */
/*                                                                     * */
/*      SUBROUTINE INTAND(I1,I2,I3)                                    * */
/*                                                                     * */
/*      SUBROUTINE BITWISE "ANDS" THE INTEGERS IN I1 AND I2            * */
/*      AND RETURNS THE RESULT IN INTEGER I3                           * */
/*      FOR EXAMPLE                                                    * */
/*      IF THE 32 BITS IN I1 ARE 11001100 1110001110 11110000 11111111 * */
/*      AND IN I2 ARE            10101010 1100110011 00110011 00110011 * */
/*      I3 WILL BECOME           10001000 1100000010 00110000 00110011 * */
/*                                                                     * */
/*      NOTE "AND" IS NOT STANDARD FORTRAN 77 FUNCTION SO THIS IS A    * */
/*      MACHINE DEPENDANT SUBROUTINE                                   * */
/* ********************************************************************** */

/* Subroutine */ int intand_(int *i1, int *i2, int *i3)
{
/* ##SUN## REMOVE COMMENT FROM NEXT LINE FOR SUN COMPUTERS */
    *i3 = *i1 & *i2;
/* ##DEC## REMOVE COMMENT FROM NEXT LINE FOR PDP AND VAX COMPUTERS */
/*      I3 = JIAND(I1,I2) */
/* ##IBM## REMOVE COMMENT FROM NEXT LINE FOR IBM PC'S (I THINK) */
/*      I3 = IAND(I1,I2) */
    return 0;
} /* intand_ */

