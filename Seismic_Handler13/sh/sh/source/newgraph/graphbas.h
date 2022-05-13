
/* file GRAPHBAS.H
 *      ==========
 *
 * version 5, 21-May-92
 *
 * basic constants for graphic channels
 * K. Stammler, 9-AUG-91
 */

#ifndef __GRAPHBAS
#define __GRAPHBAS

#define GBC_MAXWDW 7
#define GBC_CYCLEWDW -1

typedef int CHMAP;
typedef float GBC_COO;

#define GBC_REPLACE 1           /* writing mode: replace */
#define GBC_TRANSP  2           /* writing mode: transparent */
#define GBC_XOR     3           /* writing mode: XOR */
#define GBC_REVTRAN 4           /* writing mode: reverse transparent */


#define GBC_WPAINT 1
	/* windowed paintbox type */
#define GBC_PPAINT 2
	/* plain paintbox type */

/*
 *   changes in the paintbox: update routines gc_playback AND gm_redraw
 */

typedef struct w_paintbox {
	int  pbtype;
	void (*prepare)( int wdw, STATUS *status );
	void (*moveto)( int wdw, GBC_COO x, GBC_COO y );
	void (*drawto)( int wdw, int style, GBC_COO x, GBC_COO y );
	void (*arrayplot)( int wdw, int style, long cnt, int red,
		GBC_COO xoff, GBC_COO xinc, GBC_COO yoff, GBC_COO yarr[],
		float yzoom, STATUS *status );
	void (*text)( int wdw, int style, GBC_COO x, GBC_COO y, char text[] );
	void (*setstyle)( int wdw, int style, char item[],
		char value[], STATUS *status );
	void (*setcoo)( int wdw, GBC_COO x, GBC_COO y, GBC_COO w,
		GBC_COO h, STATUS *status );
	void (*cleanup)( int wdw, char outf[], STATUS *status );
} W_PAINTBOX;

typedef struct p_paintbox {
	int  pbtype;
	void (*prepare)( STATUS *status );
	void (*moveto)( GBC_COO x, GBC_COO y );
	void (*drawto)( int style, GBC_COO x, GBC_COO y );
	void (*arrayplot)( int style, long cnt, int red,
		GBC_COO xoff, GBC_COO xinc, GBC_COO yoff, GBC_COO yarr[],
		float yzoom, STATUS *status );
	void (*text)( int style, GBC_COO x, GBC_COO y, char text[] );
	void (*setstyle)( int style, char item[], char value[], STATUS *status );
	void (*setcoo)( GBC_COO x, GBC_COO y, GBC_COO w,
		GBC_COO h, STATUS *status );
	void (*cleanup)( char outf[], STATUS *status );
} P_PAINTBOX;

typedef union paintbox {
	W_PAINTBOX  wpb;
	P_PAINTBOX  ppb;
} PAINTBOX;


/* redraw routine available for all graphic modules
 * defined in graphch.c
 */
#ifndef __THIS_IS_GRAPHCH
extern void (*gbv_playback)( CHMAP src, CHMAP dst, PAINTBOX *pb,
	char outf[], STATUS *status );
#endif /* __THIS_IS_GRAPHCH */


#endif /* __GRAPHBAS */

