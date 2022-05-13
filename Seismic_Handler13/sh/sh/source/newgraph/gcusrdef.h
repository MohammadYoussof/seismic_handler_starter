
/* file GCUSRDEF.H
 *      ==========
 *
 * version 16, 31-Aug-93
 *
 * prototypes of module GRAPHCH.C
 * K. Stammler, 9-AUG-91
 */


#ifndef __GCUSRDEF
#define __GCUSRDEF

#ifndef __BASECNST
#include BASECNST
#endif
#ifndef __GRAPHBAS
#include BC_GRAPHBAS
#endif

/* channel bits */
#define GCF_GEM       0x100
#define GCF_MEM       0x200
#define GCF_PSF       0x400
#define GCF_DJ        0x800
#define GCF_BGI       0x0000
#define GCF_CALCOMP   0x4000
#define GCF_TEK       0x8000
#define GCF_VWS       0x80
#define GCF_XWDW      0x2000
#define GCF_HPGL      0x20
#define GCF_STDCH     0x10        /* standard text channel */
#define GCF_FILE      0x00        /* text file channel */
#define GCF_NOECHO    0x40        /* no echo bit */
#define GCF_EXTERN    0x1000      /* external routine */
#define GCF_ALLCHAN   (GCF_GEM|GCF_MEM|GCF_PSF|GCF_DJ|GCF_BGI|GCF_CALCOMP|GCF_TEK|GCF_VWS|GCF_XWDW|GCF_HPGL)      /* all channels */
#define GCF_NUMMASK   7

#define GCF_HARDCOPY GCF_MEM

/* window attributes */
#define GCF_WNAME   0x01
#define GCF_WCLOSER 0x02
#define GCF_WFULLER 0x04
#define GCF_WMOVER  0x08
#define GCF_WINFO   0x10
#define GCF_WSIZER  0x20

typedef GBC_COO COOTYPE;


/* not implemented routines */
#define gc_sethc(g,i,v,s)
#define gc_hardcopy(m,s)


/* macros */
/* #define gc_redraw(c,s) gc_playback(c,c,NULL,s) */


/*------------------------------------------------------------------------*/


void gc_init( CHMAP map, int attribs, COOTYPE xlo, COOTYPE ylo,
	COOTYPE width, COOTYPE height, STATUS *status );

/* initialises GEM
 *
 * parameters of routine
 * CHMAP      map;       input; channel map
 * int        attribs;   input; window attributes
 * COOTYPE    xlo;       input; x-coo of lower left corner
 * COOTYPE    ylo;       input; y-coo of lower left corner
 * COOTYPE    width;     input; widths of window
 * COOTYPE    height;    input; height of window
 * STATUS     *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void gc_exit( CHMAP map );

/* closes channel
 *
 * parameters of routine
 * CHMAP      map;          input; window to be closed
 */


/*------------------------------------------------------------------------*/


void gc_finish( void );

/* closes all channels and terminates graphics
 *
 * no parameters
 */


/*------------------------------------------------------------------------*/


void gc_resizewdw( CHMAP map, COOTYPE xpos, COOTYPE ypos,
	COOTYPE width, COOTYPE height, STATUS *status );

/* resizes channel
 *
 * parameters of routine
 * CHMAP      map;            input; channel map
 * COOTYPE    xpos, ypos;     input; position of window
 * COOTYPE    width, height;  input; size of window
 * STATUS     *status;        output; return status
 */


/*------------------------------------------------------------------------*/


void gc_popwdw( CHMAP map );

/* pops window on top
 *
 * parameters of routine
 * CHMAP      map;            input; channel map
 */


/*------------------------------------------------------------------------*/


void gc_setwdwname( CHMAP map, char name[] );

/* sets new window name
 *
 * parameters of routine
 * CHMAP     map;       input; channel map
 * char      name[];    input; new name
 */


/*------------------------------------------------------------------------*/


void gc_setstyle( CHMAP map, unsigned style, char item[],
	char value[], STATUS *status );

/* sets style attribute
 *
 * parameters of routine
 * CHMAP      map;       input; channel map
 * unsigned   style;     input; style block number
 * char       item[];    input; name of attribute
 * char       value[];   input; new value of attribute (text)
 * STATUS     *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void gc_setcoo( CHMAP map, COOTYPE x, COOTYPE y, COOTYPE w, COOTYPE h,
	STATUS *status );

/* sets user coordinates in window.  If x,y,w,h are all equal to zero,
 * the last call is repeated.
 *
 * parameters of routine
 * CHMAP      map;        input; channel map
 * COOTYPE    x, y, w, h; input; user coordinates
 * STATUS     *status;    output; return status
 */


/*------------------------------------------------------------------------*/


float gc_aspectratio( CHMAP map );

/* returns ratio of width to height
 *
 * parameters of routine
 * CHMAP      map;      input; channel map
 *                      returns aspect ratio
 */


/*------------------------------------------------------------------------*/


void gc_moveto( CHMAP map, COOTYPE x, COOTYPE y );

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * CHMAP      map;      input; channel map
 * COOTYPE    x, y;     input; location to move to
 */


/*------------------------------------------------------------------------*/


void gc_drawto( CHMAP map, int style, COOTYPE x, COOTYPE y );

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * CHMAP      map;      input; channel map
 * int        style;    input; style block number
 * COOTYPE    x, y;     input; location to move to
 */


/*------------------------------------------------------------------------*/



void gc_setpixel( CHMAP map, int style, COOTYPE x, COOTYPE y );

/* sets pixel at position (x,y) in user coordinates
 *
 * parameters of routine
 * CHMAP      map;      input; channel map
 * int        style;    input; style block number
 * COOTYPE    x, y;     input; location to move to
 */


/*------------------------------------------------------------------------*/


void gc_arrayplot( CHMAP map, int style, long cnt, int red, COOTYPE xoff,
	COOTYPE xinc, COOTYPE yoff, COOTYPE yarr[], COOTYPE yzoom, STATUS *status );

/* plots an array of sample data
 *
 * parameters of routine
 * CHMAP      map;       input; channel map
 * int        style;     input; line style ID
 * long       cnt;       input; length of data array
 * int        red;       input; reduction factor for plotting
 * COOTYPE    xoff;      input; x-position of first sample
 * COOTYPE    xinc;      input; x increment
 * COOTYPE    yoff;      input; y-position of first sample
 * COOTYPE    yarr[];    input; sample data
 * COOTYPE    yzoom;     input; zoom factor of sample data
 * STATUS     *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void gc_erase( CHMAP map );

/* clears channel
 *
 * parameters of routine
 * CHMAP      map;       input; channel map
 */


/*------------------------------------------------------------------------*/


void gc_text( CHMAP map, int style, COOTYPE x, COOTYPE y, char text[] );

/* writes text to window
 *
 * parameters of routine
 * CHMAP      map;        input; channel map
 * int        style;      input; character style block number
 * COOTYPE    x, y;       input; text position
 * char       text[];     input; output text
 */


/*------------------------------------------------------------------------*/


void gc_write( CHMAP map, char text[] );

/* writes text to window at current write position
 *
 * parameters of routine
 * CHMAP      map;          input; channel map
 * char       text[];       input; output text
 */


/*------------------------------------------------------------------------*/


void gc_wrtch( CHMAP map, char ch );

/* writes a single character to channel
 *
 * parameters of routine
 * CHMAP      map;         input; channel map
 * char       ch;          input; char to be written
 */


/*------------------------------------------------------------------------*/


int gc_txtwidth( CHMAP map );

/* returns width of channel in characters
 *
 * parameters of routine
 * CHMAP      map;      input; channel map
 */


/*------------------------------------------------------------------------*/


int gc_txtheight( CHMAP map );

/* returns height of window in characters
 *
 * parameters of routine
 * CHMAP      wdw;     input; channel map
 */


/*------------------------------------------------------------------------*/


float gc_chheight( CHMAP map );

/* returns current character height in window "wdw"
 *
 * parameter of routine
 * CHMAP     map;      input; channel map
 */


/*------------------------------------------------------------------------*/


void gc_read( CHMAP map, int maxlth, char text[] );

/* reads text from terminal
 *
 * parameters of routine
 * CHMAP    map;          input; channel map
 * int      maxlth;       input; maximum length of text
 * char     text[];       output; text read
 */


/*------------------------------------------------------------------------*/


void gc_getloc( CHMAP map, COOTYPE *x, COOTYPE *y, char *ch );

/* requests mouse position in window
 *
 * parameters of routine
 * CHMAP      map;        input; channel map
 * COOTYPE    *x, *y;     output; location selected
 * char       *ch;        output; key pressed
 */


/*------------------------------------------------------------------------*/


void gc_set_outputdir( char dir[], STATUS *status );

/* sets scratch directory
 *
 * parameters of routine
 * char      dir[];     input; new directory name of scratch path
 * STATUS    *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void gc_set_inputdir( char dir[], STATUS *status );

/* sets input directory
 *
 * parameters of routine
 * char      dir[];     input; new directory name
 * STATUS    *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void gc_setpar( CHMAP map, char item[], char value[], STATUS *status );

/* sets channel specific parameters
 *
 * parameters of routine
 * CHMAP      map;       input; channel map
 * char       item[];    input; name of item to be changed
 * char       value[];   input; new value of item
 * STATUS     *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void gc_flushbuffers( void );

/* flushes all output buffers
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/


void gc_update( void );

/* updates window content
 * no parameters
 */


/*------------------------------------------------------------------------*/


void gc_playback( CHMAP src, CHMAP dst, char outf[], STATUS *status );

/* copies content of channel src into channel dst
 *
 * parameters of routine
 * CHMAP      src;     input; source channel
 * CHMAP      dst;     input; destination channel
 * char       outf[];  output; output filename
 * STATUS     *status; output; return status
 */


/*------------------------------------------------------------------------*/


void gc_set_write_extern( void (*wr_rout)(char text[]) );

/* sets the write routine for the external channel
 *
 * parameters of routine
 * void       (*wr_rout)(char text[]);    input; output routine
 */


/*------------------------------------------------------------------------*/


void gc_closeplot( CHMAP map, char outf[], STATUS *status );

/* closes plot file on hardcopy channels
 *
 * parameters of routine
 * CHMAP      map;         input; channel map
 * char       outf[];      output; name of plotfile
 * STATUS     *status;     output; return status
 */


/*------------------------------------------------------------------------*/


void gc_swap( BOOLEAN on_off );

/* switches x-y swapping on or off
 *
 * parameters of routine
 * BOOLEAN    on_off;    input; TRUE=on, FALSE=off
 */


/*------------------------------------------------------------------------*/

#ifdef BC_KS_PRIVATE
#ifdef BC_G_GEM
void gm_pixhardcopy( int wdw, int mag, char outfile[] );
#define gc_pixhardcopy(w,m,f) gm_pixhardcopy(w&GCF_NUMMASK,m,f)
#else
#define gc_pixhardcopy(w,m,f)
#endif
#endif

 /*------------------------------------------------------------------------*/

#endif  /* __GCUSRDEF */
