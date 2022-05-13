
/* file VWCH.C
 *      ======
 *
 * version 4, 30-Sep-92
 *
 * VWS graphics interface for GRAPHCH.C
 * K. Stammler, 1-SEP-1990
 */


#include <stdio.h>
#include <string.h>
#include <uisentry.h>
#include "uisusrdef_gcc.h"
#include <descrip.h>
#include BASECNST
#include BC_SYSBASE
#include "vwusrdef.h"
#include "vwerrors.h"


/* constants */
#define STRLTH 80
   /* length of strings */
#define MAXDRAWLTH 0x00007fffL
   /* maximum number of points to be plotted in a uis$plot_array call */
#define DEFAULTWIDTH 100.0
   /* default user coo width */
#define DEFAULTHEIGHT 100.0
   /* default user coo height */
#define MAXDEFSTYLE 6
   /* default line styles */
#define NUMMASK 0x07


/* global variables */
static char         vwv_outputdir[BC_FILELTH+1];  /* output directory */
static BOOLEAN      vwv_dspinit={FALSE};     /* Display initialised */
static int          vwv_vcmsize;             /* size of colour map */
static int          vwv_dsp[VWC_MAXWDW];     /* Display identifier */
static int          vwv_vcm;                 /* virtual colour map identifier */
static int          vwv_wdw[VWC_MAXWDW];     /* window identifier */
static int          vwv_trf[VWC_MAXWDW];     /* transformation ID's */
static int          vwv_ison=0;              /* window open map */
static int          vwv_laststyle;           /* last used style block (text) */
static VWCOO        vwv_gxpos[VWC_MAXWDW];   /* graphics x-position */
static VWCOO        vwv_gypos[VWC_MAXWDW];   /* graphics y-position */
static BOOLEAN      vwv_arrayswap=FALSE;     /* swap arrayplot coo's */
static int          vwv_defstyle[MAXDEFSTYLE] =
   { 0xffffffff, 0xffffff00, 0xffff0000, 0xff00ff00, 0xf0f0f0f0, 0xff000000 };
static struct dsc$descriptor_s vwv_wdwnamedsc[VWC_MAXWDW]; /* window names */
static char         vwv_wdwname[VWC_MAXWDW][STRLTH+1] /* window names */
   = { "Window 0", "Window 1", "Window 2", "Window 3", "Window 4",
       "Window 5", "Window 6" };

/* macros */
#define vwh_illegalwdw(w) ((w) >= VWC_MAXWDW)
   /* illegal window number */
#define vwh_wdwopen(w) (((w) < VWC_MAXWDW) && ((1 << (w)) & vwv_ison))
   /* window w is opened */


/* prototypes of local routines */
void vwh_create_hcname( char name[] );


/*----------------------------------------------------------------------------*/



void vw_init( VWDW wdw, int attrib, float xlo, float ylo, float width,
   float height, STATUS *status )

/* initialises VWS graphics if necessary and opens window
 *
 * parameters of routine
 * VWDW       wdw;           input; window number
 * int        attribs;       input; window attributes
 * float      xlo, ylo;      input; position of window (in cm)
 * float      width, height; input; size of window (in cm)
 * STATUS     *status;       output; return status
 */
{
   /* local variables */
   float    r, r1, r2;         /* scratch */
   int      i;                 /* scratch */
   int      zero=0;
   $DESCRIPTOR( str, "SYS$WORKSTATION" );
   struct _wdwattrib {
      int   code_xpos;
      float xpos;
      int   code_ypos;
      float ypos;
      int   code_eol;
   }        wdwattrib;         /* window attributes */

   /* executable code */

   if  (!vwv_dspinit)  {
      uis$get_hw_color_info( &str, &i, &vwv_vcmsize, &i,&i,&i,&i,&i,&i,&i,&i );
      if  (vwv_vcmsize > 64)  {
         vwv_vcmsize = 60;
      } else {
         vwv_vcmsize -= 4;
         if  (vwv_vcmsize < 2)  vwv_vcmsize = 2;
      } /*endif*/
      vwv_vcm = uis$create_color_map( &vwv_vcmsize );
      vwv_dspinit = TRUE;
      /* setup window names */
      for  (i=0; i<VWC_MAXWDW; i++)  {
         vwv_wdwnamedsc[i].dsc$w_length = strlen( vwv_wdwname[i] );
         vwv_wdwnamedsc[i].dsc$b_dtype = DSC$K_DTYPE_T;
         vwv_wdwnamedsc[i].dsc$b_class = DSC$K_CLASS_S;
         vwv_wdwnamedsc[i].dsc$a_pointer = vwv_wdwname[i];
      } /*endfor*/
   } /*endif*/

   wdw &= NUMMASK;
   if  (vwh_illegalwdw(wdw))  {
      *status = VWE_ILWDW;
      return;
   } else if  ((1 << wdw) & vwv_ison)  {
      *status = VWE_WOPNTWICE;
      return;
   } /*endif*/

   r = 0.0;
   r1 = DEFAULTWIDTH; r2 = DEFAULTHEIGHT;
   vwv_dsp[wdw] = uis$create_display( &r, &r, &r1, &r2, &width, &height,
      &vwv_vcm );
   /* r = 1.0; r1 = 1.0, r2 = 0.5; i = 0; */ /* yellow background */
   r = 1.0; r1 = 1.0, r2 = 1.0; i = 0;  /* white background */
   uis$set_color( vwv_dsp+wdw, &i, &r, &r1, &r2 );
#  ifdef XXX
   r = 0.1; r1 = 0.4; r2 = 0.31; i = 1;
   uis$set_color( vwv_dsp+wdw, &i, &r, &r1, &r2 );
#  endif
   r = 0.05 * DEFAULTHEIGHT;
   for  (i=1; i<=VWC_MAXSTYLE; i++)
      uis$set_char_size( vwv_dsp+wdw, &zero, &i, NULL,NULL, &r );

   wdwattrib.code_xpos = WDPL$C_ABS_POS_X;
   wdwattrib.xpos = xlo;
   wdwattrib.code_ypos = WDPL$C_ABS_POS_Y;
   wdwattrib.ypos = ylo;
   wdwattrib.code_eol = WDPL$C_END_OF_LIST;
   vwv_wdw[wdw] = uis$create_window( vwv_dsp+wdw, &str,
      vwv_wdwnamedsc+wdw, NULL, NULL, NULL, NULL, NULL, NULL, &wdwattrib );
   vwv_trf[wdw] = vwv_dsp[wdw];
   vwv_ison |= (1 << wdw);

} /* end of vw_init */



/*----------------------------------------------------------------------------*/



void vw_exit( VWDW wdw )

/* closes window
 *
 * parameters of routine
 * VWDW       wdw;      input; window number
 */
{
   /* executable code */

   wdw &= NUMMASK;
   if  (!vwh_wdwopen(wdw))  return;
   if  (vwv_trf[wdw] != vwv_dsp[wdw])
      uis$delete_transformation( vwv_trf+wdw );
   uis$delete_window( vwv_wdw+wdw );
   uis$delete_display( vwv_dsp+wdw );
   vwv_ison &= ~(1 << wdw);

} /* end of vw_exit */



/*----------------------------------------------------------------------------*/



void vw_finish( void )

/* closes all windows
 *
 * no parameters
 */
{
   /* local variables */
   int      i;         /* counter */

   /* executable code */

   for  (i=0; i<VWC_MAXWDW; i++)
      if  (vwh_wdwopen(i))  {
         uis$delete_window( vwv_wdw+i );
         uis$delete_display( vwv_dsp+i );
      } /*endif*/
   vwv_ison = 0;

} /* end of vw_finish */



/*----------------------------------------------------------------------------*/



void vw_popwdw( VWDW wdw )

/* pops window on top
 *
 * parameters of routine
 * VWDW       wdw;      input; window number
 */
{
   /* executable code */

   wdw &= NUMMASK;
   if  (!vwh_wdwopen(wdw))  return;
   uis$pop_viewport( vwv_wdw+wdw );

} /* end of vw_popwdw */



/*----------------------------------------------------------------------------*/



void vw_pushwdw( VWDW wdw )

/* pushes window down
 *
 * parameters of routine
 * VWDW       wdw;      input; window number
 */
{
   /* executable code */

   wdw &= NUMMASK;
   if  (!vwh_wdwopen(wdw))  return;
   uis$push_viewport( vwv_wdw+wdw );

} /* end of vw_pushwdw */



/*----------------------------------------------------------------------------*/



void vw_resizewdw( VWDW wdw, float x, float y, float w, float h, STATUS *status)

/* resizes window
 *
 * parameters of routine
 * float      x, y;      input; new position of window
 * float      w, h;      input; width and height
 * STATUS     *status;   output; return status
 */
{
   /* executable code */

   wdw &= NUMMASK;
   if  (!vwh_wdwopen(wdw))  {
      *status = VWE_ILWDW;
      return;
   } /*endif*/
   uis$resize_window( vwv_dsp+wdw, vwv_wdw+wdw, &x, &y, &w, &h );

} /* end of vw_resizewdw */



/*----------------------------------------------------------------------------*/



void vw_erase( VWDW wdw )

/* clears window
 *
 * parameters of routine
 * VWDW       wdw;       input; window number
 */
{
   /* executable code */

   wdw &= NUMMASK;
   if  (!vwh_wdwopen(wdw))  return;
   uis$erase( vwv_trf+wdw );

} /* end of vw_erase */



/*----------------------------------------------------------------------------*/



void vw_setwdwname( VWDW wdw, char name[] )

/* sets new window name
 *
 * parameters of routine
 * VWDW       wdw;      input; window number
 * char       name[];   input; new window name
 */

/* !!! not supported in VWS !!! */

{
   /* local variables */
   int      lth;      /* string length */

   /* executable code */

   wdw &= NUMMASK;
   if  (!vwh_wdwopen(wdw))  return;
   lth = strlen(name);
   if  (lth > STRLTH)  lth = STRLTH;
   strcpy( vwv_wdwname[wdw], name );
   vwv_wdwnamedsc[wdw].dsc$w_length = lth;

} /* end of vw_setwdwname */



/*----------------------------------------------------------------------------*/



void vw_setcoo( VWDW wdw, VWCOO x, VWCOO y, VWCOO w, VWCOO h,
   STATUS *status )

/* sets user coordinates
 *
 * parameters of routine
 * VWDW       wdw;       input; window number
 * VWCOO      x, y;      input; origin of user coordinates
 * VWCOO      w, h;      input; size of user coordinates
 * STATUS     *status;   output; return status
 */
{
   /* executable code */

   wdw &= NUMMASK;
   if  (!vwh_wdwopen(wdw))  {
      *status = VWE_ILWDW;
      return;
   } /*endif*/
   if  (vwv_trf[wdw] != vwv_dsp[wdw])
      uis$delete_transformation( vwv_trf+wdw );
   w += x; h += y;
   vwv_trf[wdw] = uis$create_transformation( vwv_dsp+wdw, &x, &y, &w, &h );

} /* end of vw_setcoo */



/*----------------------------------------------------------------------------*/



void vw_moveto( VWDW wdw, VWCOO x, VWCOO y )

/* moves the graphic position to (x,y)
 *
 * parameters of routine
 * VWDW       wdw;       input; window number
 * VWCOO      x, y;      input; new position (user coordinates)
 */
{
   /* executable code */

   wdw &= NUMMASK;
   if  (vwh_wdwopen(wdw))  {
      vwv_gxpos[wdw] = x;
      vwv_gypos[wdw] = y;
   } /*endif*/

} /* end of vw_moveto */


/*----------------------------------------------------------------------------*/



void vw_drawto( VWDW wdw, int style, VWCOO x, VWCOO y )

/* draws to the graphic position (x,y)
 *
 * parameters of routine
 * VWDW       wdw;       input; window number
 * int        style;     input; style block number
 * VWCOO      x, y;      input; new position (user coordinates)
 */
{
   /* executable code */

   wdw &= NUMMASK;
   if  (!vwh_wdwopen(wdw))  return;
   if  ((style < 0) || (style >= VWC_MAXSTYLE))  style = 0;
   style++;
   uis$plot( vwv_trf+wdw, &style, vwv_gxpos+wdw, vwv_gypos+wdw, &x, &y );
   vwv_gxpos[wdw] = x;
   vwv_gypos[wdw] = y;

} /* end of vw_drawto */


/*----------------------------------------------------------------------------*/



void vw_arrayplot( VWDW wdw, int style, long cnt, int red, VWCOO xoff,
   VWCOO xinc, VWCOO yoff, VWCOO yarr[], float yzoom, STATUS *status )

/* plots an array of data
 *
 * parameters of routine
 * VWDW       wdw;       input; window number
 * int        style;     input; style block number
 * long       cnt;       input; length of data array
 * int        red;       input; reduction factor
 * VWCOO      xoff;      input; x-position of first sample
 * VWCOO      xinc;      input; x increment
 * VWCOO      yoff;      input; y-position of first sample
 * VWCOO      yarr[];    input; data array
 * float      yzoom;     input; zoom factor in y-direction
 * STATUS     *status;   output; return status
 */
{
   /* local variables */
   long     drawlth;       /* number of samples to be plotted */
   long     drawcnt;       /* number of sample already plotted */
   float    *xvec, *yvec;  /* plot arrays */
   float    *xcur, *ycur;  /* moving pointers */
   VWCOO    *yptr;         /* pointer to current y-value */
   long     i;             /* sample counter */

   /* executable code */

   wdw &= NUMMASK;
   if  (!vwh_wdwopen(wdw))  {
      *status = VWE_ILWDW;
      return;
   } /*endif*/
   if  (cnt == 0L)  return;
   if  (red <= 0)  return;
   if  ((style < 0) || (style >= VWC_MAXSTYLE))  style = 0;
   style++;

   drawlth = cnt / red;
   if  ((red > 1) && ((cnt % red) != 0))
      drawlth++;

   xvec = (float *)sy_allocmem( drawlth, sizeof(float), status );
   if  (Severe(status)) return;
   yvec = (float *)sy_allocmem( drawlth, sizeof(float), status );
   if  (Severe(status))  {
      sy_deallocmem( xvec );
      return;
   } /*endif*/

   yptr = yarr;
   xcur = xvec;
   ycur = yvec;
   for  (i=0; i<drawlth; i++)  {
      *xcur++ = xoff + xinc * (VWCOO)(red*i);
      *ycur++ = yoff + yzoom * (*yptr);
      yptr += red;
   } /*endfor*/

   if  (drawlth > MAXDRAWLTH)  {
      xcur = xvec;
      ycur = yvec;
      drawcnt = 0L;
      do {
         i = ((drawlth-drawcnt) > MAXDRAWLTH) ? MAXDRAWLTH : (drawlth-drawcnt);
         if  (vwv_arrayswap)  {
            uis$plot_array( vwv_trf+wdw, &style, &i, ycur, xcur );
         } else {
            uis$plot_array( vwv_trf+wdw, &style, &i, xcur, ycur );
         } /*endif*/
         xcur += MAXDRAWLTH-1;
         ycur += MAXDRAWLTH-1;
         drawcnt += MAXDRAWLTH-1;
      } while (drawcnt < drawlth);
   } else {
      if  (vwv_arrayswap)  {
         uis$plot_array( vwv_trf+wdw, &style, &drawlth, yvec, xvec );
      } else {
         uis$plot_array( vwv_trf+wdw, &style, &drawlth, xvec, yvec );
      } /*endif*/
   } /*endif*/

   sy_deallocmem( xvec );
   sy_deallocmem( yvec );

} /* end of vw_arrayplot */


/*----------------------------------------------------------------------------*/



void vw_text( VWDW wdw, int style, VWCOO x, VWCOO y, char text[] )

/* writes text at position (x,y) in window wdw
 *
 * parameters of routine
 * VWDW       wdw;       input; window number
 * int        style;     input; style block number
 * VWCOO      x, y;      input; position of text
 * char       text[];    input; text to be printed
 */
{
   /* local variables */
   $DESCRIPTOR( str, "@" );
   float      charsize;       /* height of char */

   /* executable code */

   str.dsc$w_length = strlen( text );
   str.dsc$a_pointer = text;

   wdw &= NUMMASK;
   if  (!vwh_wdwopen(wdw))  return;
   if  ((style < 0) || (style >= VWC_MAXSTYLE))  style = 0;
   style++;
   uis$get_char_size( vwv_trf+wdw, &style, NULL, NULL, &charsize );
   y += charsize;
   uis$text( vwv_trf+wdw, &style, &str, &x, &y );

   vwv_laststyle = style;

} /* end of vw_text */



/*----------------------------------------------------------------------------*/



void vw_setstyle( VWDW wdw, int style, char item[], char value[],
   STATUS *status )

/* sets attributes in attribute block
 *
 * parameters of routine
 * VWDW       wdw;       input; window number
 * int        style;     input; style block number
 * char       item[];    input; attribute name
 * char       value[];   input; new value (as string)
 * STATUS     *status;   output; return status
 */
{
   /* local variables */
   int      i;               /* integer var */
   float    r, g, b;         /* real var */
   float    wx, wy, ww, wh;  /* window pos & size in user coo's */
   float    vw, vh;

   /* executable code */

   wdw &= NUMMASK;
   if  ((style < 0) || (style >= VWC_MAXSTYLE))  {
      *status = VWE_ILSTYLE;
      return;
   } else if  (!vwh_wdwopen(wdw))  {
      /* *status = VWE_ILWDW; */
      return;
   } /*endif*/
   style++;

   if  (strcmp(item,"FONT") == 0)  {
      ;
   } else if  ((strcmp(item,"CHARSIZE")==0) || (strcmp(item,"CHARHEIGHT")==0)) {
      if  (sscanf(value,"%f",&r) != 1)  {
         *status = VWE_ILVALUE;
         return;
      } /*endif*/
      uis$get_window_size( vwv_trf+wdw, vwv_wdw+wdw, &wx, &wy, &ww, &wh );
      ww -= wx; wh -= wy;
      uis$get_viewport_size( vwv_wdw+wdw, &vw, &vh );
      r *= 2.0*wh;
      g = 0.5316901 * r/wh*ww *vh/vw;
      uis$set_char_size( vwv_trf+wdw, &style, &style, NULL, &g, &r );
   } else if  (strcmp(item,"LINEWIDTH") == 0)  {
      if  (sscanf(value,"%f",&r) != 1)  {
         *status = VWE_ILVALUE;
         return;
      } /*endif*/
      uis$set_line_width( vwv_trf+wdw, &style, &style, &r );
   } else if  (strcmp(item,"LINESTYLE") == 0)  {
      if  (sscanf(value,"%d",&i) != 1)  {
         *status = VWE_ILVALUE;
         return;
      } else if  ((i < 0) || (i >= MAXDEFSTYLE))  {
         *status = VWE_ILVALUE;
         return;
      } /*endif*/
      uis$set_line_style( vwv_trf+wdw, &style, &style, vwv_defstyle+i );
   } else if  (strcmp(item,"LINESTYLE_V") == 0)  {
      if  (sscanf(value,"%x",&i) != 1)  {
         *status = VWE_ILVALUE;
         return;
      } /*endif*/
      uis$set_line_style( vwv_trf+wdw, &style, &style, &i );
   } else if  (strcmp(item,"COLOR") == 0)  {
      if  (sscanf(value,"%f,%f,%f",&r,&g,&b) != 3)  {
         *status = VWE_ILVALUE;
         return;
      } /*endif*/
      if  (r < 0.0)  *status = VWE_ILVALUE;
      if  (r > 1.0)  *status = VWE_ILVALUE;
      if  (g < 0.0)  *status = VWE_ILVALUE;
      if  (g > 1.0)  *status = VWE_ILVALUE;
      if  (b < 0.0)  *status = VWE_ILVALUE;
      if  (b > 1.0)  *status = VWE_ILVALUE;
      if  (Severe(status))  return;
      i = style;
      if  (i < vwv_vcmsize)  {
         uis$set_color( vwv_dsp+wdw, &i, &r, &g, &b );
         uis$set_writing_index( vwv_dsp+wdw, &style, &style, &i );
      } /*endif*/
   } else if  (strcmp(item,"COLOR_VWS") == 0)  {
      if  (sscanf(value,"%d,%f,%f,%f",&i,&r,&g,&b) == 4)  {
         if  (i < 0)  *status = VWE_ILVALUE;
         if  (i >= vwv_vcmsize)  *status = VWE_ILVALUE;
         if  (r < 0.0)  *status = VWE_ILVALUE;
         if  (r > 1.0)  *status = VWE_ILVALUE;
         if  (g < 0.0)  *status = VWE_ILVALUE;
         if  (g > 1.0)  *status = VWE_ILVALUE;
         if  (b < 0.0)  *status = VWE_ILVALUE;
         if  (b > 1.0)  *status = VWE_ILVALUE;
         if  (Severe(status))  return;
         uis$set_color( vwv_dsp+wdw, &i, &r, &g, &b );
      } else if  (sscanf(value,"%d",&i) != 1)  {
         *status = VWE_ILVALUE;
         return;
      } /*endif*/
      if  (i < 0)  *status = VWE_ILVALUE;
      if  (i >= vwv_vcmsize)  *status = VWE_ILVALUE;
      uis$set_writing_index( vwv_dsp+wdw, &style, &style, &i );
   } else if  (strcmp(item,"WRMODE") == 0)  {
      if  (strcmp(value,"REPLACE") == 0)  {
         i = UIS$C_MODE_REPL;
      } else if  (strcmp(value,"XOR") == 0)  {
         i = UIS$C_MODE_COMP;
      } else {
         *status = VWE_ILVALUE;
         return;
      } /*endif*/
      uis$set_writing_mode( vwv_dsp+wdw, &style, &style, &i );
   } else {
      *status = VWE_ILSTYATT;
      return;
   } /*endif*/

} /* end of vw_setstyle */



/*----------------------------------------------------------------------------*/



void vw_charsize( VWDW wdw, int style, float size, STATUS *status )

/* sets character size in units of display height
 *
 * parameters of routine
 * VWDW       wdw;       input; window ID
 * int        style;     input; style number
 * float      size;      input; size of character in units of display height
 * STATUS     *status;   output; return status
 */
{
   /* local variables */
   float    wx, wy, ww, wh;   /* window size & pos */
   float    r, vw, vh;

   /* executable code */

   wdw &= NUMMASK;
   if  ((style < 0) || (style >= VWC_MAXSTYLE))  {
      *status = VWE_ILSTYLE;
      return;
   } /*endif*/
   style++;

   uis$get_window_size( vwv_trf+wdw, vwv_wdw+wdw, &wx, &wy, &ww, &wh );
   ww -= wx; wh -= wy;
   uis$get_viewport_size( vwv_wdw+wdw, &vw, &vh );
   size *= 2.0*wh;
   r = 0.5316901 * size/wh*ww *vh/vw;
   uis$set_char_size( vwv_trf+wdw, &style, &style, NULL, &r, &size );

} /* end of vw_charsize */



/*----------------------------------------------------------------------------*/



void vw_linestyle( VWDW wdw, int style, int linestyle, STATUS *status )

/* sets line linestyle in style block number "style"
 *
 * parameters of routine
 * VWDW       wdw;        input; window ID
 * int        style;      input; style block number
 * int        linestyle;  input; number of line style
 * STATUS     *status;    output; return status
 */
{
   /* executable code */

   wdw &= NUMMASK;
   if  ((style < 0) || (style >= VWC_MAXSTYLE))  {
      *status = VWE_ILSTYLE;
      return;
   } else if  ((linestyle < 0) || (linestyle >= MAXDEFSTYLE))  {
      *status = VWE_ILVALUE;
      return;
   } /*endif*/
   style++;

   uis$set_line_style( vwv_trf+wdw, &style, &style, vwv_defstyle+linestyle );

} /* end of vw_linestyle */



/*----------------------------------------------------------------------------*/



void vw_linewidth( VWDW wdw, int style, int width, STATUS *status )

/* sets line width in pixels in style block "style"
 *
 * parameters of routine
 * VWDW       wdw;      input; window number
 * int        style;    input; style block number
 * int        width;    input; width of line in pixels
 * STATUS     *status;  output; return status
 */
{
   /* local variables */
   float    w;

   /* executable code */

   wdw &= NUMMASK;
   if  ((style < 0) || (style >= VWC_MAXSTYLE))  {
      *status = VWE_ILSTYLE;
      return;
   } /*endif*/
   style++;

   w = (float)width;
   uis$set_line_width( vwv_trf+wdw, &style, &style, &w );

} /* end of vw_linewidth */



/*----------------------------------------------------------------------------*/



void vw_color( VWDW wdw, int style, float red, float green, float blue,
   STATUS *status )

/* sets color of style block "style"
 *
 * parameters of routine
 * VWDW       wdw;       input; window ID
 * int        style;            input; style block number
 * float      red, green, blue; input; red, green and blue intensities (0..1)
 * STATUS     *status;          output; return status
 */
{
   /* local variables */
   int      colentry;      /* color entry */

   /* executable code */

   wdw &= NUMMASK;
   if  ((style < 0) || (style >= VWC_MAXSTYLE))  {
      *status = VWE_ILSTYLE;
      return;
   } /*endif*/
   style++;

   if  (red < 0.0)  *status = VWE_ILVALUE;
   if  (red > 1.0)  *status = VWE_ILVALUE;
   if  (green < 0.0)  *status = VWE_ILVALUE;
   if  (green > 1.0)  *status = VWE_ILVALUE;
   if  (blue < 0.0)  *status = VWE_ILVALUE;
   if  (blue > 1.0)  *status = VWE_ILVALUE;
   if  (Severe(status))  return;
   colentry = style;
   if  (colentry >= vwv_vcmsize)  return;
   uis$set_color( vwv_dsp+wdw, &colentry, &red, &green, &blue );
   uis$set_writing_index( vwv_dsp+wdw, &style, &style, &colentry );

} /* end of vw_color */



/*----------------------------------------------------------------------------*/



void vw_set_outputdir( char dir[], STATUS *status )

/* sets scratch directory
 *
 * parameters of routine
 * char      dir[];     input; new directory name of scratch path
 * STATUS    *status;   output; return status
 */
{
	/* executable code */

	if  (strlen(dir) > BC_FILELTH)  {
		*status = VWE_STROVFL;
		return;
	} /*endif*/
	strcpy( vwv_outputdir, dir );

} /* end of vw_set_outputdir */



/*----------------------------------------------------------------------------*/



void vw_getloc( VWDW wdw, VWCOO *x, VWCOO *y, char *ch )

/* returns location selected by user
 *
 * parameters of routine
 * VWDW       wdw;      input; window number
 * VWCOO      *x, *y;   output; location selected
 * char       *ch;      output; key pressed
 */
{
   /* local variables */
   int      ascii;            /* ascii code read */
   int      atb;              /* attribute block */
   BOOLEAN  visible;          /* mouse in window */
   BOOLEAN  char_received;    /* character read */
   BOOLEAN  cross_shown;      /* crosshair cursor on */
   float    xl, yl, xh, yh;   /* window pos & size */
   float    xc, yc;           /* cursor coo */
   int      i;                /* scratch */
   $DESCRIPTOR( chdescr, " " );
   short    cursor_map[] =
      {256,256,256,256,256,256,256,65535,256,256,256,256,256,256,256,256};
   short    cursor_full[] =
      {65535,65535,65535,65535,65535,65535,65535,65535,65535,65535,65535,
      65535,65535,65535,65535,65535};

   /* executable code */

   wdw &= NUMMASK;
   if  (!vwh_wdwopen(wdw))  return;

   atb = VWC_MAXSTYLE+1;
   ascii = 8;
   uis$set_pointer_pattern( vwv_trf+wdw, vwv_wdw+wdw, cursor_map, NULL,
      &ascii, &ascii );
   sys_read_init();
   ascii = 0;
   i = UIS$C_MODE_COMP;
   uis$set_writing_mode( vwv_dsp+wdw, &ascii, &atb, &i );
   uis$get_window_size( vwv_trf+wdw, vwv_wdw+wdw, &xl, &yl, &xh, &yh );
   cross_shown = FALSE;
   visible = FALSE;
   while  (!visible)  {
      char_received = FALSE;
      while  (!char_received)  {
         if  (uis$get_pointer_position(vwv_trf+wdw,vwv_wdw+wdw,x,y))  {
            if  (cross_shown)  {
               if  ((*x != xc) || (*y != yc))  {
                  uis$plot( vwv_trf+wdw, &atb, &xl, &yc, &xh, &yc );
                  uis$plot( vwv_trf+wdw, &atb, &xc, &yl, &xc, &yh );
                  xc = *x; yc = *y;
                  uis$plot( vwv_trf+wdw, &atb, &xl, &yc, &xh, &yc );
                  uis$plot( vwv_trf+wdw, &atb, &xc, &yl, &xc, &yh );
               } /*endif*/
            } else {
               xc = *x; yc = *y;
               uis$plot( vwv_trf+wdw, &atb, &xl, &yc, &xh, &yc );
               uis$plot( vwv_trf+wdw, &atb, &xc, &yl, &xc, &yh );
               cross_shown = TRUE;
            } /*endif*/
         } /*endif*/
         sys_busy_read( &chdescr, &char_received );
      } /*endwhile*/
      visible = uis$get_pointer_position( vwv_trf+wdw, vwv_wdw+wdw, x, y );
   } /*endwhile*/
   sys_read_finish();

   if  (cross_shown)  {
      uis$plot( vwv_trf+wdw, &atb, &xl, &yc, &xh, &yc );
      uis$plot( vwv_trf+wdw, &atb, &xc, &yl, &xc, &yh );
   } /*endif*/
   ascii = 8;
   uis$set_pointer_pattern( vwv_trf+wdw, vwv_wdw+wdw, cursor_full, NULL,
      &ascii, &ascii );
   xc = 0.05;
   lib$wait( &xc );
   uis$set_pointer_pattern( vwv_trf+wdw, vwv_wdw+wdw );

   *ch = *(char *)chdescr.dsc$a_pointer;

} /* end of vw_getloc */



/*----------------------------------------------------------------------------*/



void vw_prepare( STATUS *status )

/* prepares hardcopy
 *
 * parameters of routine
 * STATUS     *status;    output; return status
 */
{

} /* end of vw_prepare */



/*----------------------------------------------------------------------------*/



void vw_cleanup( VWDW wdw, char outf[], STATUS *status )

/* makes hardcopy of window wdw
 *
 * parameters of routine
 * VWDW       wdw;        input; window number
 * char       outf[];     output; name of output file
 * STATUS     *status;    output; return status
 */
{
   /* local variables */
   char     hcname[STRLTH+1];    /* name of hardcopy file */
   $DESCRIPTOR( sdsc, " " );     /* UIS string parameter */

   /* executable code */

   wdw &= NUMMASK;
   /* create hardcopy name */
   vwh_create_hcname( hcname );

   /* write UIS file */
   strcpy( outf, vwv_outputdir );
   strcat( outf, hcname );
   sdsc.dsc$w_length = strlen( outf );
   sdsc.dsc$a_pointer = outf;
   hcuis$write_display( vwv_trf+wdw, &sdsc );

} /* end of vw_cleanup */



/*----------------------------------------------------------------------------*/



void vw_setpar( char item[], char value[], STATUS *status )

/* sets VWS parameters
 *
 * parameters of routine
 * char       item[];     input; item to be set
 * char       value[];    input; new value of item
 * STATUS     *status;    output; return status
 */
{
   /* executable code */

   if  (strlen(value) > STRLTH)  {
      *status = VWE_STROVFL;
      return;
   } /*endif*/

   if  (strcmp(item,"JOB_QUEUE") == 0)  {
      /* strcpy( vwv_jobqueue, value ); */
   } else {
      *status = VWE_UKHCITEM;
      return;
   } /*endif*/

} /* end of vw_setpar */



/*----------------------------------------------------------------------------*/



float vw_aspectratio( VWDW wdw )

/* returns ratio of width to height
 *
 * parameters of routine
 * VWDW       wdw;      input; window number
 */
{
   /* local variables */
   float    width, height;   /* size of window in cm */

   /* executable code */

   wdw &= NUMMASK;
   if  (!vwh_wdwopen(wdw))  return 1.0;

   uis$get_viewport_size( vwv_wdw+wdw, &width, &height );
   return  width/height;

} /* end of vw_aspectratio */



/*----------------------------------------------------------------------------*/



VWCOO vw_chheight( VWDW wdw )

/* returns character height in user coordinates of last used style
 *
 * parameters of routine
 * VWDW       wdw;      input; window number
 */
{
   /* local variables */
   float    charsize;    /* char size returned */

   /* executable code */

   wdw &= NUMMASK;
   if  (!vwh_wdwopen(wdw))  return;
   uis$get_char_size( vwv_trf+wdw, &vwv_laststyle, NULL, NULL, &charsize );
   return charsize;

} /* end of vw_chheight */



/*----------------------------------------------------------------------------*/



void vw_arrayswap( BOOLEAN on_off )

/* switches arrayplot-swap on or off
 *
 * parameters of routine
 * BOOLEAN    on_off;    TRUE=on, FALSE=off
 */
{
   /* executable code */

   vwv_arrayswap = on_off;

} /* end of vw_arrayswap */



/*----------------------------------------------------------------------------*/



void vw_not_implemented( void )

/* dummy routine
 */
{
   /* executable code */

   printf( "   *** routine not implemented ! ***\n" );

} /* end of vw_not_implemented */



/*----------------------------------------------------------------------------*/



void vwh_create_hcname( char name[] )

/* creates hardcopy filename
 *
 * parameters of routine
 * char       name[];      output; filename created
 */
{
   /* local variables */
   static int   hc_count=0;   /* hardcopy counter */

   /* executable code */

   strcpy( name, "HC" );
   sprintf( name+2, "%03d", ++hc_count );
	strcat( name, ".UIS" );

} /* end of vwh_create_filename */



/*----------------------------------------------------------------------------*/
