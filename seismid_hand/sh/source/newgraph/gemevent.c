
/* file GEMEVENT.C
 *      ==========
 *
 * version 6, 21-May-92
 *
 * event manager (GEM)
 * K. Stammler, 1-AUG-1990
 */


#include <aes.h>
#include <vdi.h>
#include <tos.h>
#include <stdio.h>
#include <string.h>
#include <ext.h>
#include BASECNST
#include BC_SYSBASE
#include "graphbas.h"
#include "gmusrdef.h"
#include "shrsrc.h"

#ifdef BC_KS_PRIVATE
#ifdef MESSAGE_DEBUGGING
#include "e:\pure_c\util\mailbuf.h"
#endif /* MESSAGE_DEBUGGING */
#endif /* BC_KS_PRIVATE */


#define WAITFOR (MU_KEYBD|MU_MESAG|MU_TIMER)
#define KEYBUFLTH 128


/* global variables */
static int evv_rdflag=0;     /* resize redraw flag */
static int evv_rdwdw;        /* redraw window */
static int evv_lotimer=250;  /* timer event */
static int evv_hitimer=0;
static OBJECT *evv_menutree; /* menu tree */
static BOOLEAN evv_treeinit; /* object tree addresses initialised */
static char evv_keybuf[KEYBUFLTH]; /* key buffer */
static int evv_keyptr;       /* pointer to char in key buffer */


/* prototypes of local routines */
void ev_message( int msg[], char *ch, BOOLEAN *quit );
void ev_keyboard( int code, char *ch, BOOLEAN *quit );
void evh_aboutbox( void );
void evh_getfile( char file[] );


/*-------------------------------------------------------------------------*/



void ev_read_character( char *ch, BOOLEAN polling )

/* reads a single character from keyboard and handles events
 * if "polling" then no keyboard event is waited for.
 *
 * parameters of routine
 * char       *ch;       output; character read
 * BOOLEAN    polling;   input; if "FALSE", wait for keyboard event
 */
{
	/* local variables */
	int      events;      /* events occurred */
	int      mesbuf[16];  /* message buffer */
	int      mo_x, mo_y;  /* position of mouse */
	int      btn_state;   /* state of mouse button */
	int      kbd_state;   /* state of keyboard */
	int      key_code;    /* key code */
	int      btn_clicks;  /* number of button clicks */
	int      status=0;    /* scratch */
	int      lotim, hitim;/* local timer values */
	BOOLEAN  quit;        /* quit loop */

	/* executable code */

	if  (evv_keybuf[evv_keyptr] != '\0')  {
		*ch = evv_keybuf[evv_keyptr++];
		if  (evv_keybuf[evv_keyptr] == '\0')  {
			*evv_keybuf = '\0';
			evv_keyptr = 0;
		} /*endif*/
		return;
	} /*endif*/

	/* show mouse (reset) */
	graf_mouse( ARROW, NULL );
   v_show_c( gm_vdid(), 0 );

	if  (!evv_treeinit)  {
		if  (gm_rsrcloaded())  {
			rsrc_gaddr( R_TREE, RSRC_TREE_MENU, &evv_menutree );
		} else {
			evv_menutree = NULL;
		} /*endif*/
		evv_treeinit = TRUE;
	} /*endif*/

	if  (polling)  {
		quit = TRUE;
		lotim = hitim = 0;
		*ch = '\0';
	} else {
		quit = FALSE;
		lotim = evv_lotimer;
		hitim = evv_hitimer;
	} /*endif*/
	do  {

		/* wait for events */
		events = evnt_multi(
			WAITFOR,      /* wait for this events */
			1, 1, 1,      /* single clicks of left mouse button */
			0,0,0,0,0,    /* no mouse event 1 */
			0,0,0,0,0,    /* no mouse event 2 */
			mesbuf,       /* message buffer */
			lotim, hitim, /* timer */
			&mo_x, &mo_y, /* position of mouse */
			&btn_state,   /* state of mouse button */
			&kbd_state,   /* state of keyboard (Shift, Alternate, ...) */
			&key_code,    /* key code */
			&btn_clicks   /* number of button clicks */
		);

		if  (MU_MESAG & events)  {
		   v_hide_c( gm_vdid() );
			wind_update( BEG_UPDATE );
			ev_message( mesbuf, ch, &quit );
			wind_update( END_UPDATE );
		   v_show_c( gm_vdid(), 1 );
		} /*endif*/

		if  (MU_KEYBD & events)  {
		   v_hide_c( gm_vdid() );
			wind_update( BEG_UPDATE );
			ev_keyboard( key_code, ch, &quit );
			wind_update( END_UPDATE );
		   v_show_c( gm_vdid(), 1 );
		} /*endif*/

		if  (evv_rdflag > 0)  {        /* resize message received ...        */
			if  (evv_rdflag++ > 1)  {   /* ... and no redraw message received */
			   v_hide_c( gm_vdid() );
				wind_update( BEG_UPDATE );
				gm_redraw( gm_getwdw(evv_rdwdw), &status );
				evv_rdflag = 0;
				wind_update( END_UPDATE );
			   v_show_c( gm_vdid(), 1 );
			} /*endif*/
		} /*endif*/
	
	}  while  (!quit);

	/* hide mouse */
   v_hide_c( gm_vdid() );
	graf_mouse( HOURGLASS, NULL );

} /* end of ev_read_character */



/*-------------------------------------------------------------------------*/



void ev_message( int msg[], char *ch, BOOLEAN *quit )

/* handles message from GEM
 *
 * parameter of routine
 * int       msg[];    input; message
 * char      *ch;      output; character read
 * BOOLEAN   *quit;    output; quit event loop
 */
{
	/* local variables */
	static int currw[4]; /* to store current size */
	int      fullw[4];   /* full size */
	int      status=0;   /* return status */
	char     file[BC_FILELTH+1];  /* file selected */

	/* executable code */

#	ifdef BC_SHARE_CPU
#	ifdef MESSAGE_DEBUGGING
	mb_inccounter( 1 );
	mb_setcounter( 2, *msg );
#	endif
#	endif

	switch  (*msg)  {
	case MN_SELECTED:
		if  (msg[4] == RSRC_ENTRY_QUIT)  {
			strcpy( evv_keybuf, "QUIT Y\r" );
			*ch = evv_keybuf[evv_keyptr++];
			*quit = TRUE;
		} else if  (msg[4] == RSRC_ENTRY_GETFILE)  {
			evh_getfile( file );
			if  (*file != '\0')  {
				strcpy( evv_keybuf, file );
				*ch = evv_keybuf[evv_keyptr++];
				*quit = TRUE;
			} /*endif*/
		} else {
			evh_aboutbox();
		} /*endif*/
		menu_tnormal( evv_menutree, msg[3], TRUE );
		break;
	case WM_REDRAW:
		gm_redraw( gm_getwdw(msg[3]), &status );
		if  ((evv_rdflag > 0) && (evv_rdwdw == msg[3]))  /* redraw done */
			evv_rdflag = 0;
		break;
	case WM_TOPPED:
		gm_popwdw( gm_getwdw(msg[3]) );
		break;
	case WM_CLOSED:
		break;
	case WM_FULLED:
		if  (currw[2] == 0)  {  /* not full size */
			wind_get( msg[3], WF_CURRXYWH, currw, currw+1, currw+2, currw+3 );
			wind_get( msg[3], WF_FULLXYWH, fullw, fullw+1, fullw+2, fullw+3 );
			wind_set( msg[3], WF_CURRXYWH, fullw[0], fullw[1], fullw[2], fullw[3] );
		} else {
			wind_set( msg[3], WF_CURRXYWH, currw[0], currw[1], currw[2], currw[3] );
			currw[2] = 0;
		} /*endif*/
		gm_setcoo( gm_getwdw(msg[3]), 0.0, 0.0, 0.0, 0.0, &status );
		evv_rdwdw = msg[3];
		evv_rdflag = 1;  /* redraw window, but wait for a  */
		break;           /* possibly coming redraw message */
	case WM_SIZED:
	   wind_set( msg[3], WF_CURRXYWH, msg[4], msg[5], msg[6], msg[7] );
		gm_setcoo( gm_getwdw(msg[3]), 0.0, 0.0, 0.0, 0.0, &status );
		evv_rdwdw = msg[3];
		evv_rdflag = 1;  /* redraw window, but wait for a  */
		break;           /* possibly coming redraw message */
	case WM_MOVED:
		wind_get( msg[3], WF_CURRXYWH, fullw, fullw+1, fullw+2, fullw+3 );
	   wind_set( msg[3], WF_CURRXYWH, msg[4], msg[5], msg[6], msg[7] );
		gm_pixinc_write( gm_getwdw(msg[3]), msg[4]-fullw[0], msg[5]-fullw[1] );
		gm_setcoo( gm_getwdw(msg[3]), 0.0, 0.0, 0.0, 0.0, &status );
		break;
	case WM_NEWTOP:
		break;
	case 53:
		printf( "\n***\n*** got message !!! ***\n***\n" );
		break;
	} /*endswitch*/

} /* end of ev_message */



/*-------------------------------------------------------------------------*/



void ev_keyboard( int code, char *ch, BOOLEAN *quit )

/* handles keyboard event
 *
 * parameters of event
 * int        code;    input; key code
 * char       *ch;     output; ascii code (character read)
 * BOOLEAN    *quit;   output; leave event handler (after valid char)
 */
{
	/* executable code */

	*ch = (char)code;

	if  (*ch == 0x17)  {  /* Ctrl-W */
		gm_popwdw( GBC_CYCLEWDW );
		*quit = FALSE;
	} else {
		*quit = TRUE;
	} /*endif*/

} /* end of ev_keyboard */



/*-------------------------------------------------------------------------*/



void evh_aboutbox( void )

/* displays the About SH box
 *
 * no parameters
 */
{
	/* local variables */
	OBJECT   *about;     /* about box */
	int      x, y, w, h; /* box coo's */

	/* executable code */

	rsrc_gaddr( R_TREE, RSRC_TREE_ABOUT, &about );
	form_center( about, &x, &y, &w, &h );
	form_dial( FMD_START, 0,0,0,0, x,y,w,h );
	form_dial( FMD_GROW, 0,0,0,0, x,y,w,h );
	/* objc_change( about, RSRC_BTN_ABOUTEX, 0, x,y,w,h */
	about[RSRC_BTN_ABOUTEX].ob_state &= ~SELECTED;
	objc_draw( about, 0, 2, x,y,w,h );
   v_show_c( gm_vdid(), 1 );
	form_do( about, 0 );
   v_hide_c( gm_vdid() );
	form_dial( FMD_SHRINK, 0,0,0,0, x,y,w,h );
	form_dial( FMD_FINISH, 0,0,0,0, x,y,w,h );

} /* end of evh_aboutbox */



/*-------------------------------------------------------------------------*/



void evh_getfile( char file[] )

/* returns filename from selector box
 *
 * parameter of routine
 * char      file[];     output; filename
 */
{
	/* local variables */
	static char  currpath[BC_FILELTH+1];  /* path */
	static char  str[BC_FILELTH+1];       /* selected file */
	int    btn;                           /* button */
	int    i;                             /* counter */

	/* executable code */

	if  (*currpath == '\0')  {
		Dgetpath( currpath+2, 0 );
		currpath[1] = ':';
		*currpath = Dgetdrv() + 'A';
		strcat( currpath, "\\*.*" );
	} /*endif*/

	*str = '\0';   /* name preset doesn't work */
   v_show_c( gm_vdid(), 1 );
	fsel_input( currpath, str, &btn );
   v_hide_c( gm_vdid() );
	if  (btn == 0)  {  /* cancel */
		*file = '\0';
	} else {
		i = (int)strlen( currpath ) - 1;
		while  (currpath[i] != '\\'  &&  i > 0)
			i--;
		if  (currpath[i] == '\\')  {
			strncpy( file, currpath, i+1 );
			strcat( file, str );
		} else {
			strcpy( file, str );
		} /*endif*/
	} /*endif*/

} /* end of evh_getfile */



/*-------------------------------------------------------------------------*/
