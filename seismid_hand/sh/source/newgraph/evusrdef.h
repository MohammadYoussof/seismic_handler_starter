
/* file EVUSRDEF.H
 *      ==========
 *
 * version 2, 31-AUG-91
 *
 * prototypes of module EVENTMGR.C
 * K. Stammler, 31-AUG91
 */


#ifndef __EVUSRDEF
#define __EVUSRDEF

/*-------------------------------------------------------------------------*/


void ev_read_character( char *ch, BOOLEAN polling );

/* reads a single character from keyboard and handles events
 *
 * parameters of routine
 * char       *ch;       output; character read
 * BOOLEAN    polling;   input; if "FALSE", wait for keyboard event
 */


/*-------------------------------------------------------------------------*/

#endif
