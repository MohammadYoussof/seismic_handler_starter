
/* file UIERRORS.H
 *      ==========
 *
 * version 3, 22-May-2006
 *
 * error codes of module USRINPW
 * K. Stammler, 22-MAY-91
 */


/*
 *
 *  SeismicHandler, seismic analysis software
 *  Copyright (C) 1996,  Klaus Stammler, Federal Institute for Geosciences
 *                                       and Natural Resources (BGR), Germany
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */


#ifndef __UIERRORS
#define __UIERRORS

#define UIE_NOERROR 0

#define UIE_OFFSET 1000
#define UIE_OPENFILE   (UIE_OFFSET+1)
#define UIE_WRITEFILE  (UIE_OFFSET+2)
#define UIE_READFILE   (UIE_OFFSET+3)
#define UIE_LEVOVFL    (UIE_OFFSET+4)
#define UIE_LEVUDRFL   (UIE_OFFSET+5)
#define UIE_NAMLTH     (UIE_OFFSET+6)
#define UIE_IAJ        (UIE_OFFSET+7)   /* goto in interactive mode */
#define UIE_LABNF      (UIE_OFFSET+8)   /* label not found */
#define UIE_STEPABORT  (UIE_OFFSET+9)   /* step aborted */
#define UIE_NOTIMPL    (UIE_OFFSET+10)  /* function not implemented */
#define UIE_HKFILE     (UIE_OFFSET+11)  /* hotkey from file */

#endif /* __UIERRORS */
