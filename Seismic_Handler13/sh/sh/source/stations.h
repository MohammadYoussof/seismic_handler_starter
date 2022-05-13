
/* file stations.h
 *      ==========
 *
 * version 10, 2-Feb-2007
 *
 * Define station lists
 * K. Stammler, 11-Jul-2001
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


#define STC_GRSN_ALL 31
char *stv_grsn_all[] = {
	"bfo",  "brg",  "brnl", "bseg", "bug",  "cll",  "clz",  "fbe",
	"fur",  "gec2", "grfo", "gsh",  "gttg", "gunz", "ham",  "hlg",
	"ibbn", "lid",  "manz", "mox",  "neub", "nrdl", "nott", "pst",
	"rgn",  "rotz", "rue",  "stu",  "tann", "tns",  "ubba", "werd",
	"wern", "wet",  ""
};

#define STC_GRF 13
char *stv_grf[] = {
	"gra1", "gra2", "gra3", "gra4",
	"grb1", "grb2", "grb3", "grb4", "grb5",
	"grc1", "grc2", "grc3", "grc4",
	""
};

#define STC_GMS 9
char *stv_gms[] = {
	"gms1", "gms2", "gms3",
	"gms4", "gms5", "gms6",
	"gms7", "gms8", "gms9",
	""
};

#define STC_YANQING 9
char *stv_yanqing[] = {
	"fykt", "ssht", "xhtt", "xyft", "yjbt", "dhtt", "dnht", "fjct", "hskt",
	""
};

#define STC_GEORGIA 12
char *stv_georgia[] = {
	"akha", "ambr", "bech", "chal", "chkv", "deli", "dush", "garj", "gori",
	"khel", "mtda", "oni",
	""
};
