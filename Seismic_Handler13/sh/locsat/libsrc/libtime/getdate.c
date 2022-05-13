# define ID 257
# define MONTH 258
# define DAY 259
# define MERIDIAN 260
# define NUMBER 261
# define UNIT 262
# define MUNIT 263
# define SUNIT 264
# define ZONE 265
# define DAYZONE 266
# define AGO 267

# line 7 "getdate.y"
	/* 	Steven M. Bellovin (unc!smb)			*/
	/*	Dept. of Computer Science			*/
	/*	University of North Carolina at Chapel Hill	*/
	/*	@@(#)getdate.y	2.5	3/3/83	*/

#include <sys/types.h>
#ifdef USG
struct timeb
{
	time_t	time;
	unsigned short millitm;
	short	timezone;
	short	dstflag;
};
#else
#include <sys/timeb.h>
#endif
#include <ctype.h>
#include <sys/time.h>

#define	NULL	0
#define daysec (24L*60L*60L)
	static int timeflag, zoneflag, dateflag, dayflag, relflag;
	static time_t relsec, relmonth;
	static int hh, mm, ss, fs, merid, daylight;
	static int dayord, dayreq;
	static int month, day, year;
	static int ourzone;
#define AM 1
#define PM 2
#define DAYLIGHT 1
#define STANDARD 2
#define MAYBE    3
#ifdef __cplusplus
#  include <stdio.h>
#  include <yacc.h>
#endif	/* __cplusplus */ 
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif

/* __YYSCLASS defines the scoping/storage class for global objects
 * that are NOT renamed by the -p option.  By default these names
 * are going to be 'static' so that multi-definition errors
 * will not occur with multiple parsers.
 * If you want (unsupported) access to internal names you need
 * to define this to be null so it implies 'extern' scope.
 * This should not be used in conjunction with -p.
 */
#ifndef __YYSCLASS
# define __YYSCLASS static
#endif
#ifndef YYSTYPE
#define YYSTYPE int
#endif
YYSTYPE yylval;
__YYSCLASS YYSTYPE yyval;
typedef int yytabelem;
# define YYERRCODE 256

# line 117 "getdate.y"


static int mdays[12] =
	{31, 0, 31,  30, 31, 30,  31, 31, 30,  31, 30, 31};
#define epoch 1970

extern struct tm *localtime();
double dateconv(mm, dd, yr, h, m, s, f, mer, zone, dayflag)
int mm, dd, yr, h, m, s, f, mer, zone, dayflag;
{
	double tod;
	double jdate;
	register int i;
	double timeconv();

	if (yr < 0) yr = -yr;
	if (yr < 100) yr += 1900;
	mdays[1] = 28 + (yr%4 == 0);
	if (/* yr < epoch || */ yr > 1999 || mm < 1 || mm > 12 ||
		dd < 1 || dd > mdays[--mm]) return (-1);
	jdate = dd-1;
        for (i=0; i<mm; i++) jdate += mdays[i];
	if (yr >= epoch)
	{
		for (i = epoch; i < yr; i++)
			jdate += 365 + (i%4 == 0);
	}
	else
	{
		for (i = epoch; i > yr; i--)
			jdate -= (365 + (i%4 == 0));
	}
	jdate *= daysec;
	jdate += zone * 60L;
	if (dayflag==DAYLIGHT || (dayflag==MAYBE&&localtime(&jdate)->tm_isdst))
		jdate += -1*60*60;
	if ((tod = timeconv(h, m, s, f, mer)) < 0) return (-1);
	return (jdate + tod);
}

double dayconv(ord, day, now) int ord, day; double now;
{
	register struct tm *loctime;
	double tod;
	double daylcorr();

	tod = now;
	loctime = localtime(&tod);
	tod += daysec * ((day - loctime->tm_wday + 7) % 7);
	tod += 7*daysec*(ord<=0?ord:ord-1);
	return daylcorr(tod, now);
}

double timeconv(hh, mm, ss, fs, mer) register int hh, mm, ss, fs, mer;
{
	double	frac;
	
	if (mm < 0 || mm > 59 || ss < 0 || ss > 59) return (-1);

	frac = fs;
	while (frac > 1.0)
		frac /= 10.0;

	switch (mer) {
		case AM: if (hh < 1 || hh > 12) return(-1);
			 return ((60L * ((hh%12)*60L + mm)+ss) + frac);
		case PM: if (hh < 1 || hh > 12) return(-1);
			 return ((60L * ((hh%12 +12)*60L + mm)+ss) + frac);
		case 24: if (hh < 0 || hh > 23) return (-1);
			 return ((60L * (hh*60L + mm)+ss) + frac);
		default: return (-1);
	}
}
double monthadd(sdate, relmonth) double sdate; time_t relmonth;
{
	struct tm *ltime;
	double  dateconv();
	double daylcorr();
	int mm, yr;

	if (relmonth == 0) return 0;
	ltime = localtime(&sdate);
	mm = 12*ltime->tm_year + ltime->tm_mon + relmonth;
	yr = mm/12;
	mm = mm%12 + 1;
	return daylcorr(dateconv(mm, ltime->tm_mday, yr, ltime->tm_hour,
		ltime->tm_min, ltime->tm_sec, 0, 24, ourzone, MAYBE), sdate);
}

double daylcorr(future, now) double future, now;
{
	int fdayl, nowdayl;

	nowdayl = (localtime(&now)->tm_hour+1) % 24;
	fdayl = (localtime(&future)->tm_hour+1) % 24;
	return (future-now) + 60L*60L*(nowdayl-fdayl);
}

static char *lptr;

yylex()
{
	extern int yylval;
	int sign;
	register char c;
	register char *p;
	char idbuf[20];
	int pcnt;

	for (;;) {
		while (isspace(*lptr)) lptr++;

		if (isdigit(c = *lptr) || c == '-' || c == '+') {
			if (c== '-' || c == '+') {
				if (c=='-') sign = -1;
				else sign = 1;
				if (!isdigit(*++lptr)) {
					/* yylval = sign; return (NUMBER); */
					return yylex();	/* skip the '-' sign */
				}
			} else sign = 1;
			yylval = 0;
			while (isdigit(c = *lptr++)) yylval = 10*yylval + c - '0';
			yylval *= sign;
			lptr--;
			return (NUMBER);

		} else if (isalpha(c)) {
			p = idbuf;
			while (isalpha(c = *lptr++) || c=='.')
				*p++ = c;
			*p = '\0';
			lptr--;
			return (lookup(idbuf));
		}

		else if (c == '(') {
			pcnt = 0;
			do {
				c = *lptr++;
				if (c == '\0') return(c);
				else if (c == '(') pcnt++;
				else if (c == ')') pcnt--;
			} while (pcnt > 0);
		}

		else return (*lptr++);
	}
}

struct table {
	char *name;
	int type, value;
};

struct table mdtab[] = {
	{"January", MONTH, 1},
	{"February", MONTH, 2},
	{"March", MONTH, 3},
	{"April", MONTH, 4},
	{"May", MONTH, 5},
	{"June", MONTH, 6},
	{"July", MONTH, 7},
	{"August", MONTH, 8},
	{"September", MONTH, 9},
	{"Sept", MONTH, 9},
	{"October", MONTH, 10},
	{"November", MONTH, 11},
	{"December", MONTH, 12},

	{"Sunday", DAY, 0},
	{"Monday", DAY, 1},
	{"Tuesday", DAY, 2},
	{"Tues", DAY, 2},
	{"Wednesday", DAY, 3},
	{"Wednes", DAY, 3},
	{"Thursday", DAY, 4},
	{"Thur", DAY, 4},
	{"Thurs", DAY, 4},
	{"Friday", DAY, 5},
	{"Saturday", DAY, 6},
	{0, 0, 0}};

#define HRS *60
#define HALFHR 30
struct table mztab[] = {
	{"a.m.", MERIDIAN, AM},
	{"am", MERIDIAN, AM},
	{"p.m.", MERIDIAN, PM},
	{"pm", MERIDIAN, PM},
	{"nst", ZONE, 3 HRS + HALFHR},		/* Newfoundland */
	{"n.s.t.", ZONE, 3 HRS + HALFHR},
	{"ast", ZONE, 4 HRS},		/* Atlantic */
	{"a.s.t.", ZONE, 4 HRS},
	{"adt", DAYZONE, 4 HRS},
	{"a.d.t.", DAYZONE, 4 HRS},
	{"est", ZONE, 5 HRS},		/* Eastern */
	{"e.s.t.", ZONE, 5 HRS},
	{"edt", DAYZONE, 5 HRS},
	{"e.d.t.", DAYZONE, 5 HRS},
	{"cst", ZONE, 6 HRS},		/* Central */
	{"c.s.t.", ZONE, 6 HRS},
	{"cdt", DAYZONE, 6 HRS},
	{"c.d.t.", DAYZONE, 6 HRS},
	{"mst", ZONE, 7 HRS},		/* Mountain */
	{"m.s.t.", ZONE, 7 HRS},
	{"mdt", DAYZONE, 7 HRS},
	{"m.d.t.", DAYZONE, 7 HRS},
	{"pst", ZONE, 8 HRS},		/* Pacific */
	{"p.s.t.", ZONE, 8 HRS},
	{"pdt", DAYZONE, 8 HRS},
	{"p.d.t.", DAYZONE, 8 HRS},
	{"yst", ZONE, 9 HRS},		/* Yukon */
	{"y.s.t.", ZONE, 9 HRS},
	{"ydt", DAYZONE, 9 HRS},
	{"y.d.t.", DAYZONE, 9 HRS},
	{"hst", ZONE, 10 HRS},		/* Hawaii */
	{"h.s.t.", ZONE, 10 HRS},
	{"hdt", DAYZONE, 10 HRS},
	{"h.d.t.", DAYZONE, 10 HRS},
	{"bst", ZONE, 11 HRS},		/* Bering */
	{"b.s.t.", ZONE, 11 HRS},
	{"bdt", DAYZONE, 11 HRS},
	{"b.d.t.", DAYZONE, 11 HRS},

	{"gmt", ZONE, 0 HRS},
	{"g.m.t.", ZONE, 0 HRS},

	{"aest", ZONE, -10 HRS},	/* Australian Eastern Time */
	{"a.e.s.t.", ZONE, -10 HRS},
	{"aesst", DAYZONE, -10 HRS},	/* Australian Eastern Summer Time */
	{"a.e.s.s.t.", DAYZONE, -10 HRS},
	{"acst", ZONE, -(9 HRS + HALFHR)},	/* Australian Central Time */
	{"a.c.s.t.", ZONE, -(9 HRS + HALFHR)},
	{"acsst", DAYZONE, -(9 HRS + HALFHR)},	/* Australian Central Summer */
	{"a.c.s.s.t.", DAYZONE, -(9 HRS + HALFHR)},
	{"awst", ZONE, -8 HRS},		/* Australian Western Time */
	{"a.w.s.t.", ZONE, -8 HRS},	/* (no daylight time there, I'm told */
	{0, 0, 0}};

struct table unittb[] = {
	{"year", MUNIT, 12},
	{"month", MUNIT, 1},
	{"fortnight", UNIT, 14*24*60},
	{"week", UNIT, 7*24*60},
	{"day", UNIT, 1*24*60},
	{"hour", UNIT, 60},
	{"minute", UNIT, 1},
	{"min", UNIT, 1},
	{"second", SUNIT, 1},
	{"sec", SUNIT, 1},
	{0, 0, 0}};

struct table othertb[] = {
	{"tomorrow", UNIT, 1*24*60},
	{"yesterday", UNIT, -1*24*60},
	{"today", UNIT, 0},
	{"now", UNIT, 0},
	{"last", NUMBER, -1},
	{"this", UNIT, 0},
	{"next", NUMBER, 2},
	{"first", NUMBER, 1},
	/* {"second", NUMBER, 2}, */
	{"third", NUMBER, 3},
	{"fourth", NUMBER, 4},
	{"fifth", NUMBER, 5},
	{"sixth", NUMBER, 6},
	{"seventh", NUMBER, 7},
	{"eigth", NUMBER, 8},
	{"ninth", NUMBER, 9},
	{"tenth", NUMBER, 10},
	{"eleventh", NUMBER, 11},
	{"twelfth", NUMBER, 12},
	{"ago", AGO, 1},
	{0, 0, 0}};

struct table milzone[] = {
	{"a", ZONE, 1 HRS},
	{"b", ZONE, 2 HRS},
	{"c", ZONE, 3 HRS},
	{"d", ZONE, 4 HRS},
	{"e", ZONE, 5 HRS},
	{"f", ZONE, 6 HRS},
	{"g", ZONE, 7 HRS},
	{"h", ZONE, 8 HRS},
	{"i", ZONE, 9 HRS},
	{"k", ZONE, 10 HRS},
	{"l", ZONE, 11 HRS},
	{"m", ZONE, 12 HRS},
	{"n", ZONE, -1 HRS},
	{"o", ZONE, -2 HRS},
	{"p", ZONE, -3 HRS},
	{"q", ZONE, -4 HRS},
	{"r", ZONE, -5 HRS},
	{"s", ZONE, -6 HRS},
	{"t", ZONE, -7 HRS},
	{"u", ZONE, -8 HRS},
	{"v", ZONE, -9 HRS},
	{"w", ZONE, -10 HRS},
	{"x", ZONE, -11 HRS},
	{"y", ZONE, -12 HRS},
	{"z", ZONE, 0 HRS},
	{0, 0, 0}};

lookup(id) char *id;
{
#define gotit (yylval=i->value,  i->type)
#define getid for(j=idvar, k=id; *j++ = *k++; )

	char idvar[20];
	register char *j, *k;
	register struct table *i;
	int abbrev;

	getid;
	if (strlen(idvar) == 3) abbrev = 1;
	else if (strlen(idvar) == 4 && idvar[3] == '.') {
		abbrev = 1;
		idvar[3] = '\0';
	}
	else abbrev = 0;

	if (islower(*idvar)) *idvar = toupper(*idvar);

	for (i = mdtab; i->name; i++) {
		k = idvar;
		for (j = i->name; *j++ == *k++;) {
			if (abbrev && j==i->name+3) return gotit;
			if (j[-1] == 0) return gotit;
		}
	}

	getid;
	for (i = mztab; i->name; i++)
		if (strcmp(i->name, idvar) == 0) return gotit;

	for (j = idvar; *j; j++) if (isupper(*j)) *j = tolower(*j);
	for (i=mztab; i->name; i++)
		if (strcmp(i->name, idvar) == 0) return gotit;

	getid;
	for (i=unittb; i->name; i++)
		if (strcmp(i->name, idvar) == 0) return gotit;

	if (idvar[strlen(idvar)-1] == 's') idvar[strlen(idvar)-1] = '\0';
	for (i=unittb; i->name; i++)
		if (strcmp(i->name, idvar) == 0) return gotit;

	getid;
	for (i = othertb; i->name; i++)
		if (strcmp(i->name, idvar) == 0) return gotit;

	getid;
	if (strlen(idvar) == 1 && isalpha(*idvar)) {
		if (isupper(*idvar)) *idvar = tolower(*idvar);
		for (i = milzone; i->name; i++)
			if (strcmp(i->name, idvar) == 0) return gotit;
	}

	return(ID);
}

/* K.S. 1-Dec-97, changed name of 'getdate' to 'getdatex' */
double getdatex(p, now) char *p; struct timeb *now;
{
#define mcheck(f)	if (f>1) err++
	double monthadd();
	int err;
	struct tm *lt;
	struct timeb ftz;

	double sdate, tod;
	struct timeval tp;
	struct timezone tzp;

	lptr = p;
#ifdef FTIME	/* old, ftime version of this code */
	if (now == ((struct timeb *) NULL)) {
		now = &ftz;
		ftime(&ftz);
	}
#else	        /* new version of getdate using gettimeofday */
	if (now == ((struct timeb *) NULL)) {
		now = (struct timeb *) (malloc (sizeof (struct timeb)));
	        gettimeofday(&tp, &tzp);
		now->time = tp.tv_sec;
		now->timezone = tzp.tz_minuteswest;
	}
#endif
	lt = localtime(&now->time);
	year = lt->tm_year;
	month = lt->tm_mon+1;
	day = lt->tm_mday;
	relsec = 0; relmonth = 0;
	timeflag=zoneflag=dateflag=dayflag=relflag=0;
	ourzone = now->timezone;

	/* The problem here was that if daylight was specified as MAYBE,
	   then it would later try and figure out whether daylight savings
	   applied in your current timezone, not in the one you specified
	   by supplying the ftime argument. Since we know whether DST
	   applies or not from the ftime structure, we use that. */

/*	daylight = MAYBE;	*/

	daylight = (now->dstflag ? DAYLIGHT : STANDARD);
	
	hh = mm = ss = fs = 0;
	merid = 24;

	if (err = yyparse()) return (-1);

	mcheck(timeflag);
	mcheck(zoneflag);
	mcheck(dateflag);
	mcheck(dayflag);

	if (err) return (-1);

	if (dateflag || timeflag || dayflag) {
		sdate = dateconv(month,day,year,hh,mm,ss,fs,merid,ourzone,daylight);
	}
	else {
		sdate = now->time;
		if (relflag == 0)
			sdate -= (lt->tm_sec + lt->tm_min*60 +
				lt->tm_hour*(60L*60L));
	}

	sdate += relsec;
	sdate += monthadd(sdate, relmonth);

	if (dayflag) {
		tod = dayconv(dayord, dayreq, sdate);
		sdate += tod;
	}

	return sdate;
}

yyerror(s) char *s;
{}
__YYSCLASS yytabelem yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
	};
# define YYNPROD 35
# define YYLAST 226
__YYSCLASS yytabelem yyact[]={

    12,    13,    21,     9,    14,    15,    16,    10,    11,    40,
    17,    38,    37,    19,    36,    35,    30,    29,    33,    28,
    26,    41,    34,    31,    27,     8,     7,     6,     5,     4,
     3,     2,     1,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,    22,    20,    18,     0,    23,    24,    25,
    32,     0,     0,     0,     0,    39 };
__YYSCLASS yytabelem yypact[]={

 -3000,  -258, -3000, -3000, -3000, -3000, -3000,  -257, -3000,   -45,
 -3000, -3000,  -241,   -20, -3000, -3000, -3000, -3000, -3000,  -242,
 -3000,  -244,  -245, -3000, -3000, -3000,   -21, -3000,   -40,   -25,
 -3000,  -246, -3000,  -247,  -249, -3000,   -35, -3000,  -252, -3000,
  -239, -3000 };
__YYSCLASS yytabelem yypgo[]={

     0,    32,    31,    30,    29,    28,    27,    26,    25 };
__YYSCLASS yytabelem yyr1[]={

     0,     1,     1,     2,     2,     2,     2,     2,     2,     8,
     3,     3,     3,     3,     3,     3,     3,     4,     4,     6,
     6,     6,     5,     5,     5,     5,     5,     5,     7,     7,
     7,     7,     7,     7,     7 };
__YYSCLASS yytabelem yyr2[]={

     0,     0,     4,     3,     3,     3,     3,     3,     2,     3,
     5,     7,     9,    11,    15,    13,    17,     3,     3,     3,
     5,     5,     7,    11,     5,     9,     5,     7,     5,     5,
     5,     3,     3,     3,     5 };
__YYSCLASS yytabelem yychk[]={

 -3000,    -1,    -2,    -3,    -4,    -5,    -6,    -7,    -8,   261,
   265,   266,   258,   259,   262,   263,   264,   267,   260,    58,
   259,    47,   258,   262,   263,   264,   261,    44,   261,   261,
   261,    44,   260,    58,    47,   261,   261,   261,    46,   260,
   261,   260 };
__YYSCLASS yytabelem yydef[]={

     1,    -2,     2,     3,     4,     5,     6,     7,     8,     9,
    17,    18,     0,    19,    31,    32,    33,    34,    10,     0,
    21,     0,    26,    28,    29,    30,    24,    20,    11,    22,
    27,     0,    12,     0,     0,    25,    13,    23,     0,    15,
    14,    16 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

__YYSCLASS yytoktype yytoks[] =
{
	"ID",	257,
	"MONTH",	258,
	"DAY",	259,
	"MERIDIAN",	260,
	"NUMBER",	261,
	"UNIT",	262,
	"MUNIT",	263,
	"SUNIT",	264,
	"ZONE",	265,
	"DAYZONE",	266,
	"AGO",	267,
	"-unknown-",	-1	/* ends search */
};

__YYSCLASS char * yyreds[] =
{
	"-no such reduction-",
	"timedate : /* empty */",
	"timedate : timedate item",
	"item : tspec",
	"item : zone",
	"item : dtspec",
	"item : dyspec",
	"item : rspec",
	"item : nspec",
	"nspec : NUMBER",
	"tspec : NUMBER MERIDIAN",
	"tspec : NUMBER ':' NUMBER",
	"tspec : NUMBER ':' NUMBER MERIDIAN",
	"tspec : NUMBER ':' NUMBER ':' NUMBER",
	"tspec : NUMBER ':' NUMBER ':' NUMBER '.' NUMBER",
	"tspec : NUMBER ':' NUMBER ':' NUMBER MERIDIAN",
	"tspec : NUMBER ':' NUMBER ':' NUMBER '.' NUMBER MERIDIAN",
	"zone : ZONE",
	"zone : DAYZONE",
	"dyspec : DAY",
	"dyspec : DAY ','",
	"dyspec : NUMBER DAY",
	"dtspec : NUMBER '/' NUMBER",
	"dtspec : NUMBER '/' NUMBER '/' NUMBER",
	"dtspec : MONTH NUMBER",
	"dtspec : MONTH NUMBER ',' NUMBER",
	"dtspec : NUMBER MONTH",
	"dtspec : NUMBER MONTH NUMBER",
	"rspec : NUMBER UNIT",
	"rspec : NUMBER MUNIT",
	"rspec : NUMBER SUNIT",
	"rspec : UNIT",
	"rspec : MUNIT",
	"rspec : SUNIT",
	"rspec : rspec AGO",
};
#endif /* YYDEBUG */
#define YYFLAG  (-3000)
/* @(#) $Revision: 70.7 $ */    

/*
** Skeleton parser driver for yacc output
*/

#if defined(NLS) && !defined(NL_SETN)
#include <msgbuf.h>
#endif

#ifndef nl_msg
#define nl_msg(i,s) (s)
#endif

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab

#ifndef __RUNTIME_YYMAXDEPTH
#define YYACCEPT	return(0)
#define YYABORT		return(1)
#else
#define YYACCEPT	{free_stacks(); return(0);}
#define YYABORT		{free_stacks(); return(1);}
#endif

#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( (nl_msg(30001,"syntax error - cannot backup")) );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#define YYRECOVERING()	(!!yyerrflag)
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/
int yydebug;			/* set to 1 to get debugging */

/*
** driver internal defines
*/
/* define for YYFLAG now generated by yacc program. */
/*#define YYFLAG		(FLAGVAL)*/

/*
** global variables used by the parser
*/
# ifndef __RUNTIME_YYMAXDEPTH
__YYSCLASS YYSTYPE yyv[ YYMAXDEPTH ];	/* value stack */
__YYSCLASS int yys[ YYMAXDEPTH ];		/* state stack */
# else
__YYSCLASS YYSTYPE *yyv;			/* pointer to malloc'ed value stack */
__YYSCLASS int *yys;			/* pointer to malloc'ed stack stack */

#if defined(__STDC__) || defined (__cplusplus)
#include <stdlib.h>
#else
	extern char *malloc();
	extern char *realloc();
	extern void free();
#endif /* __STDC__ or __cplusplus */


static int allocate_stacks(); 
static void free_stacks();
# ifndef YYINCREMENT
# define YYINCREMENT (YYMAXDEPTH/2) + 10
# endif
# endif	/* __RUNTIME_YYMAXDEPTH */
long  yymaxdepth = YYMAXDEPTH;

__YYSCLASS YYSTYPE *yypv;			/* top of value stack */
__YYSCLASS int *yyps;			/* top of state stack */

__YYSCLASS int yystate;			/* current state */
__YYSCLASS int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */
__YYSCLASS int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */



/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
int
yyparse()
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */

	/*
	** Initialize externals - yyparse may be called more than once
	*/
# ifdef __RUNTIME_YYMAXDEPTH
	if (allocate_stacks()) YYABORT;
# endif
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

	goto yystack;
	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/
	yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	yy_stack:
		/*
		** put a state and value onto the stacks
		*/
#if YYDEBUG
		/*
		** if debugging, look up token value in list of value vs.
		** name pairs.  0 and negative (-1) are special values.
		** Note: linear search is used since time is not a real
		** consideration while debugging.
		*/
		if ( yydebug )
		{
			register int yy_i;

			printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	/* room on stack? */
		{
# ifndef __RUNTIME_YYMAXDEPTH
			yyerror( (nl_msg(30002,"yacc stack overflow")) );
			YYABORT;
# else
			/* save old stack bases to recalculate pointers */
			YYSTYPE * yyv_old = yyv;
			int * yys_old = yys;
			yymaxdepth += YYINCREMENT;
			yys = (int *) realloc(yys, yymaxdepth * sizeof(int));
			yyv = (YYSTYPE *) realloc(yyv, yymaxdepth * sizeof(YYSTYPE));
			if (yys==0 || yyv==0) {
			    yyerror( (nl_msg(30002,"yacc stack overflow")) );
			    YYABORT;
			    }
			/* Reset pointers into stack */
			yy_ps = (yy_ps - yys_old) + yys;
			yyps = (yyps - yys_old) + yys;
			yy_pv = (yy_pv - yyv_old) + yyv;
			yypv = (yypv - yyv_old) + yyv;
# endif

		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		/*
		** we have a new state - find out what to do
		*/
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		yytmp = yychar < 0;
#endif
		if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			printf( "Received token " );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	/*valid shift*/
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
#if YYDEBUG
			yytmp = yychar < 0;
#endif
			if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				printf( "Received token " );
				if ( yychar == 0 )
					printf( "end-of-file\n" );
				else if ( yychar < 0 )
					printf( "-none-\n" );
				else
				{
					for ( yy_i = 0;
						yytoks[yy_i].t_val >= 0;
						yy_i++ )
					{
						if ( yytoks[yy_i].t_val
							== yychar )
						{
							break;
						}
					}
					printf( "%s\n", yytoks[yy_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( yy_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( yyerrflag )
			{
			case 0:		/* new error */
				yyerror( (nl_msg(30003,"syntax error")) );
				yynerrs++;
				goto skip_init;
			yyerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
				yynerrs++;
			skip_init:
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				yyerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( yydebug )
						printf( _POP_, *yy_ps,
							yy_ps[-1] );
#	undef _POP_
#endif
					yy_ps--;
					yy_pv--;
				}
				/*
				** there is no state on stack with "error" as
				** a valid shift.  give up.
				*/
				YYABORT;
			case 3:		/* no shift yet; eat a token */
#if YYDEBUG
				/*
				** if debugging, look up token in list of
				** pairs.  0 and negative shouldn't occur,
				** but since timing doesn't matter when
				** debugging, it doesn't hurt to leave the
				** tests here.
				*/
				if ( yydebug )
				{
					register int yy_i;

					printf( "Error recovery discards " );
					if ( yychar == 0 )
						printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						printf( "token -none-\n" );
					else
					{
						for ( yy_i = 0;
							yytoks[yy_i].t_val >= 0;
							yy_i++ )
						{
							if ( yytoks[yy_i].t_val
								== yychar )
							{
								break;
							}
						}
						printf( "token %s\n",
							yytoks[yy_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( yychar == 0 )	/* reached EOF. quit */
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}/* end if ( yy_n == 0 ) */
		/*
		** reduction by production yy_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( yydebug )
			printf( "Reduce by (%d) \"%s\"\n",
				yy_n, yyreds[ yy_n ] );
#endif
		yytmp = yy_n;			/* value to switch over */
		yypvt = yy_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using yy_state here as temporary
		** register variable, but why not, if it works...
		** If yyr2[ yy_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto yy_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}
					/* save until reenter driver code */
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( yytmp )
	{
		
case 3:
# line 46 "getdate.y"

		{timeflag++;} break;
case 4:
# line 48 "getdate.y"

		{zoneflag++;} break;
case 5:
# line 50 "getdate.y"

		{dateflag++;} break;
case 6:
# line 52 "getdate.y"

		{dayflag++;} break;
case 7:
# line 54 "getdate.y"

		{relflag++;} break;
case 9:
# line 58 "getdate.y"

		{if (timeflag && dateflag && !relflag) year = yypvt[-0];
		else {timeflag++;hh = yypvt[-0]/100;mm = yypvt[-0]%100;ss = 0;merid = 24;}} break;
case 10:
# line 62 "getdate.y"

		{hh = yypvt[-1]; mm = 0; ss = 0; merid = yypvt[-0];} break;
case 11:
# line 64 "getdate.y"

		{hh = yypvt[-2]; mm = yypvt[-0]; merid = 24;} break;
case 12:
# line 66 "getdate.y"

		{hh = yypvt[-3]; mm = yypvt[-1]; merid = yypvt[-0];} break;
case 13:
# line 68 "getdate.y"

		{hh = yypvt[-4]; mm = yypvt[-2]; ss = yypvt[-0]; merid = 24;} break;
case 14:
# line 70 "getdate.y"

		{hh = yypvt[-6]; mm = yypvt[-4]; ss = yypvt[-2]; fs = yypvt[-0]; merid = 24;} break;
case 15:
# line 72 "getdate.y"

		{hh = yypvt[-5]; mm = yypvt[-3]; ss = yypvt[-1]; merid = yypvt[-0];} break;
case 16:
# line 74 "getdate.y"

		{hh = yypvt[-7]; mm = yypvt[-5]; ss = yypvt[-3]; fs = yypvt[-1]; merid = yypvt[-0];} break;
case 17:
# line 77 "getdate.y"

		{ourzone = yypvt[-0]; daylight = STANDARD;} break;
case 18:
# line 79 "getdate.y"

		{ourzone = yypvt[-0]; daylight = DAYLIGHT;} break;
case 19:
# line 82 "getdate.y"

		{dayord = 1; dayreq = yypvt[-0];} break;
case 20:
# line 84 "getdate.y"

		{dayord = 1; dayreq = yypvt[-1];} break;
case 21:
# line 86 "getdate.y"

		{dayord = yypvt[-1]; dayreq = yypvt[-0];} break;
case 22:
# line 89 "getdate.y"

		{month = yypvt[-2]; day = yypvt[-0];} break;
case 23:
# line 91 "getdate.y"

		{month = yypvt[-4]; day = yypvt[-2]; year = yypvt[-0];} break;
case 24:
# line 93 "getdate.y"

		{month = yypvt[-1]; day = yypvt[-0];} break;
case 25:
# line 95 "getdate.y"

		{month = yypvt[-3]; day = yypvt[-2]; year = yypvt[-0];} break;
case 26:
# line 97 "getdate.y"

		{month = yypvt[-0]; day = yypvt[-1];} break;
case 27:
# line 99 "getdate.y"

		{month = yypvt[-1]; day = yypvt[-2]; year = yypvt[-0];} break;
case 28:
# line 103 "getdate.y"

		{relsec +=  60L * yypvt[-1] * yypvt[-0];} break;
case 29:
# line 105 "getdate.y"

		{relmonth += yypvt[-1] * yypvt[-0];} break;
case 30:
# line 107 "getdate.y"

		{relsec += yypvt[-1];} break;
case 31:
# line 109 "getdate.y"

		{relsec +=  60L * yypvt[-0];} break;
case 32:
# line 111 "getdate.y"

		{relmonth += yypvt[-0];} break;
case 33:
# line 113 "getdate.y"

		{relsec++;} break;
case 34:
# line 115 "getdate.y"

		{relsec = -relsec; relmonth = -relmonth;} break;
	}
	goto yystack;		/* reset registers in driver code */
}

# ifdef __RUNTIME_YYMAXDEPTH

static int allocate_stacks() {
	/* allocate the yys and yyv stacks */
	yys = (int *) malloc(yymaxdepth * sizeof(int));
	yyv = (YYSTYPE *) malloc(yymaxdepth * sizeof(YYSTYPE));

	if (yys==0 || yyv==0) {
	   yyerror( (nl_msg(30004,"unable to allocate space for yacc stacks")) );
	   return(1);
	   }
	else return(0);

}


static void free_stacks() {
	if (yys!=0) free((char *) yys);
	if (yyv!=0) free((char *) yyv);
}

# endif  /* defined(__RUNTIME_YYMAXDEPTH) */

