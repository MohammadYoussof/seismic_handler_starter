
/*
 * NAME
 *	getargp -- Get command-line agruments from PredSAT

 * FILE
 *	getargp.c

 * SYNOPSIS
 *	Process command-line arguments representing input/output files
 *	from program, PredSAT.

 * DESCRIPTION
 *	Function.  Input and output files are processed here via
 *	user-specified command-line arguments in program, PredSAT.

 * DIAGNOSTICS
 *	Complains when bogus command line arguments are given.

 * FILES
 *	None.

 * NOTES
 *	None.

 * SEE ALSO
 *
 
 * AUTHOR
 *	Walter Nagy, May 1991.
 */


#include <stdio.h>
#include <strings.h>

#define TOKE_SET	1
#define TOKO_SET	2
#define TOKS_SET	4
#define TOKT_SET	8
#define TOKW_SET	16
#define	TOK_ALLSET	(TOKE_SET|TOKO_SET|TOKS_SET|TOKT_SET|TOKW_SET)


char *getargp (argc, argv, inter, toke, toko, toks, tokt, tokw)
int	argc,
	*inter;
char	**argv,
	**toke,		/* Event	filename */
	**toko,		/* Output	filename */
	**toks,		/* Station	filename */
	**tokt,		/* T-T		filename */
	**tokw;		/* Phase list	filename */
{
	char	*index();
	int	arglen,		/* Length of command line element */
		inputset = 0;	/* set input flags */
	char	*ermsg,		/* Error message pointer */
		cflag,		/* Command line flag */
		*arg;		/* Argument to command line flag */

	ermsg  = '\0';

	while (ermsg == '\0' && *++argv != NULL &&
		*argv[0] == '-' && (arglen = strlen(*argv)) > 1)
	{
		cflag = *(*argv+1);

		if (index("eostw",cflag) != NULL )
		{
			if (arglen > 2 )
				arg = *argv + 2;
			else
			{
				if (*++argv == NULL )
					return ("option requires argument");
				arg = *argv;
			}
		}

		switch (cflag)
		{

		case 'e':
			if ((inputset & TOKE_SET) == TOKE_SET)
				ermsg = "command line TOKE"; 
			else
				*toke = arg;
			inputset |= TOKE_SET;
			break;

		case 'i':
			*inter = 1;
			break;

		case 'o':
			if ((inputset & TOKO_SET) == TOKO_SET)
				ermsg = "command line TOKO"; 
			else
				*toko = arg;
			inputset |= TOKO_SET;
			break;

		case 's':
			if ((inputset & TOKS_SET) == TOKS_SET)
				ermsg = "command line TOKS"; 
			else
				*toks = arg;
			inputset |= TOKS_SET;
			break;

		case 't':
			if ((inputset & TOKT_SET) == TOKT_SET)
				ermsg = "command line TOKT"; 
			else
				*tokt = arg;
			inputset |= TOKT_SET;
			break;

		case 'w':
			if ((inputset & TOKW_SET) == TOKW_SET)
				ermsg = "command line TOKW"; 
			else
				*tokw = arg;
			inputset |= TOKW_SET;
			break;

		default:
			ermsg = "invalid command line option";
			break;

		}

	}

	return (ermsg); 

}
