
/*
 * NAME
 *	getargs -- Get command-line agruments from LocSAT

 * FILE
 *	getargs.c

 * SYNOPSIS
 *	Process command-line arguments representing input/output files
 *	from program, LocSAT.

 * DESCRIPTION
 *	Function.  Input and output files are processed here via
 *	user-specified command-line arguments in program, LocSAT.

 * DIAGNOSTICS
 *	Complains when bogus command line arguments are given.

 * FILES
 *	None.

 * NOTES
 *	None.

 * SEE ALSO
 *
 
 * AUTHOR
 *	Walter Nagy, February 1991.
 */


#include <stdio.h>
#include <strings.h>

#define TOKC_SET	1
#define TOKD_SET	2
#define TOKO_SET	4
#define TOKS_SET	8
#define TOK_ALLSET	(TOKC_SET|TOKD_SET|TOKO_SET|TOKS_SET)


char *getargs (argc, argv, inter, tokc, tokd, toko, toks)
int	argc,
	*inter;
char	**argv,
	**toks,		/* Station	filename */
	**tokd,		/* Data		filename */
	**tokc,		/* Control	filename */
	**toko;		/* Output	filename */
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

		if (index("cdso",cflag) != NULL )
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

		case 'c':
			if ((inputset & TOKC_SET) == TOKC_SET)
				ermsg = "command line TOKC"; 
			else
				*tokc = arg;
			inputset |= TOKC_SET;
			break;

		case 'd':
			if ((inputset & TOKD_SET) == TOKD_SET)
				ermsg = "command line TOKD"; 
			else
				*tokd = arg;
			inputset |= TOKD_SET;
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

		default:
			ermsg = "invalid command line option";
			break;

		}

	}

	return (ermsg); 

}
