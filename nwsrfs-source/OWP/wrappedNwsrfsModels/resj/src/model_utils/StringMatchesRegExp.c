/*------------------------------------------------------------------------------
** StringMatchesRegExp - does a string match a regular expression?
**------------------------------------------------------------------------------
** Notes:	(1)	This routine compares a candidate string with a string
**			that contains regular expression wildcard characters and
**			returns 1 if the candidate string matches.  The
**			following wild cards are currently recognized:
**
**				.	Match one character.
**				*	Match zero or more characters.
**				[...]	Match any one the characters enclosed
**					in the brackets.  This can be a list
**					of characters or a character range
**					separated by a -.
**		(2)	This routine is designed to be called by a higher level
**			routine to check actual filenames versus wildcard
**			patterns.
**		(3)	The following combination is known not to work:
**
**				xxx*.[abc]
**
**			and will be fixed as time allows.
**------------------------------------------------------------------------------
** History:
**
** 28 Sep 1995	Steven A. Malers		Original routine - based on
**		Riverside Technology, inc.	CUJ April 1995 article by Mike
**						Cornelison, but use regular
**						expressions from the "sed and
**						awk" O'Reilly book.
** 04 Oct 1995	SAM, RTi			Add [] capability.
**------------------------------------------------------------------------------
** Variable		I/O	Description
**
** asterisk		L	Indicates whether we are in a section of the
**				regular expression that starts with an asterisk.
** candidate_string	I	Candidate string to check.
** dl			L	Debug level for this routine.
** i			L	Loop counter on characters.
** nokchars		L	Number of "okchars".
** okchars		L	Characters that are for the [] operator.
** pt_candidate		L	Pointer to candidate string.
** pt_wild		L	Pointer to regular expression string.
** regexp_string	I	String containing wildcards to check against.
** routine		L	Name of this routine.
**------------------------------------------------------------------------------
*/

#include "ResJ.h"
#include "resj_main.h"

int StringMatchesRegExp ( char *candidate_string, char *regexp_string )
{	char	message[256], okchars[256], *pt_candidate = candidate_string,
		*pt_regexp = regexp_string, routine[] = "StringMatchesRegExp";
	int	asterisk, dl = 50, i, j, jumptotest = 0, nokchars = 0;

	sprintf ( message, "Comparing \"%s\" to \"%s\"",
	pt_candidate, pt_regexp );
	if ( getFewsDebugFlag() > 3 )	
	{
           /*AVCP PrintDebug( 50, routine,*/
	   logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
		  "%s %s ", routine, message);
	}

	while ( 1 ) {
		/*
		** Start new segment in the regular expression...
		*/
		if ( getFewsDebugFlag() > 3 )	
	        {
                   /*AVCP PrintDebug( 50, routine,*/
	           logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
		       "%s Start new segment section", routine );
		}
		if ( !jumptotest ) {
			asterisk = 0;
			while ( *pt_regexp == '*' ) {
			   if ( getFewsDebugFlag() > 3 )	
	                   {
                               /*AVCP PrintDebug( dl, routine,*/
	                       logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
			          "%s Skipping *", routine);
			   }
			   ++pt_regexp;
			   asterisk = 1;
			}
		}

		/*
		** Now test for a match...
		*/

		if ( getFewsDebugFlag() > 3 )	
	        {
                      /*AVCP PrintDebug( 50, routine,*/
	              logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
		         "%s Start test section", routine );
		}

		jumptotest = 0;
		while ( 1 ) {
			for (	i = 0, j = 0;
				pt_regexp[i] && (pt_regexp[i] != '*');
				i++, j++ ) {
				sprintf ( message,
				"pt_regexp=[%d]=%c pt_candidate[%d]=%c",
				i, pt_regexp[i], j, pt_candidate[j] );
				if ( getFewsDebugFlag() > 3 )	
	        		{
                     		    /*AVCP PrintDebug( 50, routine,*/
	              		   logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
		         	   "%s %s ",routine, message );
				}
				if ( pt_regexp[i] != pt_candidate[j] ) {
					if ( !pt_candidate[j] ) {
						/*
						** No match...
						*/
						return 0;
						if ( getFewsDebugFlag() > 3 )	
	        				{
                      					/*AVCP PrintDebug( 50, routine,*/
	             					logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
		         				"%s Chars do not match - exit 0",routine);
						}
					}
					else if ( pt_regexp[i] == '.' ) {
						/*
						** Single character match...
						*/
						if ( getFewsDebugFlag() > 3 )	
	        				{
                      					/*AVCP PrintDebug( 50, routine,*/
	             					 logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
						         "%s Character . - go to next character", routine );
						}
						continue;
					}
					else if ( pt_regexp[i] == '[' ) {
						/*
						** Start of character range.
						** First need to get OK characters...
						*/
						if ( getFewsDebugFlag() > 3 )	
	        				{
                      					/*AVCP PrintDebug( 50, routine,*/
	             					 logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
							   "%s [ - check range character",routine );
						}
						++i;
						while ( pt_regexp[i] != ']' ) {
							if ( !pt_regexp[i] ) {
								return 0;
							}
							else if(pt_regexp[i]
								== '-' ) {
								/*
								** Need to find
								** the next
								** character and
								** then go
								** until that
								** matches...
								*/
								++i;
								if ( !pt_regexp[i] ) {
									return 0;
								}
								else if ( (nokchars > 0) &&
									(pt_regexp[i] <
									okchars[nokchars - 1]) ) {
									return 0;
								}
								sprintf ( message,
								"Using range %c to %c",
								okchars[nokchars - 1],
								pt_regexp[i] );
								if ( getFewsDebugFlag() > 3 )	
	        						{
                      						   /*AVCP PrintDebug( 50, routine,*/
	             					 	   logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
								   "%s %s ", routine, message );
								}
								while ( 1 ) {
									okchars[nokchars] =
									okchars[nokchars - 1] + 1;
									++nokchars;
									sprintf ( message,
									"Added %c from [-] list",
									okchars[nokchars - 1] );
									if ( getFewsDebugFlag() > 3 )	
	        							{
                      						   	   /*AVCP PrintDebug( 50, routine,*/
	             					 	   	   logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
								   		"%s %s ", routine, message );
									}
									if (	okchars[nokchars - 1] ==
										pt_regexp[i] ) {
										/*
										** Last character in range...
										*/
										break;

									}
								}
							}
							else {	/*
								** Just add the
								** character...
								*/
								okchars[nokchars] =
								pt_regexp[i];
								++nokchars;
								sprintf ( message,
								"Added %c from [abc] list",
								okchars[nokchars - 1] );
								if ( getFewsDebugFlag() > 3 )	
	        						{
                      						   /*AVCP PrintDebug( 50, routine,*/
	             					 	   logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
								   "%s %s ", routine, message );
								}
								++i;
							}
						}
						/*
						** Now check the character...
						*/
						okchars[nokchars] = '\0';
						if (	strchr(okchars,
							pt_candidate[j]) ) {
							/*
							** Matches OK...
							*/
							continue;
						}
						else {	/*
							** No match...
							*/
							return 0;
						}
					}
					else if ( !asterisk ) {
						/*
						** ?
						*/
						if ( getFewsDebugFlag() > 3 )	
	        				{
                      		   			 /*AVCP PrintDebug( 50, routine,*/
	             		    			logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
						 		"%s Not asterisk - exit 0", routine );
						}
						return 0;
					}
					++pt_candidate;
					/*
					** Reevaluate the loop again...
					*/
					if ( getFewsDebugFlag() > 3 )	
	        			{
                      				/*AVCP PrintDebug( 50, routine,*/
	             				logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
						 "%s Jumping to test", routine );
					}
				
					jumptotest = 1;
					break;
				}
				else {	
				
				   if ( getFewsDebugFlag() > 3 )	
	        		   {
                      			/*AVCP PrintDebug( 50, routine,*/
	             			logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
						 "%s Chars are equal.  Increment...", routine );
				   }
				}
			}
			if (	jumptotest ||
				!pt_regexp[i] || (pt_regexp[i] == '*') ) {
				break;
			}
		}

		if ( getFewsDebugFlag() > 3 )	
	        {
                      /*AVCP PrintDebug( 50, routine,*/
	             	logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
				"%s Outside for loop", routine );
		}

		if ( !jumptotest ) {
			if ( pt_regexp[i] == '*' ) {
				sprintf ( message,
				"Have an * - increment by %d and restart segment",
				i );
				if ( getFewsDebugFlag() > 3 )	
	        		{
                      		    /*AVCP PrintDebug( 50, routine,*/
	             		    logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
						 "%s %s", routine, message);
				}
				pt_candidate	+= j;
				pt_regexp	+= i;
				continue;
			}

			if ( !pt_candidate[j] ) {
				/*
				** End of string...
				*/
				if ( getFewsDebugFlag() > 3 )	
	        		{
                      		    /*AVCP PrintDebug( 50, routine,*/
	             		    logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
						 "%s End of string - return 1",routine );
				}
				return 1;
			}
			else if ( i && pt_regexp[i - 1] == '*' ) {
				/*
				** Rest of string is wildcard...
				*/
				if ( getFewsDebugFlag() > 3 )	
	        		{
                      		    /*AVCP PrintDebug( 50, routine,*/
	             		    logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
						 "%s Rest of string * - return 1" , routine);
				}
				return 1;
			}
			else if ( !asterisk ) {
				if ( getFewsDebugFlag() > 3 )	
	        		{
                      		    /*AVCP PrintDebug( 50, routine,*/
	             		    logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
						 "%s Not asterisk - return 0" , routine);
				}
				return 0;
			}
			++pt_candidate;
			if ( getFewsDebugFlag() > 3 )	
	        	{
                      		/*AVCP PrintDebug( 50, routine,*/
	             		logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
				   "%s Jumping to test", routine );
			}
			jumptotest = 1;
		}
	}

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_utils/RCS/StringMatchesRegExp.c,v $";
 static char rcs_id2[] = "$Id: StringMatchesRegExp.c,v 1.1 1999/02/18 15:17:22 dws Exp $";}
/*  ===================================================  */

}
