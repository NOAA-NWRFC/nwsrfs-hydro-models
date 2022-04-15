//------------------------------------------------------------------------------
// MaxIncrease::construct - reads in necessary data for the MaxIncrease method. 
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 20 Feb 1998	Daniel K. Weiler, Riverside Technology, inc
//					Created initial version.
// 21 Nov 2001	JRV, RTi	Improved error handling
// 2003-11-21 Luiz Teixeira, RTi - Added list = FreeStringList( list ) at the 
//  				end of the main for loop in the construct method
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include <stdio.h>

#include "resj_main.h"
#include "MaxIncrease.h"
#include "TSUtil.h"

int MaxIncrease :: construct ( char** re_list, int n_items )  
{
	char routine[] = "(MaxIncrease::construct) -", **list = NULL, 
		**value_list=NULL;
	int i, nlist, n_value;
	double ffactor = 1.0;
	int numErrs = 0;

	if( Method::getUnitType() == ENGLISH ) {
		ffactor = 0.028317;
	}

	for( i = 0; i < n_items; i++ ) {
		if( strlen( re_list[i] ) == 0 || re_list[i][0] == '#' ){
			continue;
		}
		list = BreakStringList( re_list[i], " \n\t",
			DELIM_SKIP_BLANKS, &nlist );
		if( nlist == 0 || list == NULL ) {
			list = FreeStringList( list );
			/*AVCP PrintError( routine,*/
		        logMessageWithArgsAndExitOnError( FATAL_LEVEL,
		        "%s %s Troubles getting data for %s %s.",
			errorStr, routine, _type, _id );
			//return( STATUS_FAILURE );
		}

		// This reads in the increase over the previous 24 hr period.
		if( !strcasecmp( list[0], "INCREASE"  ) ) { 
                        if( nlist < 2 ) {
				// The following is a warning since we might see
				// INCREASE later
				/*AVCP PrintWarning( 1, routine, */
			     if( getFewsDebugFlag() > 0 )
			     {
			        logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
				"%s %s Value required immediately after %s "
				"keyword %s.", warnStr, routine, _type, list[0]);
			     }
			     list = FreeStringList( list );
			     continue;
			}
			_max_increase = atof( list[1] ) * ffactor;
			list = FreeStringList( list );
		}
		
		// Freeing memory
		if ( list ) {
			list = FreeStringList( list );  // 2003-11-21 LT
		}	
	}

	// Check to see if we got the necessaries
	if( _max_increase == MISSING ) {
		numErrs++;
		/*AVCP PrintError( routine,*/
		logMessageWithArgsAndExitOnError( FATAL_LEVEL,
		"%s %s Keyword INCREASE and Value required but not "
		"found in %s %s ", errorStr, routine, _type, _id );
	}

	if( numErrs > 0 ) {
		return(STATUS_FAILURE);
	}

	_is_constructed = 1;
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/MaxIncrease_construct.cxx,v $";
 static char rcs_id2[] = "$Id: MaxIncrease_construct.cxx,v 1.6 2006/10/26 15:26:23 hsu Exp $";}
/*  ===================================================  */

}		
