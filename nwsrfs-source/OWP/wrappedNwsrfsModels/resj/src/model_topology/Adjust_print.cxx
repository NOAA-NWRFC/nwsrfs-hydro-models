//------------------------------------------------------------------------------
// Adjust :: print - printd instance data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 04 Mar 1998	Matthew J. Rutherford, RTi
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "resj_main.h"
#include "Adjust.h"
#include "TSDateIterator.h"

int Adjust :: print( FILE* fp )
{
	char routine[] = "Adjust :: print";

	if( fp == NULL ) {
		/*AVCP PrintWarning( 1, routine,*/
	        logMessageWithArgsAndExitOnError( FATAL_LEVEL,
		"%s (%s) - Cannot print Adjust info - null FILE*.",
		errorStr, routine );
		/*AVCP return( STATUS_FAILURE );*/
	}

	// Print self id fnd owner first
	fprintf( fp, "Adjust method \"%s\" owned by Component \"%s\".\n", 
		_id, _owner->getID() );
	/*AVCP if ( getFewsDebugFlag() > 3 )
	{
	   logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	   "(%s) - Adjust method \"%s\" owned by Component \"%s\".",
	   routine, _id, _owner->getID() );
	}*/

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Adjust_print.cxx,v $";
 static char rcs_id2[] = "$Id: Adjust_print.cxx,v 1.3 2006/10/26 15:08:28 hsu Exp $";}
/*  ===================================================  */

}
