//------------------------------------------------------------------------------
// Balance :: print - printd instance data members.
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
#include "Balance.h"
#include "TSDateIterator.h"

int Balance :: print( FILE* fp )
{
	char routine[] = "Balance :: print";

	if( fp == NULL ) {
		/*AVCP PrintWarning( 1, routine,*/
	        logMessageWithArgsAndExitOnError( FATAL_LEVEL,
		"%s (%s) - Cannot print Balance info - null FILE*.",
		errorStr, routine );
		/*AVCP return( STATUS_FAILURE );*/
	}

	// Print self id fnd owner first
	/*AVCP fprintf( fp,*/
	if( getFewsDebugFlag() > 0 )
	{
          logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
          "(%s) - Balance method \"%s\" owned by Component \"%s\".\n", 
	  routine, _id, _owner->getID() );
	}
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Balance_print.cxx,v $";
 static char rcs_id2[] = "$Id: Balance_print.cxx,v 1.3 2006/10/26 15:11:21 hsu Exp $";}
/*  ===================================================  */

}
