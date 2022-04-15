//------------------------------------------------------------------------------
// SetWithdraw :: print - printd instance data members.
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
#include "SetWithdraw.h"
#include "TSDateIterator.h"

int SetWithdraw :: print( FILE* fp )
{
	char routine[] = "SetWithdraw :: print";

	if( fp == NULL ) {
		/*AVCP PrintWarning( 1, routine,*/
	        logMessageWithArgsAndExitOnError( FATAL_LEVEL,
	        "%s %s Cannot print SetWithdraw info -  null FILE*.",
	        errorStr, routine );
		/*AVCP return( STATUS_FAILURE );*/
	}

	// Print self id fnd owner first
	fprintf( fp, "SetWithdraw method \"%s\" owned by Component \"%s\".\n", 
		_id, _owner->getID() );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/SetWithdraw_print.cxx,v $";
 static char rcs_id2[] = "$Id: SetWithdraw_print.cxx,v 1.3 2006/10/26 15:35:23 hsu Exp $";}
/*  ===================================================  */

}
