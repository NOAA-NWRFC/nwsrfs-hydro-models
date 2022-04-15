//------------------------------------------------------------------------------
// LessThanEqualsExpr :: copy - recursively copies the expression.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 05 May 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "LessThanEqualsExpr.h"
#include "resj_main.h"

Expression* LessThanEqualsExpr :: copy()
{
	char		routine[]="LessThanEqualsExpr :: copy()";

	Expression	*ept=NULL, *lept=NULL, *rept=NULL;

	lept = _left_op->copy();

	if( !lept ){
		return( NULL );
	}

	rept = _right_op->copy();

	if( !rept ){
		if( lept ){
			delete lept;
		}
		return( NULL );
	}

	ept = new LessThanEqualsExpr( lept, rept );

	if( !ept ){
	   /* AVCP PrintWarning( 1, routine */
	   logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
	       "%s (%s) - Unable to copy expression \"%s\".",
	       warnStr, routine, toString() );
	   delete lept;
	   delete rept;
	   return( NULL );
	}

	return( ept );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_expression/RCS/LessThanEqualsExpr_copy.cxx,v $";
 static char rcs_id2[] = "$Id: LessThanEqualsExpr_copy.cxx,v 1.3 2006/10/26 15:23:14 hsu Exp $";}
/*  ===================================================  */

}
