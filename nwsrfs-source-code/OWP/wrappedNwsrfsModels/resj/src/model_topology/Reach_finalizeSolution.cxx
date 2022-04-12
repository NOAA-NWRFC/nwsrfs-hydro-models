//------------------------------------------------------------------------------
// Reach :: finalizeSolution - does solution cleanup at the end of a 
//				   time-step.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 31 Mar 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 15 Apr 1998	Daniel Weiler		Added TSDate& argument.
// 19 Feb 2004	JRV, RTi	Replaced calls to sumInflow with calls to 
// 				getTotalInflow.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Reach.h"
#include "resj_main.h"

int Reach :: finalizeSolution( TSDate& cur_date )
{
	char routine[] = "Reach :: finalizeSolution";
	double cur_inflow;
	int i;	

	if( _outflow == MISSING ) {
	   /* AVCP totErrs++;		
	   PrintError( routine, */
	   logMessageWithArgsAndExitOnError( FATAL_LEVEL,
	       "%s (%s) Outflow on reach '%s' is missing.  "
		"Please ensure a routing method is active." ,
		errorStr, routine, _id );
	   /*AVCP	return( STATUS_FAILURE );*/
	}
	TSIdent tempJRV = _outflow_ts.getIdentifier();
	_outflow_ts.setDataValue( cur_date, _outflow );

	cur_inflow = getTotalInflow( cur_date );
	/* AVCP PrintDebug( 5, routine */
	if( getFewsDebugFlag() > 3 )
	{
	   logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	    "(%s) TIME  \"%s\".Reach  \"%s\"."
	    "Inflow   %f Outflow   %f",
	     routine, cur_date.toString(), _id ,cur_inflow , _outflow );
	}
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Reach_finalizeSolution.cxx,v $";
 static char rcs_id2[] = "$Id: Reach_finalizeSolution.cxx,v 1.4 2006/10/26 15:30:21 hsu Exp $";}
/*  ===================================================  */

}
