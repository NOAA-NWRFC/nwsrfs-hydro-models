//------------------------------------------------------------------------------
// Solver :: run - main entry point for the Solver.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
  // History:
// 
// 19 Mar 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 01 May 1998	Daniel Weiler, RTi	Took out the call to finalize,
//					moved that to the solve at the 
//					Component level.
// 03 Jun 1998  DKW			Added carryover date stuff. 
// 12 Jun 2002	James R. VanShaar, RTi	Added setEndOfTimestepStates
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------
#include "Solver.h"
#include "ResJSys.h"
#include "resj_main.h"

extern int _totNumInterval;  //bug #562 5/29/12

int Solver :: run( 	Component* root, TSDate& t1, TSDate& t2, int base, 
			int mult, int isPrimary )
{
	char	routine[]="Solver :: run";
	int	count=0, nintv=0;
	TSDate	t;

	if( root == NULL ){
	      /* AVCP PrintError( routine, */
	      logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
	      "%s (%s) - Incoming topology root is NULL, cannot continue.",
	      errorStr,  routine );
		
	   //return( STATUS_FAILURE );
	}
	// Check the dates...
	if( t1 > t2 ){
	      /* AVCP PrintError( routine, */
	      logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
	      "%s (%s) - Start date %s is after end date %s.",
	      errorStr, routine, t1.toString(), t2.toString());
		
	   //return( STATUS_FAILURE );
	}
//	for( count=0, t=t1; t<=t2; t.addInterval( base, mult ), count++ );

//	nintv = count;

//	for( count=0, t=t1; t<=t2; t.addInterval( base, mult ), count++ ){

	for( t=t1; t<=t2; t.addInterval( base, mult )){
		// The first thing we have to do is set the current date for
		// use with the expressions...
		Expression :: setCurrentDate( t );
		// Check to see if this date matches up with any in the 
		// _co_dates list, and set the flag at the Component level
		// accordingly.
		if( isPrimary ) {
			Component :: setCOFlag( ResJSys :: isCarryoverDate( t ) ); 

		}
		// Now we have to solve the system for this particular 
		// time step. This method works recursively so we only have to
		// do is call is on the root of the topology tree. When this
		// method returns it means that the whole topology tree has
		// been solved for this time step.
		if( root->solve( t, isPrimary ) ){
			// If we fail here we have big problems since we are
			// in the middle of the solution.
			/* AVCP PrintWarning( 1, routine */
	                logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
		       	"%s (%s) - Solution failed at %s.", warnStr, routine,
			t.toString());
			
			return( STATUS_FAILURE );
		}

		_totNumInterval++;  //bug #562 5/29/12

		// Update the timestep states and test / execute carryover
		// handling
		root->setEndOfTimestepStates( t );

		// Reset the Carryover tools on the ResJSys level
		if( isPrimary && ResJSys :: isCarryoverDate( t ) ) {
			ResJSys :: resetCOString();
		}

//		if( (count % 75) == 0 ){
//		}
	}
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_solver/RCS/Solver_run.cxx,v $";
 static char rcs_id2[] = "$Id: Solver_run.cxx,v 1.6 2006/10/26 15:35:53 hsu Exp $";}
/*  ===================================================  */

}
