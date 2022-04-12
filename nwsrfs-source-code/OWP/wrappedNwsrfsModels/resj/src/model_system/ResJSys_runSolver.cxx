//------------------------------------------------------------------------------
// ResJSys :: runSolver - this function runs the solver...
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 02 Apr 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ResJSys.h"
#include "Solver.h"
#include "resj_main.h"

int _totNumInterval; //bug 562 5/29/12

int ResJSys :: runSolver()
{
	int	ierr;
	char	routine[]="ResJSys :: runSolver";
	Solver	solver;

	_totNumInterval = 1; //bug 562 5/29/12

	ierr= solver.run( _root, _t1, _t2, 
		Method :: getTimeInterval(), Method :: getTimeMult(), 1 );

	if( ierr ){
	       
		/* AVCP PrintWarning( 1, routine */
		logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
		"%s (%s) TROUBLES RUNNING SOLVER.", warnStr, routine );
		return( STATUS_FAILURE );
	}

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ResJSys_runSolver.cxx,v $";
 static char rcs_id2[] = "$Id: ResJSys_runSolver.cxx,v 1.4 2006/10/26 15:31:31 hsu Exp $";}
/*  ===================================================  */

}
