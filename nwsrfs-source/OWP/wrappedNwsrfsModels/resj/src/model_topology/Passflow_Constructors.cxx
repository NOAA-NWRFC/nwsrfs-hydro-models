//------------------------------------------------------------------------------
// Passflow :: Passflow - Constructors.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
//  23 AUG 2004	KSH, OHD	Added this method
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Passflow.h"
#include "Reservoir.h"
#include "resj_main.h"

Passflow :: Passflow( Reservoir* owner ) : ReservoirMethod( owner )
{
   char routine[]="Passflow::constructor";
	if( owner == NULL ) {
	     if( getFewsDebugFlag() > 0 )
	     {
		 /* AVCP PrintWarning( 1, routine, */
		logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
	        "%s (%s) - Cannot set owner to NULL.", warnStr, routine );
	     }
	}
	initialize();

	// Set the _owner
	_owner = owner;
}

Passflow :: Passflow( const Passflow& meth, Reservoir* owner ) 
	: ReservoirMethod( meth, owner )
{
	char	routine[]="Passflow::Passflow";

	initialize();


/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Passflow_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: Passflow_Constructors.cxx,v 1.2 2006/10/26 15:28:46 hsu Exp $";}
/*  ===================================================  */

}
