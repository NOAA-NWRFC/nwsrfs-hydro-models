//------------------------------------------------------------------------------
// ResJSys Operators
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 08 Jan 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ResJSys.h"
#include "resj_main.h"

void ResJSys :: operator= ( const ResJSys& rjs )
{
	char	routine[]="ResJSys :: operator= ";
        /* AVCP PrintWarning( 1, routine */
        logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
        "%s (%s) - routine is not currently enabled.", warnStr, routine);

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ResJSys_Operators.cxx,v $";
 static char rcs_id2[] = "$Id: ResJSys_Operators.cxx,v 1.3 2006/10/26 15:30:49 hsu Exp $";}
/*  ===================================================  */

}
