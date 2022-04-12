//------------------------------------------------------------------------------
// Adjust :: construct - Reads a stringlist and constructs precip and/or
//				evap timeseries data.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 13 Feb 1998	Daniel K. Weiler, Riverside Technology, inc
//					Created initial version.
// 24 Sep 1998	DKW,		Moved the input (obs) release TS to be
//				stored on the Method as opposed to the 
//				Reservoir.
// 24 Nov 2001	JRV, RTi	Allowed for carry over  blendts step 
//				values
// 27 Nov 2001	JRV, RTi	Improved error and warning handling
// 27 Nov 2001  JRV, RTi        Revised usage of BLENDTS to handle using defaults
//				for _n_tstep if it is not explicity defined.
// 01 Aug 2002  KSH, HRL        Added BLENDTS keyword to indicate missing data
//                              blending. BLEND is retained for old datasets.
// 14 Oct 2002	JRV, RTi	Added handling of ADJSIM keyword.
// 2003-11-21 Luiz Teixeira, RTi - Added list = FreeStringList( list ) at the 
//  				end of the main for loop in the construct method
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include <stdio.h>
#include "resj_main.h"
#include "Adjust.h"
#include "TSUtil.h"

int Adjust :: construct ( char** re_list, int n_items )  
{
	char routine[] = "Adjust :: construct", **list = NULL,
		ts_id[MAXC] = "";
	int i, j, nlist = 0, totErrs = 0;

	for( i = 0; i < n_items; i++ ) {
		if( strlen( re_list[i] ) == 0 || re_list[i][0] == '#' ){
			continue;
		}
		list = BreakStringList( re_list[i], " \n\t", 
			DELIM_SKIP_BLANKS, &nlist );
		if( !list || nlist == 0 ) {
			if( list ) {
				list = FreeStringList( list );
			}
			/*AVCP PrintError( routine,*/
		         logMessageWithArgsAndExitOnError( FATAL_LEVEL,
			       "%s (%s) - Troubles constructing %s "
			       "on %s - ADJUST method malformed.", errorStr,
			       routine, _id, _owner->_id );
			/*AVCP continue;*/
		}

		// Check for BLENDTS
		else if( !strcasecmp( list[0], "BLEND" ) ||
		         !strcasecmp( list[0], "BLENDTS" ) ) {
			if( nlist > 1 ) {
				if( !IsInteger( list[1] ) ) {
					//totErrs++;
					/*AVCP PrintError( routine,*/
				        logMessageWithArgsAndExitOnError(
					FATAL_LEVEL,
				      	"%s (%s) - %s value '%s' is "
					"not an integer.", errorStr,
					routine, list[0], list[1] );
					/*AVCP list = FreeStringList( list );
					continue;*/
				}
				_n_blend = atoi( list[1] );
				_n_tstep = _n_blend + 1;
			}
			else {
				//totErrs++;
				/*AVCP PrintError( routine,*/
			        logMessageWithArgsAndExitOnError( FATAL_LEVEL,
				"%s (%s) - Value required "
				"immediately after %s method keyword %s.", 
				errorStr, routine, _type, list[0] );
				/*AVCP list = FreeStringList( list );
				continue; */
			}
			if( nlist > 2 ) {
				if( !IsInteger( list[2] ) ) {
					//totErrs++;
					/*AVCP PrintError( routine,*/
				        logMessageWithArgsAndExitOnError(
					FATAL_LEVEL,
				      	"%s (%s) - %s value '%s' is "
					"not an integer.", errorStr, routine,
					list[0], list[2] );
					/*AVCP list = FreeStringList( list );
					continue;*/
				}
				_n_tstep = atoi( list[2] );
			}
			list = FreeStringList( list );
			continue;
		}

		// Check for input time series - the only applicable inputs for 
		// this method would be observed release or pool TS.
		if( !strcasecmp( list[0], "TSINPUT" ) ) {
			// Read either full identifiers or alias 
			strcpy( ts_id, list[2] );
			for( j = 3; j < nlist; j++ ) {
				strcat( ts_id, list[j] );
				strcat( ts_id, " " );
			}
			if( !strcasecmp( list[1], "OBSERVEDRELEASE" ) ) {
				_release_obs = (HourTS*)TSList :: getTSFromList( 
					ts_id );
				if( _release_obs == NULL ) {
					//totErrs++;
					/*AVCP PrintError( routine,*/
				        logMessageWithArgsAndExitOnError(
				        FATAL_LEVEL, "%s (%s) - Could not "
					"find %s as a valid time series in "
					"%s %s.", errorStr, routine, 
					ts_id, _type, _id );
					/*AVCP list = FreeStringList( list );
					continue;*/
				}
			}
			else if( !strcasecmp( list[1], "OBSERVEDPOOL" ) ) {
				_pool_obs = (HourTS*)TSList :: getTSFromList( 
					ts_id );
				if( _pool_obs == NULL ) {
					//totErrs++;
					/*AVCP PrintError( routine,*/
				        logMessageWithArgsAndExitOnError( 
				        FATAL_LEVEL, "%s (%s) - Could not "
					"find %s as a valid time series in "
					"%s %s.", errorStr, routine, 
					ts_id, _type, _id );
					/*AVCP list = FreeStringList( list );
					continue; */
				}
			}
			else {
				/*AVCP PrintWarning( 1, routine,*/
			   if( getFewsDebugFlag() > 0 )
			   {
			       // logMessageWithArgsAndExitOnError( FATAL_LEVEL,
                               logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
				"%s (%s) - %s not a recognized "
				"Input time series for %s method %s.",
				warnStr, routine, list[1], _type, _id );
			   }
			}
			list = FreeStringList( list );
			continue;
		}

		// Check for ADJSIM
		else if( !strcasecmp( list[0], "ADJSIM" ) ) {
			if( nlist > 1 ) {
				if( !strcasecmp( list[1], "OFF" )) {
					_adjsim = 0;
				}
				else if( strcasecmp( list[1], "ON" )) {
					//totErrs++;
					/*AVCP PrintError( routine,*/
				        logMessageWithArgsAndExitOnError(
					FATAL_LEVEL, "%s (%s) - Unrecognized "
					"keyword immediately after %s "
					"method keyword %s.", errorStr, routine,
					_type, list[0] );
					/*AVCP list = FreeStringList( list );
					continue;*/
				}
			}
			else {
				//totErrs++;
				/*AVCP PrintError( routine,*/
			       logMessageWithArgsAndExitOnError( FATAL_LEVEL,
			       "%s (%s) - Keyword required immediately after "
			       "%s method keyword %s.", errorStr, routine,
			       _type, list[0] );
				/*AVCP list = FreeStringList( list );
				continue; */
			}
			list = FreeStringList( list );
			continue;
		}
		
		// Freeing memory
		if ( list ) {
			list = FreeStringList( list );  // 2003-11-21 LT
		}
	}

	// Ensure you have at least one observed time series
	if( !( _release_obs || _pool_obs ) ) {
		//totErrs++;
		/*AVCP PrintError( routine,*/
	       logMessageWithArgsAndExitOnError( FATAL_LEVEL,
	       "%s (%s) - %s method %s must have at least one "
	       "observed time series.", errorStr, routine, _type, _id );
	}

	/*AVCP if ( totErrs > 0 ) {
	        logMessageWithArgsAndExitOnError( FATAL_LEVEL,
		"%s (%s) - Total error %d", errorStr, routine, totErrs );      
		return( STATUS_FAILURE );
	}*/

	if( _n_blend == -1 ) {
                _n_blend = 1;
	}

	// We have to write the carryover inflow to the CO array at the System
	// level...
	setCOstring();

	_is_constructed = 1;
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Adjust_construct.cxx,v $";
 static char rcs_id2[] = "$Id: Adjust_construct.cxx,v 1.8 2006/10/26 15:08:15 hsu Exp $";}
/*  ===================================================  */

}		


