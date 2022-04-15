//------------------------------------------------------------------------------
// ResJSys :: parseTimeSeries - top level code to parse out the time-series.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//------------------------------------------------------------------------------
// History:
// 
// 01 Apr 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 16 Apr 1998  Daniel Weiler, RTi	Added time step stuff.
// 14 May 2001	James R. VanShaar, RTi	Improved error handling
// 11 Oct 2001	JRV, RTi	Added check for alias length
// 14 Feb 2006	JRV, RTi	Set data units on input time series identifiers.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "ResJSys.h"
#include "HourTS.h"
#include "TSList.h"
#include "resj_main.h"


int ResJSys :: parseTimeSeries( char** ts_list, int nts_list )
{
	char		routine[]="ResJSys :: parseTimeSeries", **sub_list=NULL,
			words[512], units[512], units_dim[512], record[512],
			ts_string[512], **list = NULL;
	int		i=0, nsub_list=0, step_found = 0, nlist = 0, 
			ts_count=0;
	TS		*tspt=NULL;
	TSIdent		id;
	int		totErrs=0, curErrs;

	// NOTE on error handling:
	// 	All timeseries entries will be tested before any failure(s)
	//	cause termination of the function, so as to give the user the
	//	most complete description of input problems possible with one
	//	run.  To accomplish this, a counter of total errors is kept and
	//	checked prior to returning success.  Also, a counter of errors
	//	in the current timeseries is kept to enable function naviagation
	//	around commands that will fail due to dependency on the 
	//	successful execution of previous commands.  
	//	Errors are printed at the point they are located.

	// NOTE on carryover transfer use of this routine:
	//	Carryover transfer requires this routine to instantiate the
	//	reservoir systems (old and new).  Because the time series are
	//	static, certain difficulties arrise when the new system attempts
	//	to define time series identical to those defined by the old
	//	system.  Currently (7/2/01) no time series data is used in
	//	carryover transfer.  Therefore, this routine is run by the new
	//	system (to enable system instantiation) while ignoring any
	//	errors created as described above.  This is sufficient for 
	//	carryover transfer as it stands, but extensive tests as to the
	//	implications beyond carryover transfer have not been made.
	//	While this routine has no explicit knowlegde of how it was 
	//	called, the static variable ResJSys::_TSdefined is assigned to
	//	a non-zero value following successful completion.  This can
	//	be used should the routine be called later (for the new system)
	//	to avoid duplication of time series definition. Currently,
	//	only carryover transfer work will call this routine more than
	//	once.

	
	for( i=0; i<nts_list; i++ ) {
		curErrs = 0;
		/* AVCP PrintDebug( 20, routine, */
		if( getFewsDebugFlag() > 3 )
		{
		   logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
		   "(%s) - TimeSeries Line[%d]: %s", routine, i, ts_list[i] );
	        }
		
		list = BreakStringList( ts_list[i], " \t\n", 
			DELIM_SKIP_BLANKS, &nlist );
		// added the following check  - jgg 12/10/01
		if( list == NULL) {
		   /* AVCP PrintError( routine, ; */
		   logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		   "%s (%s) - TimeSeries list is malformed.",
		   errorStr, routine );			
		   //return( STATUS_FAILURE );
		}


		// Deal with the explicit time step declaration here
		if( !strcasecmp( list[0], "TIMESTEP" ) ) {
			// If there is only 1 value assume it is hourly base,
			// if there are two values then use them as a base and
			// mult, respectively. If there are more than two, then
			// ignore anything after the mult.
			if( nlist == 2 ) {
				if( !IsInteger( list[1] ) ) {
					/*AVCP curErrs++; totErrs++;
					 PrintError( routine, */
					logMessageWithArgsAndExitOnError( 
				        FATAL_LEVEL, "%s (%s) - Solution "
					"timestep '%s' is not an integer.",
					errorStr, routine, list[1] );
					
					/*AVCP list = FreeStringList( list );
					continue;*/
				}
				else if( Method :: 
					setTimeStep(atoi( list[1] ) ) ) {
					/*AVCP curErrs++; totErrs++;
					PrintError( routine, */
					logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		                        "%s (%s) - \"%s\" is "
					"not a valid hourly time step.",
					errorStr, routine, list[1]);					
					/*AVVP list = FreeStringList( list );
					continue;*/
					//return( STATUS_FAILURE );
				}
			}
			else {
				if( !( IsInteger( list[1] ) && 
					IsInteger( list[2] ) ) ) {
					/*AVCP curErrs++; totErrs++;
					PrintError( routine, */
					logMessageWithArgsAndExitOnError( 
				        FATAL_LEVEL, "%s (%s) - Both Solution "
					"timestep '%s' and multiplier "
					"'%s' must be integers.",
					errorStr, routine, list[1], list[2] );					
					/*AVCP list = FreeStringList( list );
					continue;*/
				}
				else if( Method :: setTimeStep(atoi( list[1] ),
					atoi( list[2]) ) ) {
					/*AVCP curErrs++; totErrs++;
				        PrintError( routine, */
					logMessageWithArgsAndExitOnError( 
				        FATAL_LEVEL, "%s (%s) - \"%s\" "
					"and/or \"%s\" are not valid "
					"interval and multiplier values.",
					errorStr, routine, list[1], list[2] );					
					/*AVCP list = FreeStringList( list );
					continue;*/
					//return( STATUS_FAILURE );
				}
			}
			step_found=1;
			list = FreeStringList( list );
			continue;
		}

		// If time step not explicitly set, print Error, set to 1 hour
		// then continue to check input syntax.
		if( !step_found ) {
			curErrs++; totErrs++;
			/*AVCP PrintError( routine, */
			logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
		        "%s (%s) - Must set the time step on line immediately "
			"after TIMESERIES keyword.  Continuing with "
			"timestep = 1 hr for further syntax check."
			"values.", warnStr, routine );
			
			if( Method :: setTimeStep( 1 ) ) {
				// If this happens, it's best just to bail out
				// because it should NEVER happen.
				/* AVCP PrintError( routine, */
				logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		                "%s (%s) - Default time step assignment failed.",
				errorStr, routine );				
				/*AVCP return( STATUS_FAILURE );*/
			}
			step_found=1;
		}

		if( !list || nlist != 5 ){
			/*AVCP curErrs++; totErrs++;
			PrintError( routine, */
			logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		        "%s (%s) - Line: \"%s\" is malformed."
			"values.", errorStr, routine, ts_list[i] );
			
			/*AVCP if( list ){
				list = FreeStringList( list );
			}
			continue;*/
		}

		// We have to allocate memory for a time-series object and 
		// save the identifier and alias.

		tspt = new HourTS();

		if( tspt == NULL ){
			/*AVCP curErrs++; totErrs++;
			PrintError( routine, */
			logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		        "%s (%s) - Unable to allocate memory for TimeSeries (%s).",
			errorStr, routine, ts_list[i] );			
			
			/*AVCP list = FreeStringList( list );
			continue; */
		}

		id = tspt->getIdentifier();

		if( id.setLocation( list[1] ) ){
			/*AVCP curErrs++; totErrs++;
		        PrintError( routine, */
			logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		        "%s (%s) - Troubles setting location for %s.",
		        errorStr, routine, ts_list[i] );
	
		}
		if( id.setType( list[2] ) ){
			/*AVCP curErrs++; totErrs++;
			PrintError( routine, */
			logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		        "%s (%s) - Troubles setting type for %s.",
			errorStr, routine, ts_list[i] );
		
		}

		// Check to see if it is an instantaneous data type.
		/*AVCP GetDataTypeInfo( list[2], words, units, units_dim, 
			record ); */

                getDimensionAndUnitAndTimeCode( list[2], units, units_dim,
		                                record );

                tspt->setDataUnits( units );

		if( !strcasecmp( record, "INST" ) ) {
			tspt->setInstantaneous();
		}

		if( !IsInteger( list[3] ) ) {
			/*AVCP curErrs++; totErrs++;
			PrintError( routine, */
			logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		        "%s (%s) - Timeseries '%s %s' interval "
			"value '%s' is not an integer.",
			errorStr, routine, list[i], list[2], list[3] );
			
		}
		else if( id.setInterval( TS :: INTERVAL_HOUR, 
			atoi( list[3] ) ) ){
			/*AVCP curErrs++; totErrs++;
		        PrintError( routine, */
			logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		        "%s (%s) - Troubles setting interval for %s.",
			errorStr, routine, ts_list[i] );
			
		}

		// We have to check that this identifier hasn't been used
		// yet...

		if( TSList :: getTSFromList( id.getIdentifier() ) != NULL ) {
			// Only act on error if this is the first time through
			// the routine (non-carryover transfer)
			if ( !ResJSys::get_TSdefined() ) {
				/*AVCP curErrs++; totErrs++;
				PrintError( routine, */
				logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		                "%s (%s) - Multiple timeseries '%s' exist.  "
				"Duplicates not allowed.",
			        errorStr, routine, id.getIdentifier() );
				
			}
		}

		if( strlen(list[4]) > 12 ) {
			/*AVCP curErrs++; totErrs++;
			PrintError( routine, */
			logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		        "%s (%s) - Time series alias for %s is too long (> 12 "
			"chars).", errorStr, routine, ts_list[i] );
			
		}
		if( id.setAlias( list[4] ) ){
			/*AVCP curErrs++; totErrs++;
			PrintError( routine, */
			logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		        "%s (%s) - Troubles setting alias for %s.",
			errorStr, routine, ts_list[i] );
			
		}
		if( TSList :: getTSFromList( id.getAlias() ) != NULL ){
			// Only act on error if this is the first time through
			// the routine (non-carryover transfer)
			if ( !ResJSys::get_TSdefined() ) {
				/*AVCP curErrs++; totErrs++;
				PrintError( routine, */
				logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		                "%s (%s) - Multiple timeseries with alias '%s' "
			        "exist.  Aliases must be unique.",
			        errorStr, routine, id.getAlias() );
			}	
		}

		if ( curErrs > 0 ) {
			list = FreeStringList( list );
			delete( tspt );
			continue;
		}

		// At this point, everything seems OK with the time series 
		// identifier so we now have to add this id to the _po_list in 
		// a very specific format depending on whether this is an input
		// or an output TS
		sprintf( ts_string, "%-8.8s%-4.4s%4.2s%4.2s", list[1],
			list[2], list[3], list[0] );

		if( addPOString( ts_string ) ) {
			/*AVCP curErrs++; totErrs++;
			PrintError( routine, */
			logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		        "%s (%s) - Troubles adding string to "
			"pin58 PO array.", errorStr, routine);
			
		}

		if( tspt->setIdentifier( id ) ){
			/*AVCP curErrs++; totErrs++;
			PrintError( routine, */
			logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		        "%s (%s) - Troubles setting identifier for %s.",
			errorStr, routine, id.getIdentifier() );
		
		}

		// Now we have to set the dates and allocate memory for the 
		// time-series. If we are just checking the syntax here then
		// we are just testing the availability of the memory, otherwise
		// we will fill in these areas as needed.
		tspt->setDate1( _t1 );
		tspt->setDate1Original( _t1 );
		tspt->setDate2( _t2 );
		tspt->setDate2Original( _t2 );

		tspt->setDataInterval( id.getIntervalBase(), 
			id.getIntervalMult() );

		// Ensure output timeseries have the same timestep as the 
		// simulation timestep
		if(!strncasecmp( list[0], "OUT", 3 ) && 
			( atoi( list[3] ) != Method :: getTimeMult() ) ) {
			/*AVCP curErrs++; totErrs++;
			PrintError( routine, */
			logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		        "%s (%s) - Output time series %s must "
			"have the same time step as simulation (%d "
			"hours).", errorStr, routine, Method::getTimeMult() );
			
		}

		if ( curErrs > 0 ) {
			list = FreeStringList( list );
			delete( tspt );
			continue;
		}

		// If we arrive at this point, everything is sufficiently in
		// order to add it to our list.
		if( TSList :: addTSToList( tspt, list[0] ) ){
		    /*AVCP totErrs++;
		    PrintError( routine, */
		    logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		    "%s (%s) - Troubles adding %s to TSList.",
		    errorStr, routine, id.getIdentifier());			
		}
		list = FreeStringList( list );

	}

	if ( totErrs > 0 ) {
		return ( STATUS_FAILURE );
	}

	ResJSys::set_TSdefined();
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ResJSys_parseTimeSeries.cxx,v $";
 static char rcs_id2[] = "$Id: ResJSys_parseTimeSeries.cxx,v 1.5 2006/10/26 15:31:22 hsu Exp $";}
/*  ===================================================  */

}
