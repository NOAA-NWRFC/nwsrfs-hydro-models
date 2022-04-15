/*****************************************************************************
 * Module Name: ssarresv_write_states_output()
 *
 * Original Author: AiVo
 *
 * Module Creation Date: 09/30/08
 *
 * Description:
 *    Replaces the last char in the string to \0
 *
 * Calling Arguments:
 *
 *    Name       		Input/Output    Type      Description
 *    args       		In              char*     string 
 *    outputStateFilePtr        In 		FILE*    passing the output states filename
 *    pArray                    In		float*   PO array
 *    cArray                    In		float*   Carry over array contains states information
 * Return Value: None
 *
 * Local Variables:
 *    Type       Name                      Description
 * int 		parmInfoPtr		   Parameter Information index pointer
 * int          reservoirTypeIndicator     Reservoir indicator
 * int          unit			   input units
 * Global Variables Used:
 *    Type       Name            Origin    Description
 *              
 * Constant and Macro Substitutions:
 *    Name                 	  Header File       Description
 *TWORESERVOIR	           	ssarresv.h	Two reservoir
 *STATIONBACKWATER		ssarresv.h	Station with backwater effect
 *                                               from a downstreamm reservoir
 *DS_RES_INST_IN_STRING		ssarresv.h	Intantaneous inflow at 
 *                                               start of run
 *DS_RES_INST_OUT_STRING	ssarresv.h	Instantaneous outflow for the 
 *                                               downstream reservoir 
 *                                               at start of run
 *DS_RES_ELEVATION_STRING	ssarresv.h	Elevation for the downstrem 
 *                                               reservoir
 *						 start of run      
 *DS_RES_STORAGE_STRING		ssarresv.h	Storage for the downstream 
 *                                              reservoir at start of run
 *US_RES_OUTFLOW_STRING		ssarresv.h	Instantaneous outflow for the 
 *                                               upstream station
 *US_RES_ELEVATION_STRING       ssarresv.h      Elevation for the upstream 
 *                                               reservoir
 *US_RES_STORAGE_STRING         ssarresv.h      Storage ofr the upstream 
 *                                               reservoir
 *TRIB_FLOW_INTO_DS_RES_STRING  ssarresv.h      Tributary flow into the 
 *                                               downstream reservoir
 *ENGLISH			ssarresv.h	Use English units 
 *METRIC 			ssarresv.h	Use Metric units
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/


#include <stdio.h>
#include <string.h>

#include "utilities.h"
#include "ssarresv.h"

void ssarresv_write_states_output( FILE *outputStateFilePtr, float *pArray, 
                                                             float *cArray )

{

      int parmInfoPtr;
      int reservoirTypeIndicator;
      int numberOfCarryoverValues;
      int unit;
      numberOfCarryoverValues = pArray[15 - 1]; 
      parmInfoPtr = pArray[11-1];
      unit = pArray[8-1];
      reservoirTypeIndicator = pArray[parmInfoPtr-1];
      writeFloatStateToFile( outputStateFilePtr, DS_RES_INST_IN_STRING, 
                                                 cArray, 1 );
      writeFloatStateToFile( outputStateFilePtr, DS_RES_INST_OUT_STRING, 
                                                 cArray, 2 );
      writeFloatStateToFile( outputStateFilePtr, DS_RES_ELEVATION_STRING, 
                                                 cArray, 3 );
      writeFloatStateToFile( outputStateFilePtr, DS_RES_STORAGE_STRING, 
                                                 cArray, 4 );
      writeFloatStateToFile( outputStateFilePtr, DS_RES_NOT_USED_STRING, 
                                                 cArray, 5 );   

     if( numberOfCarryoverValues == TWORESERVOIR )
     {

        writeFloatStateToFile( outputStateFilePtr, US_RES_OUTFLOW_STRING, 
	                                   cArray, 6 );
	writeFloatStateToFile( outputStateFilePtr, US_RES_ELEVATION_STRING, 
	                                   cArray, 7 );
	writeFloatStateToFile( outputStateFilePtr, US_RES_STORAGE_STRING, 
	                                   cArray, 8 );
	writeFloatStateToFile( outputStateFilePtr, TRIB_FLOW_INTO_DS_RES_STRING,
	                                   cArray, 9 );	

        if( reservoirTypeIndicator == STATIONBACKWATER )
	{
            writeFloatStateToFile( outputStateFilePtr, DS_RES_NOT_USED_STRING, 
	                                               cArray, 7 );
	    writeFloatStateToFile( outputStateFilePtr, DS_RES_NOT_USED_STRING, 
	                                               cArray, 8 );
        }
	  
     } 
     if( unit == ENGLISH )
     {
	  writeStringStateToFile( outputStateFilePtr, "UNIT", "ENGLISH" );	     
     }
     else
     {
	  writeStringStateToFile( outputStateFilePtr, "UNIT", "METRIC" );	
     }     



    
}
