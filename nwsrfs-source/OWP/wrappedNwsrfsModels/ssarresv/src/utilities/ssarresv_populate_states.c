/*****************************************************************************
 * Module Name: ssarresv_populate_states()
 *
 * Original Author: AiVo
 *
 * Module Creation Date: 09/30/08
 *
 * Description:
 *    read states values and store it into P array
 *levation f
 * Calling Arguments:
 *
 *    Name                     Input/Output    Type      Description
 *            
 *    numberOfCarryoverValues	input		int      single reservoir /two reservoir sys
 *    reservoirTypeIndicator	input		int      Indicates reservoir backwater or 
 *                                                       station backwater
 *    cArray			output		char[]   Carry over array 
 * Return Value: None
 *
 * Local Variables:
 *    Type       Name                      Description
 *
 * Global Variables Used:
 *    Type       Name            Origin    Description
 *
 * Constant and Macro Substitutions:
 *    Name                 	  Header File       Description
 *TWORESERVOIR	           	ssarresv.h	Two reservoir
 *STATIONBACKWATER		ssarresv.h	Station with backwater effect from a
 *                                              downstreamm reservoir
 *DS_RES_INST_IN_STRING		ssarresv.h	Intantaneous inflow at start of run
 *DS_RES_INST_OUT_STRING	ssarresv.h	Instantaneous outflow for the 
 *                                               downstream reservoir at start of run
 *DS_RES_ELEVATION_STRING	ssarresv.h	Elevation for the downstrem reservoir
 *						 start of run      
 *DS_RES_STORAGE_STRING		ssarresv.h	Storage for the downstream reservoir at start 
 *                                               of run
 *US_RES_OUTFLOW_STRING		ssarresv.h	Instantaneous outflow for the upstream station
 *US_RES_ELEVATION_STRING       ssarresv.h      Elevation for the upstream reservoir
 *US_RES_STORAGE_STRING         ssarresv.h      Storage ofr the upstream reservoir
 *TRIB_FLOW_INTO_DS_RES_STRING  ssarresv.h      Tributary flow into the downstream reservoir
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

void ssarresv_populate_states( float cArray[], int numberOfCarryoverValues,
                        int reservoirTypeIndicator)
{
     populateFloatStateValue(DS_RES_INST_IN_STRING,&cArray[0],1);
     populateFloatStateValue(DS_RES_INST_OUT_STRING,&cArray[1],1);
     populateFloatStateValue(DS_RES_ELEVATION_STRING,&cArray[2],1);
     populateFloatStateValue(DS_RES_STORAGE_STRING,&cArray[3],1);
     populateFloatStateValue(DS_RES_NOT_USED_STRING,&cArray[4],1);

    
     if (numberOfCarryoverValues == TWORESERVOIR) 
     {	 
       
	 populateFloatStateValue(US_RES_OUTFLOW_STRING,&cArray[5],1);
	 populateFloatStateValue(US_RES_ELEVATION_STRING,&cArray[6],1);
	 populateFloatStateValue(US_RES_STORAGE_STRING,&cArray[7],1);
	 populateFloatStateValue(TRIB_FLOW_INTO_DS_RES_STRING,&cArray[8],1);	
         if( reservoirTypeIndicator == STATIONBACKWATER )
	 {
             populateFloatStateValue(DS_RES_NOT_USED_STRING,&cArray[6],1);
	     populateFloatStateValue(DS_RES_NOT_USED_STRING,&cArray[7],1);
         }   
     }

}
