/******************************************************************************
     Filename:  readWriteStates.c

     Description:
     ============
        This file contains readStateSacsmaht(), and writeStatesSacsmaht().
	
*****************************************************************************/

#include "sacsmaht.h"

/* Required 6 carryover states */
static char statesKey[][100] = { 
                  "UPPER_ZONE_TENSION_WATER_CONTENTS_UZTWC",
                  "UPPER_ZONE_FREE_WATER_CONTENTS_UZFWC",
                  "LOWER_ZONE_TENSION_WATER_CONTENTS_LZTWC",
                  "LOWER_ZONE_FREE_SECONDARY_WATER_CONTENTS_LZFSC",
                  "LOWER_ZONE_FREE_PRIMARY_WATER_CONTENTS_LZFPC",
                  "ADDITIONAL_IMPERVIOUS_AREA_TENSION_WATER_CONTENTS_ADIMC" };

/* Additional carryover states */                          
static char additionalStatesKey[][100] = { "RUNOFF_SUM_RMS_", "PE_PPX",
                                           "SNOW_COVER_PPSC" };

/* Frozen ground carryover: fgco(1) ... fgco(6) */
static char fgcoKey[][100] = { "FROST_INDEX",
                               "UPPER_ZONE_TENSION_UNFROZEN_WATER",
	   		       "UPPER_ZONE_FREE_UNFROZEN_WATER",
			       "LOWER_ZONE_TENSION_UNFROZEN_WATER",
			       "LOWER_ZONE_FREE_SECONDARY_UNFROZEN_WATER",
	  		       "LOWER_ZONE_FREE_PRIMARY_UNFROZEN_WATER" };

/* Frozen ground carryover: pta, pwe, psh */
static char initStatesKey[][100] = { "INIT_AIR_TEMPERATURE",
                                     "INIT_SNOW_WATER_EQUIVALENT",
                                     "INIT_SNOW_DEPTH" };

static char numSoilLayerKey[] = "NUMBER_SOIL_LAYER";

/* Frozen ground carryover: fgpm(numsoilLayer), tsoil(numsoilLayer),
 * smc(numsoilLayer), sh2o(numSoilLayer) - max numSoilLayer = 8 */
static char soilLayers[][100] = { "DEPTH_TO_BOTTOM_OF_LAYER_",
                                  "SOILLAYER_TEMPERATURE_TSOIL_",
                                  "SOILLAYER_MOISTURE_CONTENT_SMC_",
                                  "SOILLAYER_UNFROZEN_WATER_SH2O_" };
char *unitStr = NULL;
const int NUMSEVEN = 7;
const int NUMSIX = 6;
const int NUMFOUR = 4;
const int NUMTHREE = 3;
const int NUMTWO = 2;

/*****************************************************************************
     Modules: readStatesSacsmaHt()
     
        This function read the state values from statesI.txt then pass to EX 
     routine.
     
     Input: 
     -----
     float  poArray[]   - contains param information 

     Output:
     ------
     float  coArray[]   - contains input state values

     Change History
     ==============

     DATE          VERSION    PROGRAMMERS           NOTES
     ----------    -------    -----------------     ----------------------
     02/18/09      1          Cham Pham             Initial implementation
                          
******************************************************************************/
void readStatesSacsmaHt( float poArray[], float coArray[] )
{
   int   i, nco;
   int   numbOfStates, stateUnit;
   int   numSoilLayer, irxc;
   float Funit[7];

   if ( getFewsDebugFlag() > 4 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "start readStatesSacsmaHt...");
   }

   /* Read states information from statesI.txt file */
   readStatesFromFile( &numbOfStates, &stateUnit );
   
   if ( getFewsDebugFlag() > 4 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "stateunit (%d)\t#states (%d)", stateUnit, numbOfStates );
   }
   
   /* Read Unit string ALWAYS 'METRIC' */
   unitStr = populateStringStateValue( "UNIT", Funit, 1, strlen("METRIC") );
   
   /* Populate the required carryover states (state(1) - state(6)) */ 
   for (i = 0; i < NUMSIX; i++ )
   {
      populateFloatStateValue( statesKey[i], coArray, i+1 );

      if ( getFewsDebugFlag() > 4 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
         "%s = %f\n", statesKey[i], coArray[i] );
      }
   }
   nco = NUMSIX;

   /* Populate the additional carryover states - rsm(7), ppx, ppsc */
   irxc = (int)poArray[11];
   
   if ( irxc > 0 )  
   {
      /* rms(7) */
      populateArrayOfFloatStateValues( additionalStatesKey[0], NUMSEVEN, 
	                               coArray, (nco+ 1) );
      if ( getFewsDebugFlag() > 4 )
      {
         for (i = 0; i < NUMSEVEN; i++ )
          logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
          "%s = %f\n", additionalStatesKey[0], coArray[i] );
      }
      
      nco += NUMSEVEN;
      
      /* ppx, ppsc */
      populateFloatStateValue( additionalStatesKey[1], coArray, nco + 1 );
      populateFloatStateValue( additionalStatesKey[2], coArray, nco + 2 );

      if ( getFewsDebugFlag() > 4 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
         "%s = %f\n", additionalStatesKey[1], coArray[nco] );
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
         "%s = %f\n", additionalStatesKey[2],coArray[nco+1] );
      }

      nco += NUMTWO;  
   }
   
   /* Read number of soil layer */
   populateIntStateValue( numSoilLayerKey, coArray , nco + 1 );      
   numSoilLayer = (int)coArray[nco];

   nco++;
 
   /* Frozen ground carryover - fgco(6), pta, pwe, and psh */
   /* fgco(1) - fgco(6) */
   for ( i = 0;  i < NUMSIX; i++ )
   {
      populateFloatStateValue( fgcoKey[i], coArray, nco + i + 1);
	 
      if ( getFewsDebugFlag() > 4 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
         "%s = %f\n", fgcoKey[i], coArray[nco+i] );
      }
   }
   
   nco += NUMSIX;
     
   /* Initial values of temperature (pta), snow water equivalent (pwe), 
    * and snow depth (psh)
    */
   for ( i = 0; i < NUMTHREE; i++ )
   {
      populateFloatStateValue( initStatesKey[i], coArray, nco + i + 1 );

      if ( getFewsDebugFlag() > 4 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
         "%s = %f\n", initStatesKey[i], coArray[nco+i] );
      }
   }
      
   nco += NUMTHREE;
   
   /* Frozen ground carryover - fgpm(numSoilLayer), tsoil(), smc(), sh2o() */
   if ( numSoilLayer > 0 )
   {
      for (i = 0; i < NUMFOUR; i++ )
      {
         populateArrayOfFloatStateValues( soilLayers[i], numSoilLayer, 
  	                                  coArray, (nco + i + 1) );
         if ( getFewsDebugFlag() > 4 )
         {
	    int j;
	    for (j = 0; j < numSoilLayer; j++ )
                logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	        "%s_#%d = %f\n", soilLayers[i], j, coArray[nco+j] );
	 }
	    
         nco += (numSoilLayer - 1);	   
      }
   }/*end if (numSoilLayer > 0 ) */
     
   if ( getFewsDebugFlag() > 4 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "End readStatesSacsmaHt()..." );
   }
   
} /* readStatesSacsmaht() -------------------------------------------------- */
 

/*****************************************************************************
     Modules: writeStatesSacsmaHt()

        This function write the state results that return from EX routine into
     the statesO.txt file.
     
     Input: 
     -----
     float  poArray[]   - contains param information 
     float  coArray[]   - contains the result carryover
     char   *outputStatesFileName - state output file name (statesO.txt)
     
     Output:
     ------

     Change History
     ==============

     DATE          VERSION    PROGRAMMERS           NOTES
     ----------    -------    -----------------     ----------------------
     02/18/09      1          Cham Pham             Initial implementation
                          
******************************************************************************/
void writeStatesSacsmaHt( float poArray[], float coArray[], 
                          char *outputStatesFileName )
{
   int i, ixrc, numSoilLayer, nco;

   FILE *outputStateFilePtr = NULL;

   if ( getFewsDebugFlag() > 4 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "Start writeStatesSacsmaHt()..." );
   }
   
   /* Open state file */
   outputStateFilePtr = fopen( outputStatesFileName, "w+" );

   if ( outputStateFilePtr == NULL )
   {
      logMessageWithArgsAndExitOnError( FATAL_LEVEL,
      "ERROR: could not open the %s\n", outputStatesFileName );
   }

   /* Write out unit */
   writeStringStateToFile( outputStateFilePtr, "UNIT", "METRIC" );

   /* Write out the state(1) ... state(6) */
   for ( i = 0; i < NUMSIX; i++ )
   {
      writeFloatStateToFile(outputStateFilePtr, statesKey[i], coArray, i+1 );
   }
   
   /* Write out the additional carryover states - rsm(7), ppx, ppsc */
   nco = NUMSIX;
   ixrc = (int)poArray[11];

   if ( ixrc > 0 )
   {
      writeArrayOfFloatStatesToFile( outputStateFilePtr, additionalStatesKey[0],
	                             coArray, (nco+1), NUMSEVEN );
      nco += NUMSEVEN;
      writeFloatStateToFile( outputStateFilePtr, additionalStatesKey[1], 
	                     coArray, nco + 1 );
      writeFloatStateToFile( outputStateFilePtr, additionalStatesKey[2],
                             coArray, nco + 2 );
      nco += NUMTWO;                                              	    
   }
   
   /* Write out number of soil layer */
   numSoilLayer = (int)coArray[nco];
   
   writeFloatStateToFile( outputStateFilePtr, numSoilLayerKey, coArray,
                          nco + 1 );      
      
   nco ++;   /* Increment number of carryover index */
      
   /* Frozen ground carryover - fgco(6), pta, pwe, and psh */
   for ( i = 0; i < NUMSIX; i++ )
   {
      writeFloatStateToFile( outputStateFilePtr, fgcoKey[i], coArray, 
                             (nco+i+1) );
   }
      
   nco += NUMSIX;
      
   for ( i = 0; i < NUMTHREE; i++ )
   {
      writeFloatStateToFile( outputStateFilePtr, initStatesKey[i], coArray,
                             (nco+i+1) );
   }
   nco += NUMTHREE;

   if ( numSoilLayer > 0 )
   {
      for (i = 0; i < NUMFOUR; i++ )
      {
         writeArrayOfFloatStatesToFile( outputStateFilePtr, soilLayers[i], 
	                                coArray, (nco + i + 1), numSoilLayer );
	 nco += (numSoilLayer - 1);	   
      }
   } /* end if(numsoillayer > 0 ) */ 
      
   /* Close output state file */
   fclose( outputStateFilePtr );

   if ( getFewsDebugFlag() > 4 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "End writeStatesSacsmaHt()..." );
   }
      
} /* writeStatesSacsmaht() ------------------------------------------------- */
