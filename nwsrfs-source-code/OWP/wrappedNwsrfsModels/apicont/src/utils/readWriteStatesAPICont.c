/******************************************************************************
     Filename:  readWriteStatesAPICont.c

     Description:
     ============
     This file contains readStateApiCont(), and writeStatesApiCont() functions.
	
*****************************************************************************/

#include "apicont.h"

/* Maximum 7 state values (carryover) */
static char statesKey[][100] = { "ANTECEDENT_PRECIPITATION_INDEX",
                                 "SURFACE_MOISTURE_INDEX",
                                 "BASEFLOW_STORAGE_CONTENTS",
                                 "BASEFLOW_INDEX",
                                 "FIRST_QUADRANT_INDEX",
		                 "FROST_INDEX", 
		                 "FROST_EFFICIENCY_INDEX" };

/* Declare and initialize global variables that are used by this file. */
char unitStr[8];
const int NUMFOUR = 4;
const int NUMSEVEN = 7;

static int quadrantIndex = 0;
static int frozenGround = 0;

/*****************************************************************************
     Modules: readStatesApiCont()
     
        This function read the state values from statesI.txt then pass to EX 
     routine.
     
     Input: 
     -----
     float  poArray[]   - contains param information 

     Output:
     ------
     float  coArray[]   - contains input state (carryover) values

     Change History
     ==============

     DATE          VERSION    PROGRAMMERS           NOTES
     ----------    -------    -----------------     ----------------------
     05/04/09      1          Cham Pham             Initial implementation
                          
******************************************************************************/
void readStatesApiCont( float poArray[], float coArray[] )
{
   int   i;
   int   numbOfStates, stateUnit;

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "START readStatesApiCont() ...");
   }
   
   /* Read states information from statesI.txt file */
   readStatesFromFile( &numbOfStates, &stateUnit );
   
   if ( getFewsDebugFlag() > 4 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "stateunit (%d)\t#states (%d)", stateUnit, numbOfStates );
   }

   /* Read Unit string */
   sprintf( unitStr, "%s", "METRIC" );
   if ( stateUnit == 0 ) 
   {
      sprintf( unitStr, "%s", "ENGLISH" );
   }
   
   /* Get first quadrant variability option */
   quadrantIndex = (int)poArray[13];
   
   /* Get frozen ground information */
   frozenGround = (int)poArray[23];
   
   /* Populate the required carryover states */ 
   for (i = 0; i < NUMFOUR; i++ )
   {
      populateFloatStateValue( statesKey[i], coArray, i+1 );
   }

   /* Initialize state(s) */
   coArray[4] = 0.0;                 /* first quadrant index   */
   coArray[5] = 32.0;                /* frost index            */
   coArray[6] = 0.0;                 /* frost efficiency index */

   /* If not use week number then use AEI or ATI */
   if ( quadrantIndex > 0 )  
   {      
      populateFloatStateValue( statesKey[4], coArray, 5 );
   }
   
   /* If frozen ground considered */
   if ( frozenGround > 0 )
   {
      /* Get frost index (FI) */
      populateFloatStateValue( statesKey[5], coArray, 6 );
      
      /* Get frost efficiency index (FEI) */
      populateFloatStateValue( statesKey[6], coArray, 7 );
   }
   
   /* For debug purpose */
   if ( getFewsDebugFlag() > 4 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "State(s) pass to EX24 ...\n");
      for ( i = 0; i < NUMSEVEN; i++ )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
         "%s = %f\n", statesKey[i], coArray[i] );
      }
   }
     
   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "END readStatesApiCont() ..." );
   }
   
} /* readStatesApiCont() -------------------------------------------------- */
 

/*****************************************************************************
     Modules: writeStatesApiCont()

        This function write the state results that return from EX24 routine 
     into the statesO.txt file.
     
     Input: 
     -----
     float  coArray[]   - contains the carryover result
     char   *outputStatesFileName - state output file name (statesO.txt)
     
     Output:
     ------

     Change History
     ==============

     DATE          VERSION    PROGRAMMERS           NOTES
     ----------    -------    -----------------     ----------------------
     05/05/09      1          Cham Pham             Initial implementation
                          
******************************************************************************/
void writeStatesApiCont( float coArray[], char *outputStatesFileName )
{
   int i;

   FILE *outputStateFilePtr = NULL;

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "START writeStatesApiCont()..." );
   }
   
   /* Open state file */
   outputStateFilePtr = fopen( outputStatesFileName, "w+" );

   if ( outputStateFilePtr == NULL )
   {
      logMessageWithArgsAndExitOnError( FATAL_LEVEL,
      "ERROR: could not open the %s", outputStatesFileName );
   }

   /* Write out unit */
   writeStringStateToFile( outputStateFilePtr, "UNIT", (char*)unitStr );

   /* Write out the state(s) value to file */
   for ( i = 0; i < NUMSEVEN; i++ )
   {
      writeFloatStateToFile(outputStateFilePtr, statesKey[i], coArray, i+1 );
      
   }
   
   /* Close output state file */
   fclose( outputStateFilePtr );

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "END writeStatesApiCont()..." );
   }
      
} /* writeStatesApiCont() ------------------------------------------------- */
