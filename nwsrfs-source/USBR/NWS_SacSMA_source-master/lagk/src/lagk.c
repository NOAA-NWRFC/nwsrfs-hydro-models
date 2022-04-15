/******************************************************************************
   Filename: lagk.c
   
   Description:
   ============

   The main function for the LAG-K operation (#7)
      1. Read the arguments passed in from the java
      2. Read the parameter file using the pin routine
      3. Perform carryover transfer if needed.
      4. Allocate memory needed for P and C arrays
      5. Set up any common blocks
      6. Read the time series file to read the data
      7. Retriev TS data
      8. Call the ex routine with appropriate params and data
      9. Write results 
    
    Module(s):
      getRequiredInputTimeSeries()
      readStateLagK()
      writeStateLagK()

   Inputs: 
      
   Outputs:

   Change History
   ==============
   DATE          VERSION    PROGRAMMERS        NOTES
   ----------    -------    -----------------  ----------------------
   05/09/08      1          Freddy C./Lee C.   Initial implementation
   01/28/09                 Cham P.            Refactored codes to read and 
                                               write states. Implemented the 
					       carryover transfer.
   06/24/09                 Cham P.            dynamically allocate the size 
                                               of input and output time series
	                                       array.			       
   10/05/09                 Cham P.            Commented out free memory for
                                               lagkTsData and qAdjTsData to fix
                                               memory fault. 
******************************************************************************/

#include "lagk.h"
#include <time.h>

int outputCount;

int main( int argc, char **argv )
{
   clock_t start, end;
   double cpu_time_used;
   
   int  driverResolution;
   int  doCOX, wkSize1, wkSize2;
   char *outflowTs = NULL;
   char *qAdjId = NULL;
   char *qAdjCd = NULL;

   /* Allocate reasonably per model*/
   float parray_curr[MAX_PARRAY], carray_curr[MAX_CARRAY], 
	 carray_unused_curr[MAX_CARRAY];
   float parray_prev[MAX_PARRAY], carray_prev[MAX_CARRAY], 
	 carray_unused_prev[MAX_CARRAY];

   float *lagkTsData = NULL;
   float *qAdjTsData = NULL;

   FILE *outputTsFilePtr = NULL;

   start = clock();
   
   /* Check option(s) sent to program */
   if ( readOptions(argc, argv) < 0 )
   {
      exit( 0 );
   }

   /*  Get infomation from arguments (arguments.txt) */
   setInformationFromArguments( argv[1] );

   /* Initialize common block used by pin7_*/
   initcommonblockspin_( getParamFileName() );

   /* Initialize P and C arrays */
   memset( parray_curr, 0, sizeof(parray_curr) );
   memset( carray_curr, 0, sizeof(carray_curr) );
   memset( carray_unused_curr, 0, sizeof(carray_unused_curr) );

   /* Read the parameters file (params.txt) 
    * NOTE: We don't want to fill/pass carray_unused_curr for ex7_ 
    */
   pin7_( parray_curr, carray_unused_curr );
 
   /* Determine if carryover transfer is to be performed */
   doCOX = 0;

   if ( strcmp(getPrevParamsInfo(), "PARAMS_UNCHANGED") != 0 )
   {
      doCOX = 1;  
      initcommonblockspin_( getPrevParamsInfo() );
      pin7_( parray_prev, carray_unused_prev );
      readStateLagK( carray_prev, carray_unused_prev );
      cox7_( parray_prev, carray_prev, parray_curr, carray_unused_curr );
      memcpy( carray_curr, carray_unused_curr, sizeof(carray_unused_curr) );
   }

   /* If do NOT perform carrayover transfer, we need to read the actual states
    * from statesI.txt file 
    */
   if ( doCOX == 0 )
   {
      readStateLagK( carray_curr, carray_unused_curr );
   }

   /* Read time series (ts.txt) file */
   readAllInputTimeSeries();

   /* Compute total length of working space needed */
   wksp7_( parray_curr, carray_curr, &wkSize1, &wkSize2 );

   if ( wkSize1 <= 0 || wkSize2 <= 0 )
   {
      logMessageWithArgsAndExitOnError( FATAL_LEVEL,
      "Could not allocate memory  for input (size %d) or "
      "output (size %d) time series", wkSize1, wkSize2 );
   }	  

   /* Allocate memory for input time serires */
   lagkTsData = (float *)calloc(wkSize1+1, sizeof(float));

   qAdjTsData = (float *)calloc(wkSize2+1, sizeof(float));
   
   /* Get Required input time series information */
   getRequiredInputTimeSeries( parray_curr, lagkTsData );

   /* Execute the LAGK operation (#7) */
   ex7_( parray_curr, carray_curr, lagkTsData, qAdjTsData );
	
   /* Open the output time seriers file here rather than in write, since 
    * we're going to write out individually and need to keep appending to the
    * same file 
    */	
   outputTsFilePtr = fopen( getOutputTsFileName(), "w+" );

   if ( outputTsFilePtr == NULL )
   {
      logMessageWithArgsAndExitOnError( FATAL_LEVEL,
      "ERROR: Could not open %s\n", getOutputTsFileName() );
   }

   /* Get the output time series information from params */
   outflowTs = getTimeSeriesIdFromPArray( parray_curr, 6 );
   
   if ( outflowTs == NULL )
   {
      logMessageAndExitOnError( FATAL_LEVEL,
      "Error reading parray_curr for outflowTs" );
   } 

   if ( strcmp(outflowTs, BLANK8SPACES) == 0 )
   {	
      qAdjId = getTimeSeriesIdFromPArray( parray_curr, 2);
      qAdjCd = getTimeSeriesCodeFromPArray( parray_curr, 4 );
      driverResolution = getIntegerFromPArray( parray_curr, 5 );	
   }	
   else 
   { 
      qAdjId = getTimeSeriesIdFromPArray( parray_curr, 6 );
      qAdjCd = getTimeSeriesCodeFromPArray( parray_curr, 8 );
      driverResolution = getIntegerFromPArray( parray_curr, 9 );	
   }

   if ( qAdjId == NULL )
   {
      logMessageAndExitOnError( FATAL_LEVEL,
      "Error reading parray_curr for qAdjId" );
   }
   
   if ( strcmp( qAdjId, "NONE" ) )
   {
      writeOneTimeSeries( outputTsFilePtr, qAdjId, qAdjCd, driverResolution,
                          qAdjTsData, outputCount );
   }

    
   /* Close output time series file - outputs.txt */ 
   fclose( outputTsFilePtr );
   
   /* Free memory */
   if ( lagkTsData != NULL )
   {
      free ( lagkTsData );
   }

   if ( qAdjTsData != NULL )
   {
      free ( qAdjTsData );
   }
   
   freeTimeSeries();

   /* Write output carryover to statesO.txt file */
   writeStateLagK( carray_curr );

   freeStates();

   /* Compute CPU time for model run */
   end = clock();
   cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;

   logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
   "===> TOTAL RUN TIME FORTRAN MODEL: %.2f (seconds)", cpu_time_used );
   
   logMessage( DEBUG_LEVEL, "Exiting lagk" );
    
   closeDiagFile();

   return SUCCESS;	

} /* main() ---------------------------------------------------------------- */

/******************************************************************************
     Module: getRequiredInputTimeSeries()

     Description:
     ============
     This function gets the driver time series.
    
     Input:
     ------
     float*         parray     - Array contains input time series information 
                               from params.txt file.

     Output:
     -------
     float*         lagkTsData - contains the time series values. 
   
   Change History
   ==============

   DATE          VERSION    PROGRAMMERS           NOTES
   ----------    -------    -----------------     ----------------------
   09/09/09                 Cham Pham             Removed free memory for
                                                  lagkId and lagkCd 
******************************************************************************/   
void getRequiredInputTimeSeries( float *parray,  float *lagkTsData )
{
   int  resolution;
   int  count;

   TimeSeries *lagkTs = NULL;

   /* This TS is the driver for this model */
   char *lagkId = getTimeSeriesIdFromPArray( parray, 2 );
   char *lagkCd = getTimeSeriesCodeFromPArray( parray, 4 );

   /* If this is stored in parray, then get it in the case of this model, 
    * it is assumed, since there can be only one resolution (data time 
    * interval)
    */
   resolution = getIntegerFromPArray( parray, 5 );

   /* It is assumed that the driver time series' last point will always
    * be beyond of any other TS data whose resolution is lower, is this
    * right?  Check with Lee
   */
   count = getNumberOfElementsInTimeSeries( resolution );
   outputCount = count;

   lagkTs = getOneTimeSeries( lagkId, lagkCd, resolution, &count );
   memcpy( lagkTsData, lagkTs->value, sizeof(float) * count );

} /* getRequiredInputTimeSeries() ------------------------------------------ */

/***************************************************************************** 
     Module: readStateLagK()

     Description:
     ============
     This function read state (carryover) values from statesI.txt file.
    
     Input:
     ------
     float[]    COarray_unused   - Array contains carryover values generated by
                                   params file.

     Output:
     -------
     float[]    COarray          - Array contains carryover value read from 
                                   statesI.txt file 
                                
     Change History
     ==============
     DATE          VERSION    PROGRAMMERS           NOTES
     ----------    -------    -----------------     ----------------------

*****************************************************************************/ 
void readStateLagK( float COarray[], float COarray_unused[] )
{
   int   nStates, stateUnits;
   int   i, numberPair;
   int   maxNumOfCarryoverValues, maxNumOfPairs;
   float numOfCarryoverValues;	

   const int  numberOfStatesWithoutPairs = 5;

   /* Open input states and read information from statesI.txt file */
   readStatesFromFile( &nStates, &stateUnits );
	
   numOfCarryoverValues = populateFloatStateValue( "LENGTH_CARRYOVER_ARRAY", 
                                                   COarray, 1 );
	    
   populateFloatStateValue( "CURRENT_LAGGED_INFLOW", COarray, 2 );
   populateFloatStateValue( "CURRENT_OUTFLOW", COarray,3 );
   populateFloatStateValue( "CURRENT_STORAGE", COarray,4 );
	
   if ( numOfCarryoverValues > numberOfStatesWithoutPairs ) 
   {	
      numberPair = (int)populateFloatStateValue( "PAIR_QT_LAG_CARRYOVER",
     	                                         COarray, 5 );
      /* This call will use the values pair array */
      populateArrayOfFloatStateValuesPair( "DISCHARGE", "LAG",
		                           numberPair, COarray, 6 );	
   } 
	
   /* Code below allows for updating state when all states not entered */	
   maxNumOfCarryoverValues = (int)COarray_unused[0];
   maxNumOfPairs = (int)COarray_unused[4];
	
   if ( numOfCarryoverValues < maxNumOfCarryoverValues ) 
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      " Number of lagk carryovers changed from %d to %d",
      (int)numOfCarryoverValues, maxNumOfCarryoverValues );
	
      COarray[0] = maxNumOfCarryoverValues;
      COarray[4] = maxNumOfPairs;
	   	   
      for ( i = numOfCarryoverValues; i < maxNumOfCarryoverValues; i++)
      {
          COarray[i] = 0.0;
      }
   }
} /* readStateLagK() ------------------------------------------------------ */

/***************************************************************************** 
     Module: writeStateLagK()

     Description:
     ============
     This function writes state (carryover) values to statesO.txt file.
    
     Input:
     ------
     float[]    COarray          - Array contains carryover values 

     Output:
     -------
     Write the output state to statesO.txt file

     Change History
     ==============
     
     DATE          VERSION    PROGRAMMERS           NOTES
     ----------    -------    -----------------     ----------------------

*****************************************************************************/ 
void writeStateLagK( float COarray[] )
{
   int numberPair;
    
   /* Open output State file - statesO.txt */
   FILE *outputStateFilePtr = fopen( getOutputStateFileName(), "w+" );

   if ( outputStateFilePtr == NULL )
   {
      logMessageWithArgsAndExitOnError( FATAL_LEVEL,
      "ERROR: Could not open %s\n", getOutputStateFileName() );
   }
    
   /* Write out the state results into file */
   writeStringStateToFile( outputStateFilePtr, "UNIT", "METRIC" );
    
   writeFloatStateToFile( outputStateFilePtr, "LENGTH_CARRYOVER_ARRAY",
                          COarray, 1 );
   writeFloatStateToFile( outputStateFilePtr, "CURRENT_LAGGED_INFLOW",
	                  COarray, 2 );
   writeFloatStateToFile( outputStateFilePtr, "CURRENT_OUTFLOW", 
                          COarray, 3 );
   writeFloatStateToFile( outputStateFilePtr, "CURRENT_STORAGE", 
	                  COarray, 4 );
		 
   numberPair = (int)COarray[4];
	
   if ( numberPair != 0 )
   {		
      writeFloatStateToFile( outputStateFilePtr, "PAIR_QT_LAG_CARRYOVER",
                             COarray, 5 );				
      writeArrayOfFloatStatesPairsToFile( outputStateFilePtr, "DISCHARGE", 
                                          "LAG", COarray, 6, numberPair );	
   }

   /* Close output state file */
   fclose( outputStateFilePtr );

} /* writeStateLagK() ------------------------------------------------------ */
