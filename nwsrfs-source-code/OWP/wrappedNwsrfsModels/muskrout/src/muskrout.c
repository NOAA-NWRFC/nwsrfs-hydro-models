/******************************************************************************
   Filename: muskrout.c

   Description:
   ============
      1. Use the arguments file passed in from fews
      2. Reads time series from ts.txt file
      3. Reads parameters from params.txt 
      4. Checks if the params are changed. If so read the params_saved.txt file
         and call the cox9 routine to get the carry over. Else read the 
         carry over states from statesI.txt file
      5. Executes the muskrout operation - #9
      6. Saves output carryover states. 
      7. Writes output time series. 

   Inputs: 

   Outputs:

   Change History 
   ==============
   DATE          VERSION    PROGRAMMERS        NOTES
   ----------    -------    -----------------  ----------------------
                 1          A. Vo             Initial Implementation
   07/22/10      2          Cham/Varalakshmi  Fixed memory leak
******************************************************************************/

#include <time.h>
#include "muskrout.h"

int main( int argc, char **argv )
{

   clock_t start, end;
   double cpu_time_used;
    
   char paramsFileName[FILE_NAME_LENGTH];
   char *dataId = NULL; 
   char *dataType = NULL;
   char *outflowTsId = NULL;
   char *outflowTsType = NULL;
   char *outputTsFileName = NULL;

   float poCurrent[POMAX], coCurrent[COMAX]; 
   float *routingInflowTsData = NULL;
   float *routingOutflowTsData = NULL;

   int i, j;
   int dataTimeInterval, outflowTsTimeInterval;
   int numberOfTimeSteps;
   int outflowNumberOfTimeSteps = 0;

   FILE *outputTsFilePtr = NULL;
   
   TimeSeries *inputTS = NULL;

   start = clock();
   
   if ( readOptions(argc, argv) < 0 )
   {
     exit(0);
   }
   
   /* Initialize po and co array */
   memset( poCurrent, 0, sizeof(poCurrent) );
   memset( coCurrent, 0, sizeof(coCurrent) );

   /* Pass input argument and Create diagnostic file */
   setInformationFromArguments( argv[1] );

   logMessage( DEBUG_LEVEL, "Entering MUSKROUT" );

   /* Load the time series first */
   readAllInputTimeSeries();

   /* Read the params.txt */
   memset( paramsFileName, '\0', sizeof(paramsFileName) );
   strcpy( paramsFileName, getParamFileName() );

  
   pin9_( poCurrent, coCurrent, paramsFileName );

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
      "After pin read from[%s]\n", paramsFileName);
   }
   /* 
    * memset coCurrent since the actual states are read from statesI.txt unlike 
    * the pin routine filling the values from the params file 
    */
   memset( coCurrent, 0, sizeof( coCurrent ) );
    
   /* Read states which actually matches the params.txt */
   readStates( poCurrent, coCurrent );
   
   /* Process input TS  */
   dataTimeInterval = getIntegerFromPArray( poCurrent, 10 );

   dataId = getTimeSeriesIdFromPArray( poCurrent, 7 ); 

   dataType = getTimeSeriesCodeFromPArray( poCurrent, 9 );

   if ( getFewsDebugFlag() > 3 ) 
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
      "currentID = %s currentType = %s currentTimeInterval = %d\n",
      dataId, dataType, dataTimeInterval);	        
   }	
      
   numberOfTimeSteps = getNumberOfElementsInTimeSeries( dataTimeInterval );

   inputTS = getOneTimeSeries( dataId, dataType, 
                               dataTimeInterval, &numberOfTimeSteps );
 
   routingInflowTsData = (float*) calloc( numberOfTimeSteps + 1,
	                                  sizeof(float) ); 
  
   if ( inputTS != NULL )
   { 
      memcpy( routingInflowTsData, inputTS->value, 
	      sizeof(float) * numberOfTimeSteps);
   }

   /* Write output time series */
   outputTsFileName = getOutputTsFileName();

   outputTsFilePtr = fopen( outputTsFileName, "w" );
   
   /* There is only one output timeseries */
   outflowTsTimeInterval = getIntegerFromPArray( poCurrent, 14 );

   outflowTsId = getTimeSeriesIdFromPArray( poCurrent, 11 );

   outflowTsType = getTimeSeriesCodeFromPArray( poCurrent, 13 );
  
   if ( outflowTsTimeInterval != 0 || outflowTsTimeInterval > 24 )
   {
      outflowNumberOfTimeSteps = getNumberOfElementsInTimeSeries( outflowTsTimeInterval );
   }
  
   if ( getFewsDebugFlag() >= 4 ) 
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "OutflowTSID = [%s] OutflowTSType = [%s] OutputTSInterval = [%d]\n",       
      outflowTsId, outflowTsType, outflowTsTimeInterval );

      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "OutflowTSNumberOfTimeSteps = [%d]\n", outflowNumberOfTimeSteps );
   }	

   routingOutflowTsData = (float*) calloc( outflowNumberOfTimeSteps + 1, 
	                                   sizeof(float) );

   /* Call ex routine thereby execute the model */
   ex9_( poCurrent, coCurrent, routingInflowTsData, routingOutflowTsData );

   if ( getFewsDebugFlag() >= 4 ) 
   {
       for(j = 0; j < outflowNumberOfTimeSteps; j++ )
       {
          if(outflowNumberOfTimeSteps <=100)
	  {
              logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
                "output Time Series[%d] = [%f] \n", j, routingOutflowTsData[j]);
	  }
       }
   }
   
   /* Write time series values to file */
   writeOneTimeSeries( outputTsFilePtr, outflowTsId,
                       outflowTsType, outflowTsTimeInterval,
                       routingOutflowTsData, outflowNumberOfTimeSteps );

   /* Free memories for timeseries */
   if( routingInflowTsData != NULL )
   {
      free ( routingInflowTsData );
   }

   if ( routingOutflowTsData != NULL )
   {
      free ( routingOutflowTsData );
   }

   freeTimeSeries();

   /* Write state values to file */
   writeStates( poCurrent, coCurrent );
  
   freeStates();

   /* Compute CPU time for model run */
   end = clock();
   cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;

   logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
   "===> TOTAL RUN TIME FORTRAN MODEL: %.2f (seconds)", cpu_time_used );

   logMessage( DEBUG_LEVEL, "Exiting muskrout" );

   closeDiagFile();

   exit( SUCCESS );

}



/*****************************************************************************
Module: readStates()

Description : reads statesI.txt and filles the cArray

input : float *pArray

output: float *cArray
*****************************************************************************/
void readStates( float* pArray, float *cArray )
{
 
   // read states file & populate  carray
   int numOfStatesKeys, numOfStateUnits;
   float returnStateValue;

   readStatesFromFile( &numOfStatesKeys, &numOfStateUnits );
       
   returnStateValue = populateFloatStateValue( "INITIAL_INFLOW", &cArray[0], 1 ); 

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
      "INITIAL_INFLOW = [%f]", returnStateValue );
   }

   returnStateValue = populateFloatStateValue( "INITIAL_OUTFLOW", &cArray[1], 1 ); 

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
      "INITIAL_OUTFLOW = [%f]", returnStateValue ); 
   }
}


/*****************************************************************************
Module: writeStates()

Description : read the cArrayy write statesO.txt

input : float *pArray
        float *cArray
*****************************************************************************/
void writeStates( float* pArray, float *cArray )
{
   // state Control indicator = 0 inflow and outflow initially set to 0
   // state Control indicator = 1 inflow and outflow were read in for states 
   //                             input
   
   int   stateControlIndicator;  
   float initialValue = 0.0;
   FILE  *outputStateFilePtr = NULL;

   stateControlIndicator = getIntegerFromPArray( pArray, 20 );  

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
      "stateControlIndicator [%d]", stateControlIndicator );
   }

   // output states should be created as its requuired by fews. 
   // The file can be empty also, but needs to be created and made available 
   // for fews.
   outputStateFilePtr = fopen(getOutputStateFileName(), "w+");
   
   writeStringStateToFile( outputStateFilePtr, "UNIT", "METRIC" );
   
   if ( stateControlIndicator == 1 )
   {
      writeFloatStateToFile( outputStateFilePtr, "INITIAL_INFLOW", cArray, 1 );

      writeFloatStateToFile( outputStateFilePtr, "INITIAL_OUTFLOW", cArray, 2 );
   }
   else
   {
      writeFloatStateToFile( outputStateFilePtr, "INITIAL_INFLOW", 
	                     &initialValue, 1 );

      writeFloatStateToFile( outputStateFilePtr, "INITIAL_OUTFLOW", 
	                     &initialValue, 2 );
   }
}
