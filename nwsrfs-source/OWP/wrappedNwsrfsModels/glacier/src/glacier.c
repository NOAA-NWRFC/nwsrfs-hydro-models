/******************************************************************************
   Filename: glacier.c

   Description:
   ============
      1. Use the arguments file passed in from fews
      2. Reads time series from ts.txt file
      3. Reads parameters from params.txt 
      4. Checks if the params are changed. If so read the params_saved.txt file
         and call the cox56 routine to get the carry over. Else read the 
         carry over states from statesI.txt file
      5. Executes the glacier operation - #56
      6. Saves output carryover states. 
      7. Writes output time series. 

   Inputs: 

   Outputs:

   Change History 
   ==============
   DATE          VERSION    PROGRAMMERS        NOTES
   ----------    -------    -----------------  ----------------------
   4/30/09        1          A. Vo             Initial Implementation
   9/09/09        2          Cham/Varalakshmi  Refactored code to work on
                                               LX6 (Redhat 5).
   07/22/10       3          Cham/Varalakshmi  Fixed memory leak
******************************************************************************/

#include <time.h>
#include "glacier.h"

int main(int argc, char **argv)
{
   clock_t start, end;
   double cpu_time_used;

   float poCurrent[POMAX], coCurrent[COMAX]; 
   float poPrevious[POMAX], coPrevious[COMAX]; 
   int   i, j;
   int   numberOfTimeSteps;
   int   outputTSNumberOfTimeSteps = 0;
   int   doCox = 0;
   char  paramsFileName[FILE_NAME_LENGTH];
   char  prevParamsInfo[FILE_NAME_LENGTH];
   int   dataTimeInterval, outputTsTimeInterval;
   char  *outputTsFileName = NULL;
   char  *dataId = NULL;  
   char  *dataType = NULL;
   char  *glacierTsId = NULL;
   char  *glacierTsType = NULL; 
   float *rainmeltInputTs = NULL;           // input ts
   float *glacierOutputTimeseries = NULL;   // output ts[outputTSNumberOfTimeSteps];
   float *AFIOutputTimeseries = NULL;       // output ts[outputTSNumberOfTimeSteps];
   int   numberOutTS;
   FILE  *outputTsFilePtr = NULL;
   TimeSeries *inputTS = NULL;
   
   start = clock();

   if( readOptions(argc, argv) < 0 )
   {
     exit(0);
   }
   
   /* Pass input argument and Create diagnostic file */
   setInformationFromArguments(argv[1]);

   logMessage( DEBUG_LEVEL, "Entering glacier" );

   /* Initialize po arrays */
   memset( poCurrent, 0, sizeof(poCurrent) );
   memset( coCurrent, 0, sizeof(coCurrent) );
   memset( poPrevious, 0, sizeof(poPrevious) );
   memset( coPrevious, 0, sizeof(coPrevious) );

   memset( paramsFileName, '\0', sizeof(paramsFileName));
   strcpy( paramsFileName, getParamFileName() );
   
   // load the time series first
   readAllInputTimeSeries();

   // read the params.txt
   pin56_( poCurrent, coCurrent, paramsFileName );

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
      "After pin read from[%s]\n", paramsFileName);
   }

   memset( prevParamsInfo, '\0', sizeof(prevParamsInfo) );
   strcpy( prevParamsInfo, getPrevParamsInfo());

   if (strcmp(prevParamsInfo, "PARAMS_UNCHANGED"))
   {
      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
         "Params changed ...");
      }

      doCox = 1;

      /* read params_saved.txt */
      pin56_( poPrevious, coPrevious, prevParamsInfo );

      /* memset cCurrent since the actual states are read from 
         statesI.txt unlike the pin routine filling the values 
         from the params file */
      memset(coPrevious, 0, sizeof(coPrevious));

      /* read states which actually matches the old params */
      readStates(poPrevious, coPrevious);

      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
                       "After reading states from statesI.txt");
      }

      cox56_(poPrevious, coPrevious, poCurrent, coCurrent);

      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
                       "Carry over transfer is done");		       
      }

   }
   if (doCox == 0)
   {
      /* 
      *
      * memset coCurrent since the actual states are read from statesI.txt unlike 
      * the pin routine filling the values from the params file 
      *
      */
      memset( coCurrent, 0, sizeof( coCurrent ) );

      // read states which actually matches the params.txt
      readStates( poCurrent, coCurrent );
   
   }
      
   /* process input TS  */

   dataTimeInterval = getIntegerFromPArray( poCurrent, 8 );

   dataId = getTimeSeriesIdFromPArray( poCurrent, 2 );
   dataType = getTimeSeriesCodeFromPArray( poCurrent, 4 );

   if ( getFewsDebugFlag() > 3 ) 
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
      "currentID = %s currentType = %s currentTimeInterval = %d\n",
      dataId, dataType, dataTimeInterval);	        
   }	
      
   numberOfTimeSteps = getNumberOfElementsInTimeSeries( dataTimeInterval );

   inputTS = getOneTimeSeries( dataId, dataType, 
                               dataTimeInterval, &numberOfTimeSteps );
   
   // declaration for variable that hold input time series from input ts file

   rainmeltInputTs = (float*) calloc( numberOfTimeSteps + 1, sizeof(float) ); 
   
   if ( inputTS != NULL )
   {
     memcpy(rainmeltInputTs, inputTS->value, sizeof(float)*numberOfTimeSteps);
   }

   /* write output time series */
   outputTsFileName = getOutputTsFileName();

   outputTsFilePtr = fopen( outputTsFileName, "w" );
   
   /* there is only one output timeseries for muskrouting*/
   /* There are two output timeseries: Glacier output and AFAI output */ 
   
   /* input and output timeseries have the same time interval ?? */
   outputTsTimeInterval = getIntegerFromPArray( poCurrent, 8 );

   glacierTsId = getTimeSeriesIdFromPArray( poCurrent, 5 );

   glacierTsType = getTimeSeriesCodeFromPArray( poCurrent, 7 ); 
 
   if( outputTsTimeInterval != 0 || outputTsTimeInterval > 24 )
   {
      outputTSNumberOfTimeSteps = getNumberOfElementsInTimeSeries( outputTsTimeInterval );
   }

   if ( getFewsDebugFlag() > 4 ) 
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "glacierTsId = [%s] glacierTsType = [%s] OutputTSInterval = [%d]\n",       
      glacierTsId, glacierTsType, outputTsTimeInterval );

      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "outputTSNumberOfTimeSteps = [%d]\n", outputTSNumberOfTimeSteps );            
   }

   glacierOutputTimeseries = (float *) calloc( outputTSNumberOfTimeSteps+1, 
	                                       sizeof(float) );

   AFIOutputTimeseries = (float *) calloc( outputTSNumberOfTimeSteps+1,
	                                   sizeof(float) );

   //call ex routine thereby execute the model
   ex56_( poCurrent, coCurrent, rainmeltInputTs, glacierOutputTimeseries,
          AFIOutputTimeseries );

   numberOutTS = getIntegerFromPArray( poCurrent, 1 );
   
   if ( getFewsDebugFlag() > 4 ) 
   {
       // print out maximum of 100 data points
       for (j = 0; j < 100; j++ )
       {
          logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
          "GOUT Time Series[%d] = [%f] ",
	  j, glacierOutputTimeseries[j]);

          if ( numberOutTS == 2 ) /* there are two output timeseries */
          {
             logMessageWithArgsAndExitOnError( DEBUG_LEVEL,             
	     "AFIOUT Time Series[%d] = [%f] ", 
	     j, AFIOutputTimeseries[j]);
	  }
       }
   }
  
   /* Write time series values to file */
   /*        ......             Write Glacier output ts  */
   writeOneTimeSeries( outputTsFilePtr, glacierTsId,
                       glacierTsType, outputTsTimeInterval,
                       glacierOutputTimeseries, outputTSNumberOfTimeSteps );
   

   if ( numberOutTS == 2 ) /* there are two output timeseries */
   {
      char *AFITsId = getTimeSeriesIdFromPArray( poCurrent, 14 );
      char *AFITsType = getTimeSeriesCodeFromPArray( poCurrent, 16 );

       /*       .......             Write AFI output ts  */
       writeOneTimeSeries( outputTsFilePtr, AFITsId,
                       AFITsType, outputTsTimeInterval,
                       AFIOutputTimeseries, outputTSNumberOfTimeSteps );
   }

   /* Free memories */
   if( rainmeltInputTs != NULL )
   {
      free( rainmeltInputTs );
   }

   if ( glacierOutputTimeseries != NULL )
   {
      free( glacierOutputTimeseries );
   }

   if ( AFIOutputTimeseries != NULL )
   {
      free( AFIOutputTimeseries );
   }

   if ( outputTsFilePtr != NULL )
   {
      fclose( outputTsFilePtr );
   }

   freeTimeSeries();

   writeStates( poCurrent, coCurrent );

   freeStates();

   /* Compute CPU time for model run */
   end = clock();
   cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;

   logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
   "===> TOTAL RUN TIME FORTRAN MODEL: %.2f (seconds)", cpu_time_used );

   logMessage( DEBUG_LEVEL, "Exiting glacier" );

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
   int   numOfStatesKeys, numOfStateUnits;
   float returnStateValue;

   readStatesFromFile( &numOfStatesKeys, &numOfStateUnits );
      
   returnStateValue = populateFloatStateValue( "GLACIER_STORAGE", 
                                                &cArray[0], 1 ); 

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
      "GLACIER_STORAGE = [%f]", returnStateValue );
   }

   returnStateValue = populateFloatStateValue( "INITIAL_AFI_DECAY_PARAMETER", 
                                               &cArray[1], 1 ); 

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
      "INITIAL_AFI_DECAY_PARAMETER = [%f]", returnStateValue ); 
   }
}


/****************************************************************************
Module: writeStates()

Description : read the cArrayy write statesO.txt

input : float *pArray
        float *cArray
****************************************************************************/
void writeStates( float* pArray, float *cArray )
{
   
   // output states should be created as its requuired by fews. 
   // The file can be empty also, but needs to be created and 
   // made available for fews.
   FILE *outputStateFilePtr = fopen(getOutputStateFileName(), "w+");
   
   writeStringStateToFile(outputStateFilePtr, "UNIT", "METRIC");
   
   writeFloatStateToFile(outputStateFilePtr, "GLACIER_STORAGE", cArray, 1);

   writeFloatStateToFile(outputStateFilePtr, "INITIAL_AFI_DECAY_PARAMETER", 
	                                     cArray, 2);
   
   if ( outputStateFilePtr != NULL )
   {
      fclose( outputStateFilePtr );
   }
}
