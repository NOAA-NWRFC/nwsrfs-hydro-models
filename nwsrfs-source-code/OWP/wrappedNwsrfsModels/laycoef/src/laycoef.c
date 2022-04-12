/******************************************************************************
   Filename: laycoef.c

   Description:
   ============
      1. Use the arguments file passed in from fews
      2. Reads time series from ts.txt file
      3. Reads parameters from params.txt 
      4. Checks if the params are changed. If so read the params_saved.txt file
         and call the cox11 routine to get the carry over. Else read the 
         carry over states from statesI.txt file
      5. Executes the LAYCOEF operation - #11
      6. Saves output carryover states.
      7. Writes output time series.

   Inputs:

   Outputs:

   Change History
   ==============
   DATE          VERSION    PROGRAMMERS          NOTES
   ----------    -------    -----------------    ----------------------
   JUne 2009     1          Varalakshmi Rajaram  Initial Implementation
   07/22/10      2          Cham/Varalakshmi     Fixed memory leak
******************************************************************************/

#include "laycoef.h"

int main(int argc, char **argv)
{

   clock_t start, end;
   double cpu_time_used;

   int doCox = 0;
   int driverResolution, outputResolution;
   int numOfLayers;
   char paramsFileName[FILE_NAME_LENGTH];
   char prevParamsInfo[FILE_NAME_LENGTH];
   char *outputTsFileName = NULL;
   char *channelOutflowTimeSeriesId = NULL;
   char *channelOutflowTimeSeriesDataTypeCode = NULL;

   float pCurrent[1000], cCurrent[1000];
   float pPrevious[1000], cPrevious[1000];

   /* All channel inflow data mentioned in ts.txt*/
   float **channelInflowTsData = (float**) malloc(sizeof(float*)); 
   *channelInflowTsData = NULL;

   /* All channel outflow data from model output*/ 
   float *channelOutflowTsData = NULL;
   float *workSpace = NULL;

   FILE *outputTsFilePtr = NULL;

   start = clock();

   if( readOptions(argc, argv) < 0 )
   {
     exit(0);
   }

   setInformationFromArguments(argv[1]);

   /*load the time series first */
   readAllInputTimeSeries();

   memset( paramsFileName, '\0', sizeof(paramsFileName) );
   
   strcpy( paramsFileName, getParamFileName() );

   /* read params.txt */
   pin11_( pCurrent, cCurrent, paramsFileName );

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
      "After pin read from[%s]\n", paramsFileName );
   }

   memset( prevParamsInfo, '\0', sizeof(prevParamsInfo) );
   
   strcpy( prevParamsInfo, getPrevParamsInfo() );

   if ( strcmp(prevParamsInfo, "PARAMS_UNCHANGED") )
   {
      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
	 "Params changed ...");
      }

      doCox = 1;

      /* read params_saved.txt */
      pin11_(pPrevious, cPrevious, prevParamsInfo);

      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
	 "After pin read from [%s]\n", prevParamsInfo);
      }

      /* memset cCurrent since the actual states are read from statesI.txt 
         unlike the pin routine filling the values from the params file */
      memset( cPrevious, 0, sizeof(cPrevious) );

      /* read states which actually matches the old params */
      readStates( pPrevious, cPrevious );

      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
         "After carray read from statesI.txt");
      }

      cox11_( pPrevious, cPrevious, pCurrent, cCurrent );

      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
         "Carry over transfer is done");
      }
   }

   if ( doCox == 0 )
   {
      /* memset cCurrent since the actual states are read from statesI.txt 
         unlike the pin routine filling the values from the params file */
      memset( cCurrent, 0, sizeof(cCurrent) );

      /* read states which actually matches the params.txt */
      readStates( pCurrent, cCurrent );
   }
   
   /* read input time series from input ts file */

   driverResolution = getIntegerFromPArray( pCurrent, 10 );
   
   getRequiredInputTimeSeries( pCurrent, channelInflowTsData, driverResolution );

   channelOutflowTsData = (float*) calloc( outputCount, sizeof(float) );  
  
   numOfLayers = getIntegerFromPArray( pCurrent, 16 );

   workSpace = (float*) calloc( numOfLayers, sizeof(float) );

   /* call ex routine thereby execute the model */
   ex11_( pCurrent, cCurrent, *channelInflowTsData, channelOutflowTsData, 
          workSpace );

   if ( workSpace != NULL )
      free ( workSpace );

   /* write output time series */
   outputTsFileName = getOutputTsFileName();

   outputTsFilePtr = fopen(outputTsFileName, "w");

   if(outputTsFilePtr == NULL)
   {
      logMessageWithArgsAndExitOnError( FATAL_LEVEL,
            "ERROR: Could not open [%s]\n", outputTsFileName );
   }

   channelOutflowTimeSeriesId = getTimeSeriesIdFromPArray( pCurrent, 7 );
   channelOutflowTimeSeriesDataTypeCode = 
                      getTimeSeriesCodeFromPArray( pCurrent, 9 );
   outputResolution = getIntegerFromPArray( pCurrent, 10 );

   if ( channelOutflowTsData == (float*)NULL )
   {
      logMessage(DEBUG_LEVEL,"channelOutflowTsData is NULL");
   }
   else
   {
     char channelOutflowTimeSeriesId[9];
     char channelOutflowTimeSeriesDataTypeCode[5] ;

     memset( channelOutflowTimeSeriesId, '\0', 9 );

     memcpy( channelOutflowTimeSeriesId,
             getTimeSeriesIdFromPArray(pCurrent, 11), 9 );

     memset( channelOutflowTimeSeriesDataTypeCode, '\0', 5 );

     memcpy( channelOutflowTimeSeriesDataTypeCode, 
             getTimeSeriesCodeFromPArray(pCurrent, 13), 5 );
    
     /* if the output ts infm is not in params.txt then the input 
        ts infm is used.*/

     if ( channelOutflowTimeSeriesId[0] == ' ' )
     {
        memcpy( channelOutflowTimeSeriesId,
                getTimeSeriesIdFromPArray(pCurrent, 7), 9 );
        memcpy( channelOutflowTimeSeriesDataTypeCode, 
                getTimeSeriesCodeFromPArray(pCurrent, 9), 5 );
     }

     writeOneTimeSeries( outputTsFilePtr, channelOutflowTimeSeriesId,
	                 channelOutflowTimeSeriesDataTypeCode, 
			 outputResolution, channelOutflowTsData, 
			 outputCount );
   }

   /* Free memories for timeseries */
   if ( channelInflowTsData != NULL )
      free ( channelInflowTsData );

   if ( channelOutflowTsData != NULL )
      free ( channelOutflowTsData );

   freeTimeSeries();

   if ( outputTsFilePtr != NULL )
      fclose( outputTsFilePtr );

   /* Write state values to file */
   writeStates( pCurrent, cCurrent );

   freeStates();

   /* Compute CPU time for model run */
   end = clock();
   cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;

   logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
    "===> TOTAL RUN TIME FORTRAN MODEL: %.2f (seconds)", cpu_time_used );
    
   logMessage( DEBUG_LEVEL,"Exiting laycoef" );

   closeDiagFile();

   return(SUCCESS);
}

/******************************************************************************
Module :
        getRequiredInputTimeSeries()
Input  :
	float * pArray - pArray populated by the pin routine.
        float **channelInflowTsData - channel inflow timeseries
        int driverResolution - driver resolution

Description :
        Read the pArray info and populates the channel 
        inflow timeseries
******************************************************************************/

void getRequiredInputTimeSeries( float *pArray, 
				 float **channelInflowTsData, 
                                 int driverResolution )
{
   char *channelInflowTimeSeriesId = NULL;
   char *channelInflowTimeSeriesDataTypeCode = NULL;

   int count;

   TimeSeries *channelInflowTs = NULL;

   channelInflowTimeSeriesId = getTimeSeriesIdFromPArray( pArray, 7 );

   channelInflowTimeSeriesDataTypeCode = getTimeSeriesCodeFromPArray( pArray, 9);
  
   if ( channelInflowTimeSeriesId == NULL )
   {
      logMessageWithArgsAndExitOnError(FATAL_LEVEL, 
      "Error: Channel inflow TS Id is null");
   }

   if ( channelInflowTimeSeriesDataTypeCode == NULL )
   {
      logMessageWithArgsAndExitOnError(FATAL_LEVEL,
      "Error: Channel inflow DataTypeCode is null");
   }

   count = getNumberOfElementsInTimeSeries(driverResolution);

   outputCount = count;

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL,  
      "Output count = %d",count );
   }
   
   channelInflowTs = getOneTimeSeries( channelInflowTimeSeriesId,
                                       channelInflowTimeSeriesDataTypeCode, 
				       driverResolution, &count );
   if ( channelInflowTs != NULL )
   {
      *channelInflowTsData = channelInflowTs->value;
   }

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL,"%s %s", 
      channelInflowTimeSeriesId, channelInflowTimeSeriesDataTypeCode);
   }	    
}


/******************************************************************************
Module: readStates()

Description : reads statesI.txt and filles the cArray

input : float *pArray

output: float *cArray

Change History: 
Date    Version Programmers         Notes
-----   ------- -----------         -----
June 2009 1     Varalakshmi Rajaram Initial implementation
******************************************************************************/

void readStates( float* pArray, float *cArray )
{
   /* read states file & populate  carray */
   int   numOfStatesKeys, numOfStateUnits;
   int   numOfLayers, index;
   int   indexOfCArray = 1;
   float returnStateValue;
   char  key[100] = {'\0'};

   readStatesFromFile( &numOfStatesKeys, &numOfStateUnits );


   numOfLayers = getIntegerFromPArray(pArray, 16);

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
      "Before reading the states input...Num of layers[%d]", 
      numOfLayers);
   }

   memset( key, '\0', sizeof(key) );

   strcpy( key, "FLOW_LAYER" );

   populateArrayOfFloatStateValues( key, numOfLayers, cArray, 
                                    indexOfCArray );
}

/******************************************************************************
Module: writeStates()

Description : read the cArrayy write statesO.txt 

input : float *pArray
        float *cArray

Change History: 
Date    Version Programmers         Notes
-----   ------- -----------         -----
June 2009 1     Varalakshmi Rajaram Initial implementation
******************************************************************************/
void writeStates( float* pArray, float *cArray )
{
   int numOfLayers;
   int indexOfCArray = 1;

   /* write output states */
   FILE *outputStateFilePtr = fopen(getOutputStateFileName(), "w+");

   if ( outputStateFilePtr == NULL )
   {
      logMessageWithArgsAndExitOnError( FATAL_LEVEL,
      "ERROR: Could not open [%s]\n", getOutputStateFileName() );
   }

  
   numOfLayers = getIntegerFromPArray( pArray, 16 );

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
      "Before writing the states output...Num of routing layers[%d]", 
      numOfLayers );
   
      logMessage(DEBUG_LEVEL,"Writing state values laycoef");
   }

   writeStringStateToFile( outputStateFilePtr, "UNIT", "METRIC" );

   writeArrayOfFloatStatesToFile( outputStateFilePtr, 
                                  "FLOW_LAYER",
                                  cArray, indexOfCArray, numOfLayers );
   
   if ( outputStateFilePtr != NULL )
   {
      fclose( outputStateFilePtr );
   }
}

