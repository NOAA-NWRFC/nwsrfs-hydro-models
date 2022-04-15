/******************************************************************************
   Filename: resj_main.c

   Description:
   ============
      The main function for the RESJ operation (#58).
      1. Reads the arguments passed in from the java
      2. Reads the time series (ts.txt) file
      3. Sets up any common blocks to pass in FORTRAN input file.
      4. Reads the parameter file using the PIN58  routine
      5. Retrives the time series values and store into D array 
      6. Read states (carryover) from statesI.txt file 
      7. Perform carryovertransfer if needed
      8. Initialize the FCTIME common block to use by EX58 routine
      9. Execute the RESJ (Joint Reservior Regulation) operation - #58 
      10. Save carryover information into StatesO.txt file 
      11. Writes the result (including value and time series) to output.txt file 
      
   Inputs: 
   
   Outputs:

   Change History
   ==============
   DATE          VERSION    PROGRAMMERS        NOTES
   ----------    -------    -----------------  ----------------------
   08/09/08      1          Ai Vo / Cham Pham  Initial implementation
   07/26/10      2          Cham / Varalakshmi Fixed memory leak
******************************************************************************/

#include "resj_main.h"
#include <time.h>

int FewsDebugFlag;        /* global debug flag is used by this file */

int main ( int argc, char **argv )
{
    clock_t start, end;
    double cpu_time_used;
    
    int i, j;
    int numTimeSeries, modelTimeStep, numCOArrayElem;

    int doCarryoverTransfer = FALSE;

    float POcurrent[MAXPARRAY], COcurrent[MAXCARRAY], 
          POprevious[MAXPARRAY], COprevious[MAXCARRAY]; 

    float *Darray = NULL;

    TimeSeries *tsInputPtr = NULL;

    char currId[NWSRFS_ID_LEN] = {'\0'}, 
	 currType[NWSRFS_TYPE_LEN] = {'\0'};
	 

    char *outputStatesFileName = (char *)calloc( FILE_NAME_LENGTH, 
	                                         sizeof(char) );
    int  nextDarrayIndx, currIndex;
    int  totalBufTSize = 0;

    FILE *outputTsFilePtr;

    start = clock();
    
    /* 
     * Check for appropriate input arguments 
     */
    if ( readOptions(argc, argv) < 0 ) {
       exit( 0 );
    } 

    /* Pass input argument and Create diagnostic file */
    setInformationFromArguments( argv[1] ); 
 
    /* Get Fews debug flag */
    FewsDebugFlag = getFewsDebugFlag(); 

    /* Initialize PO and CO arrays */
    memset ( POcurrent, 0, sizeof(POcurrent) );
    memset ( COcurrent, 0, sizeof(COcurrent) );
    memset ( POprevious, 0, sizeof(POprevious) );
    memset ( COprevious, 0, sizeof(COprevious) );

    /* Load timeseries from file (ts.txt) */
    readAllInputTimeSeries();

    logMessage(DEBUG_LEVEL, "Allocating initial memory for dataID");
    logMessage(DEBUG_LEVEL, "Allocating initial memory for dataType");

    char** dataID = (char**) calloc(MAX_TS_IO, sizeof(char*));
    char** dataType = (char**) calloc(MAX_TS_IO, sizeof(char*));
    int* timeStep = (int*) calloc(MAX_TS_IO, sizeof(int)); 
    int* desiredTimeStep = (int*)  calloc(MAX_TS_IO, sizeof(int));
    int* tsInOut = (int*)  calloc(MAX_TS_IO, sizeof(int));
    int* Dindex = (int*)  calloc(MAX_TS_IO, sizeof(int));

    int numOfRows = MAX_TS_IO;

    dataID = allocateTwoDemensionalArrayOfCharType(dataID, numOfRows, NWSRFS_ID_LEN, 1);

    dataType = allocateTwoDemensionalArrayOfCharType(dataType, numOfRows, NWSRFS_TYPE_LEN, 1);

    if ( FewsDebugFlag >= 4 ) 
    {
          logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
          "Resj: Before resj_readStates() COcurrent  = %s \n", 
	  (char*)COcurrent );
    }

    /* Read parameter file - handles all pin58 capabilities  */    
    input_parameters58( getParamFileName(), POcurrent, COcurrent );
    
    if ( FewsDebugFlag >= 3 ) 
    {
       logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
       "Resj: COcurrent After input_param58 = %s \n", (char*)COcurrent );
    }
    
    /* handles carryovertransfer.                     */
    /* if parameters had been changed since last run  */
    if ( strcmp(getPrevParamsInfo(), "PARAMS_UNCHANGED") != 0 )  
    {
       /* Read previous parameters file  */    
       input_parameters58( getPrevParamsInfo(), POprevious, COprevious );
   
       if ( FewsDebugFlag >= 4 ) 
       {
          logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
             "Resj: Before resj_readStates() COprevious  = %s \n", COprevious );
       }

       /* Read state value (carryover) from statesI.txt file */
       resj_readStates( POprevious, COprevious );

       /* performs carryover transfer - Resj passes previuos and current 
	* parameters files */
       char prevFile[FILE_NAME_LENGTH] ;
       memset(prevFile, '\0', FILE_NAME_LENGTH);
       strcpy(prevFile, getPrevParamsInfo());
       char curFile[FILE_NAME_LENGTH];
       memset(curFile, '\0', FILE_NAME_LENGTH);
       strcpy(curFile, getParamFileName());
       carryovertransfer58( COprevious, POprevious, COcurrent, POcurrent, 
	                    prevFile, curFile);
       doCarryoverTransfer = TRUE;    
    }    
    
    if ( !doCarryoverTransfer )
    {
         /* memset COcurrent since the actual states are read from statesI.txt 
	  * unlike the pin routine filling the
          * values from the params file - initializing COcurrent
	  */   
         resj_readStates( POcurrent, COcurrent );     
    }

    /* Get information from PO array */
    numTimeSeries = (int) POcurrent[1];  /* Get the number of time series   */
    modelTimeStep = (int) POcurrent[2];  /* Compute time interval in hours  */
    numCOArrayElem = (int) POcurrent[3]; /* Get number of CO array elements */

    if ( FewsDebugFlag >= 3 ) 
    {
       logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
       "No. of time series: %d\t Time Step: %d\tCO array element: %d\n",
       numTimeSeries, modelTimeStep, numCOArrayElem );
    }    

    /* Get timing for start of driver and end of run */
    int julianDayForStartOfDriverTSData = getJulianDayForStartOfDriverTSData();
    int julianHourForStartOfDriverTSData= getJulianHourForStartOfDriverTSData();
    int julianDayForEndOfRun = getJulianDayForEndOfRun();
    int julianHourForEndOfRun = getJulianHourForEndOfRun();

    if ( FewsDebugFlag >= 3 ) 
    {
       logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
       "resj_main - julianHourForStartOfDriverTSData: %d\n"
       "              julianDayForStartOfDriverTSData: %d\n"
       "              julianDayForEndOfRun: %d\n"
       "              julianHourForEndOfRun: %d\n",
       julianHourForStartOfDriverTSData, julianDayForStartOfDriverTSData,
       julianDayForEndOfRun, julianHourForEndOfRun );
    }
	
    strncpy( outputStatesFileName, getOutputStateFileName(),
	     strlen(getOutputStateFileName()) );

    memset(timeStep, 0, sizeof(timeStep));
    memset(tsInOut, 0, sizeof(tsInOut));
    
    if ( numTimeSeries > MAX_TS_IO ) 
    {
       logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
       "numTimeSeries (%d) .gt. MAX_TS_IO (%d) "
       "Need to increase MAX_TS_IO",
       numTimeSeries, MAX_TS_IO );
      
       dataID = allocateTwoDemensionalArrayOfCharType(dataID, numTimeSeries, NWSRFS_ID_LEN, 0);
       dataType = allocateTwoDemensionalArrayOfCharType(dataType, numTimeSeries, NWSRFS_TYPE_LEN, 0);

       timeStep = (int*) realloc(timeStep, numTimeSeries*sizeof(int)); 
       desiredTimeStep = (int*)  realloc(desiredTimeStep, numTimeSeries*sizeof(int));
       tsInOut = (int*)  realloc(tsInOut, numTimeSeries*sizeof(int));
       Dindex = (int*)  realloc(Dindex, numTimeSeries*sizeof(int));
    }
  
    /* Retrieves the time series values from ts.txt file and store in Darray */
    getTS_information( POcurrent, numTimeSeries, dataID, dataType, timeStep, 
	               tsInOut );

    /* Compute size input/output timeseries */
    for ( i = 0; i < numTimeSeries; i++ )
    {
       desiredTimeStep[i] = getNumberOfElementsInTimeSeries( timeStep[i] );
     
       totalBufTSize += desiredTimeStep[i];
    }

    Darray = (float *) calloc( totalBufTSize + 1 , sizeof(float) );
    
    /* Initialize Dindex array to 0 */
    memset( Dindex, 0, numTimeSeries*sizeof(int) );
    
    /* Initialize next D array index variable to 0 */
    nextDarrayIndx = 0;
    
    for ( i = 0; i < numTimeSeries; i++ )
    {
       if ( FewsDebugFlag >= 3 ) 
       {
	  logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	  "dataId (%s), dataType (%s), timeStep (%d)",
	  dataID[i], dataType[i], timeStep[i]);

          logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
          "desiredTimeStep[%d] = %d\ttsInOut[i] = %d\n",
	  i, desiredTimeStep[i], tsInOut[i] );
       }

       memset(currId, 0, sizeof(currId));
       memset(currType, 0, sizeof(currType));
       get_currId_currType( dataID[i], dataType[i], currId, currType );

       if ( FewsDebugFlag >= 3 ) 
       {
	  logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	  "Before getOne . dataId (%s), dataType (%s), timeStep (%d)",
	  dataID[i], dataType[i], timeStep[i]);

          logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
          "BEFORE getOne: dataId (%s), dataType (%s), timeStep (%d), tsIO (%d)",
          currId, currType, timeStep[i], tsInOut[i] );
       }
       
       /* Find the data indices */
       Dindex[i] = nextDarrayIndx;

       nextDarrayIndx = desiredTimeStep[i] + Dindex[i];

       /* Get input time series information */
       
       if ( tsInOut[i] )
       {
	  /* Get input time series */
	  if ( FewsDebugFlag >= 3 ) 
	  {
	     logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	     "getOneTimeSeries: %s %s %d %d\n", currId, currType,
	     timeStep[i], desiredTimeStep[i] );
	     
             logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	     "Dindex = %d\n", Dindex[i] );
	  }

	  tsInputPtr = getOneTimeSeries( currId, currType, timeStep[i],
                                         &desiredTimeStep[i] );

          /* Store time series value into D array */
	  memcpy( Darray + Dindex[i], tsInputPtr->value, 
		  sizeof(float) * desiredTimeStep[i] );

          if ( FewsDebugFlag > 3 )
          {
             logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	     "After getOne %s %s %d %d\n", currId, currType, 
             timeStep[i], desiredTimeStep[i] );
             logMessageWithArgsAndExitOnError( DEBUG_LEVEL,"After getOne Dindex[%d]=[%d]", i,Dindex[i]);
	     
             int n;
	     for (n = Dindex[i]; n < Dindex[i]+desiredTimeStep[i]; n++ )
	     {
	        logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "Darray[%d]=[%f]\n",n,Darray[n]);
	     }
          }
       }
      
    } /* end i loop */    
   

    /* Run RESi-J operation 'ex58' *******************************************/
    /* Initialize  common/fcary block. Those value mimic from 
     * common/initializeFortranCommonBlocks.f 
     */
    int iusec = (int) POcurrent[3];

    int ifillc = 1;
    int ncstor = 1;
    int icday[20], ichour[20];

    memset( icday, 0, sizeof(int)*20 );
    memset( ichour, 0, sizeof(int)*20 );

    icday[0] = julianDayForEndOfRun;
    ichour[0] = julianHourForEndOfRun;

    /* Execute the RESJ (Joint Reservoir) operation - #58 */
    execute58( getParamFileName(), COcurrent, POcurrent, Darray, Dindex,
	       &icday[0], &ichour[0], &ncstor, 
	       &julianDayForStartOfDriverTSData, 
	       &julianHourForStartOfDriverTSData, 
	       &julianDayForEndOfRun, 
	       &julianHourForEndOfRun,
	       &ifillc, &iusec ); /*AVCP , &FewsDebugFlag, 
	       &ipr, &iodebug, &ierr );*/
    
     
    if(getFewsDebugFlag() >=3)
    {
      char*   char_co = (char*)malloc( (iusec)*sizeof(float)+1 );
      memcpy( char_co, COcurrent, (iusec)*sizeof(float) );
      char_co[ (iusec)*sizeof(float) ] = '\0';
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "After execute58: %s", char_co);
    }

    /* Open the output time series file */
    outputTsFilePtr = fopen( getOutputTsFileName(), "w+" );
    
    /* Writes output Timeseries data to output file (output.txt) */
    currIndex = 0;

    for ( i = 0; i < numTimeSeries; i++ )
    {
       if ( !tsInOut[i] )  /* tsInOut[i] = 0 - output ts */
       {
	  float *dataValue = (float *)calloc( desiredTimeStep[i],
	                                      sizeof(float) );

          get_currId_currType( dataID[i], dataType[i], currId, currType );
          currIndex = Dindex[i];
	  
	  /* Copy ouput from D array into local array */
	  memcpy( dataValue, Darray+currIndex, 
		  sizeof(float) * desiredTimeStep[i] );

	  /* Write the result to output file */
	  writeOneTimeSeries( outputTsFilePtr, currId, currType, timeStep[i],
		              dataValue, desiredTimeStep[i] );

	  /* Free memory */
	  if ( dataValue != NULL ) 
	  {
	     free( dataValue );
	     dataValue = NULL;
	  }
       }
    }

    /* Free memories for timeseries */
    if ( Darray != NULL )
       free(Darray); 

    freeTimeSeries();

    /* Save carryover information into statesO.txt file */
    resj_writeStates( POcurrent, COcurrent, outputStatesFileName );
    
    freeStates();

    if ( outputStatesFileName != NULL )
    {
       free( outputStatesFileName );
    }

    fclose(outputTsFilePtr);

    /* Compute CPU time for model run */
    end = clock();
    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;

    logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
    "===> TOTAL RUN TIME MODEL: %.2f (seconds)", cpu_time_used );
    
    logMessage(DEBUG_LEVEL, "Exit RESJ program" );

    closeDiagFile();

    return SUCCESS;
    
} /* end main() ------------------------------------------------------------ */

/*****************************************************************************
  * Module Name: getTS_information()
  *
  * Original Author: Cham Pham / Ai Vo 
  * 
  * Module Creation Date: 08/18/08
  * 
  * Description:
  *   Gets time series information from PO array.
  *
  * Calling Arguments:
  *
  * Name          Input/Output    Type       Description
  * PO            In              float[]    Contains parameter information
  * numTimeSeries In              int        Total input/output time series
  * dataIds       Out             char[][]   Time series identifier
  * dataType      Out             char[][]   Data type
  * timeSteps     Out             int[]      Time interval in hours
  * tsIO          Out             int[]      Number of input/output time series 
  ***************************************************************************/
/*
void getTS_information ( float PO[], int numTimeSeries,
                         char dataIds[MAX_TS_IO][NWSRFS_ID_LEN],
                         char dataTypes[MAX_TS_IO][NWSRFS_TYPE_LEN],
                         int  timeSteps[MAX_TS_IO], int tsIO[MAX_TS_IO] ) 
*/
void getTS_information ( float PO[], int numTimeSeries,
                         char** dataIds,
                         char** dataTypes,
                         int* timeSteps, int* tsIO)
{
    int  i, totalts;
    char temp[4];
    char *char_po = (char*)malloc( (numTimeSeries*5)*sizeof(float)+1 );

    /* numTimeSeries = 5; 5 = 20/4 */
    memcpy( char_po, &PO[4], ((numTimeSeries*5)*sizeof(float)) );

    char_po[ (numTimeSeries*5)*sizeof(float) ] = '\0';

    totalts = 0; 

    memset(timeSteps, 0, sizeof(timeSteps));

    for ( i = 0; i < strlen(char_po); i+=20 ) 
    {
       tsIO[totalts] = 0;

       if ( char_po[i+18] == 'I' && char_po[i+19] == 'N' )
       {
          tsIO[totalts] = 1;     
       }
              
       strncpy( dataIds[totalts], &char_po[i], 8 );
       dataIds[totalts][8] = '\0';

       strncpy( dataTypes[totalts], &char_po[i+8], 4 );
       dataTypes[totalts][4] = '\0';

       
       memset(temp,0, sizeof(temp));
       strncpy( temp, &char_po[i+13], 3 );
       //OLD strncpy( temp, &char_po[i+12], 4 );
       
       timeSteps[totalts] = atoi( temp );
       
       if ( FewsDebugFlag > 3 )
       {
	  logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	  "1.[%d] dataId (%s), dataType (%s), timeStep (%d), tsIO (%d) numOfElements (%d)",totalts,
	  dataIds[totalts], dataTypes[totalts], timeSteps[totalts], 
	  tsIO[totalts], getNumberOfElementsInTimeSeries(timeSteps[totalts]) );
       }
       
       totalts++;
       
    } /* end i loop */

    if ( numTimeSeries != totalts )
    {
       logMessageWithArgsAndExitOnError( FATAL_LEVEL,
       "numTimeSeries (%d) did not match with totalTS (%d) ... then exit !!!\n",
       numTimeSeries, totalts );
    }
    
    if ( char_po != NULL )
    {
       free( char_po );
    }
}

/*****************************************************************************
  * Module Name: get_currId_currType()
  *
  * Original Author: Cham Pham / Ai Vo 
  * 
  * Module Creation Date: 08/18/08
  * 
  * Description:
  *   Gets time series information from PO array.
  *
  * Calling Arguments:
  *
  * Name          Input/Output    Type       Description
  * dataId        In              char*      Time series Identifier 
  * dataType      In              char*      Data type
  * currId        Out             char[]     Time series identifier without
  *                                          trailing spaces
  * currType      Out             char[]     Data type without trailing spaces
  ***************************************************************************/
void get_currId_currType( char *dataId, char *dataType, 
                          char currId[], char currType[] )
{
   /* check for empty string */
   if( strlen(dataId) == 0)
   {
       logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
       "get_currId_currType:  Data ID (%s) string is empty\n", dataId );
   }
   else
   {
      strncpy(currId,dataId, NWSRFS_ID_LEN);
      currId[NWSRFS_ID_LEN-1] = '\0';
   }
   /* check for empty string */
   if( strlen(dataType) == 0)
   {
       logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
       "get_currId_currType:  Data Type (%s) string is empty\n", dataType );
   }
   else
   {
      strncpy( currType, dataType, NWSRFS_TYPE_LEN );
      currType[NWSRFS_TYPE_LEN-1] = '\0';
   }
}

char** allocateTwoDemensionalArrayOfCharType(char** array, int rows, int cols, int firstTimeFlag)
{
   int loopCnt = 0;
   if(firstTimeFlag == 1)
   {
        logMessage(DEBUG_LEVEL, "Allocating initial memory ");
        for( loopCnt = 0; loopCnt < rows; loopCnt++)
        {
           array[loopCnt]=(char*) calloc(cols, sizeof(char));
           if(array[loopCnt] == NULL)
           {
              logMessageWithArgsAndExitOnError( FATAL_LEVEL,
              "Unable to allocate initial memory");
           }
        }
   }
   else
   {
        logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "ReAllocating memory for [%d] rows", rows);
      array = (char**) realloc(array, sizeof(char*)*rows);
      if(array == NULL)
      {
       logMessageWithArgsAndExitOnError( FATAL_LEVEL,
       "Unable to reallocate memory error");
      }
      else
      {
        for(loopCnt = 0; loopCnt < MAX_TS_IO; loopCnt++)
	{
          array[loopCnt]=(char*) realloc(array[loopCnt], cols*sizeof(char));
	}
        for(loopCnt = MAX_TS_IO; loopCnt < rows; loopCnt++)
        {
           array[loopCnt]=(char*) calloc(cols, sizeof(char));
           if(array[loopCnt] == NULL)
           {
              logMessageWithArgsAndExitOnError( FATAL_LEVEL,
              "Unable to reallocate memory error ");
           }
        }
      }
   }

   return array;
}

 
