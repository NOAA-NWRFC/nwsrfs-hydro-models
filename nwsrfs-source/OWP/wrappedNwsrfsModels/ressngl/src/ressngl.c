/******************************************************************************
   Filename: ressngl.c

   Description:
   ============

   The main function for the RES-SNGL operation (#26).
      - Read the arguments passed in from the java
      - Set up any common blocks to pass in FORTRAN input file.
      - Read the current parameter file (params.txt) using the PIN26 routine
      - Determine if carryover transfer is to be performed 
         . Set up any common blocks to pass in FORTRAN input file
         . Copy saved/previous parameter and carryover array
         . Read the previous states parameter file (params_previous.txt) using
	   the PIN26 routine 
         . Read saved/previous states from statesI.txt file
         . Peform carryover transfer by executing COX26 routine
      - Check if do not perform carryover transfer, the actual states are read
        from statesI.txt file.
      - Read the time series (ts.txt) file
      - Retrive the time series values and store into D array  
      - Initialize the FCTIME common block to use by EX26 routine
      - Execute the RES-SNGL (Single Reservoir) operation #26 
      - Write state(s) information into StatesO.txt file 
      - Write the TimeSeries results into output.txt file  
      
   Modules(s):
      getCurr_IdTypeTStep()

   Inputs: 
   
   Outputs:

   Change History
   ==============
   DATE          VERSION    PROGRAMMERS        NOTES
   ----------    -------    -----------------  ----------------------
   05/09/08      1          Cham Pham          Initial implementation
   05/04/09      2          Cham Pham          Added logic to increase
                                               memory size for long runs
					       (calibration) 
   07/22/10      3          Cham/Varalakshmi   Fixed memory leak
   08/04/10      4          Cham Pham          Add REGULATE technique
   11/04/14      5          ChamP              Added numHrYrArray variable 
                                               (FB1573)
******************************************************************************/
#include <time.h>
#include "ressngl.h"


int main( int argc, char **argv )
{
   clock_t start, end;
   double cpu_time_used;

   int i, j;

   /* Variables for time series */
   int   doCOX;
   int   currTimeStep;
   int   currIndex, endIndex;
   int   workArraySize, tsBufSize;
   int   totalIO, nextDarrayIdx; 
   int   modelTimeStep, ptrTimeSeries;
   int   totTSElements, totalTS, numTS;

   int   numMods = 0;
   int   modFileSize = -1;
   int   numRun = 0;

   int   dataTimeSteps[MXTSIO]; 
   int   dataInOut[MXTSIO];
   int   dataIndices[MXTSIO];
   int	 desiredTimeStep[MXTSIO]; 
   
   char  dataIds[MXTSIO][NWSRFS_ID_LEN]; 
   char  currId[NWSRFS_ID_LEN+1];
   char	 dataTypes[MXTSIO][NWSRFS_TYPE_LEN];
   char  currType[NWSRFS_TYPE_LEN+1]; 
    
   /* Variables for parameter and states (carryover)*/
   float POcurrent[MAXP], POprevious[MAXP];
   float COcurrent[MAXC], COprevious[MAXC];
   
   float *WK = NULL;
   float *arrayTS = NULL;
   int   *numHrYrArray = NULL;
   int   MAXWK = 40000;

   TimeSeries *tsListPtr; 
   FILE *outputTsFilePtr;
   
   start = clock();

   /* Check options sent to program */
   if ( readOptions(argc, argv) < 0 )
   {
      exit(0); 
   }

   /* Get infomation from arguments (arguments.txt) */
   setInformationFromArguments( argv[1] );

   /* Set up any common blocks to pass in Fortran model_setup */
   modelsetupinit_( getParamFileName() );
   
   /* Get flag to apply ressngl "REGULATE" technique */
   struct ModelArgs modelArgs = getModelArgs();
   int regTech = modelArgs.ressnglTechnique;

   /* Allocate memory for WK array which contains param and co information */
   WK = (float *)calloc( MAXWK, sizeof(float) );
   if ( WK == NULL )
   {
      logMessageWithArgsAndExitOnError( FATAL_LEVEL,
      "Error allocating memory (WK)");      
   }

   /* Initialize P and C arrays */
   memset(POcurrent, 0, sizeof(POcurrent));
   memset(COcurrent, 0, sizeof(COcurrent));
   memset(POprevious, 0, sizeof(POprevious));
   workArraySize = -1;

   /* Read the current parameter file (params.txt) using the pin routine */
   pin26_( POcurrent, COcurrent, WK, &MAXWK ); 

   /* Call routine to set workspace size in P array this used to be done at end
    * of PIN; was moved out because it depends on NTIMESTEPS 
    */
   wksp26_( POcurrent, &workArraySize );

   /* PO(16): num words of work space needed */
   POcurrent[15] = workArraySize;         
   
   /* Determine if carryover transfer is to be performed */
   doCOX = FALSE;
   
   if ( strcmp(getPrevParamsInfo(), "PARAMS_UNCHANGED") != 0 )
   {
      /* Set doCOX variale to TRUE if do carryover transfer. */
      doCOX = TRUE;
      
      /* Set up any common blocks to pass in FORTRAN input file */
      modelsetupinit_( getPrevParamsInfo() );

      /* Read the previous/saved states parameter file (params_previous.txt)
       * using the PIN26 routine 
       */
      pin26_( POprevious, COprevious, WK, &MAXWK );
      
      /* Initialize the COprevious array since the actual states are read 
       * from statesI.txt unlike the pin routine filling the values from the 
       * params file 
       */ 
      memset( COprevious, 0, sizeof(COprevious) );
	     
      /* Read the previous/saved states (carryover) from statesI.txt file */
      readStateRessngl( POprevious, COprevious );

      /* Call routine to set workspace size in P array this used to be done
       * at end of PIN; was moved out because it depends on NTIMESTEPS
       */
      wksp26_( POprevious, &workArraySize );
      POprevious[15] = workArraySize;

      /* Peform carryover transfer by executing COX26 routine */
      cox26_( POprevious, COprevious, POcurrent, COcurrent );
   }

   /* If do NOT perform Carryover transfer, we need to read the actual states
    * from statesI.txt.
    */
   if ( doCOX == FALSE )
   {
      /* Initialize the COcurrent array since the actual states are read from 
       * (statesI.txt) unlike the pin routine filling the values from the 
       * params file 
       */
      memset( COcurrent, 0, sizeof(COcurrent) );
   
      /* Read the current states (carryover) from statesI.txt file */
      readStateRessngl( POcurrent, COcurrent );
   }

   /* Read time series (ts.txt) file */
   readAllInputTimeSeries( );

   /* Get data time interval  PO(7) (unit HR) */  
   modelTimeStep = (int)POcurrent[6];   
   
   /* Get general time series information pointer in PO array PO(11) */            
   ptrTimeSeries = (int)POcurrent[10]; 

   /* Get number of general and specific time series */             
   numTS = (int)POcurrent[ptrTimeSeries-1]; 

   if ( getFewsDebugFlag() > 4 ) 
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	    "modelTimeStep: %d\tptrTimeSeries: %d\tnumST: %d",
	     modelTimeStep, ptrTimeSeries, numTS );
   }
   
   totalIO = 0;
   create_data_arrays_( POcurrent, dataIds, dataTypes, dataTimeSteps, dataInOut,
	                &totalIO );

   /* Initialize next D array index to 1 */   
   nextDarrayIdx = 1; 
   
   /* Retrive the count for each input time series */
   totTSElements = 0;
   totalTS = 0;
   
   for ( i = 0; i < numTS; i++ )
   {
      dataIndices[i] = 0;
      
      if ( dataInOut[i] >= 0 )
      {
         getCurr_IdTypeTStep( dataIds[i], dataTypes[i], dataTimeSteps[i],
                              currId, currType, &currTimeStep );

         desiredTimeStep[i] = getNumberOfElementsInTimeSeries( currTimeStep );

         numRun = desiredTimeStep[i];
         
	 totTSElements += desiredTimeStep[i];
         totalTS++;

         if ( getFewsDebugFlag() > 4 ) 
	 {
            logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	    "desiredTimeStep= %d totalIO= %d\n", desiredTimeStep[i], totalIO );
	 }

         /* Find the data indices */
         dataIndices[i] = nextDarrayIdx;
         nextDarrayIdx  = desiredTimeStep[i] * (currTimeStep / modelTimeStep) +
     	                  dataIndices[i];
			  
         if ( getFewsDebugFlag() > 4 ) 
	 {
            logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	    "\ndataIndices[%d] = %d nextDarrayIdx = %d (%s = %d)\n",
            i, dataIndices[i], nextDarrayIdx, currType, dataInOut[i] );
	 }
      }	 
   }

   /* Allocate memmory for time series array */
   tsBufSize = totTSElements * totalTS;
   
   arrayTS = (float *) calloc( tsBufSize+1, sizeof(float) );
   if ( arrayTS == NULL )
   {
      logMessageWithArgsAndExitOnError( FATAL_LEVEL,
      "Error allocating memory for arrayTS ");
   }

   //FB 1573 11/06/14
   numHrYrArray = (int *) calloc( numRun, sizeof(int) );
   if ( numHrYrArray == NULL )
   {
       logMessageWithArgsAndExitOnError( FATAL_LEVEL,
       "Error allocating memory for numHrYrArray");
   }
   /* Retrive the data for each input time series when executed for
    * the driving time series
    */
   for ( i = 0; i < numTS; i++ )
   {
      if ( dataInOut[i] == TSINPUT )
      {
         getCurr_IdTypeTStep( dataIds[i], dataTypes[i], dataTimeSteps[i],
                              currId, currType, &currTimeStep );

         currIndex = dataIndices[i] - 1;
	 
	 if ( getFewsDebugFlag() > 3 ) 
	 {
	    logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	    "\ncurrId (%s), currType (%s), currTimeStep (%d)\n",
	    currId, currType, currTimeStep );
	    logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	    "currIndex = %d\tendIndex = %d\n", currIndex, 
	    currIndex + desiredTimeStep[i] );
	 }  

	 tsListPtr = getOneTimeSeries( currId, currType, currTimeStep,
   				       &desiredTimeStep[i] );

	 /* Store data value into TS array */
         if ( tsListPtr != NULL )
	 {
            memcpy( arrayTS+currIndex, tsListPtr->value, 
		    sizeof(float)*desiredTimeStep[i] );
	 }
         
      } /* if ( dataInOut[i] == 0 ) */
   } 

   /* Change the size of the memory for WK  */
   //workArraySize = (workArraySize * 4) + tsBufSize;
   //
   workArraySize *= 4;

   if ( workArraySize > MAXWK ) 
   {
      WK = (float*)realloc( WK, sizeof(float) * workArraySize );
      if ( WK == NULL )
      {
         logMessageWithArgsAndExitOnError( FATAL_LEVEL,
	 "Error (re)allocating memory for WK ");
      }
   }

   /* Initialize the FCTIME and MOD126 common blocks to use by 
    * model_executable 
    */
   numMods = parseModsFile( getModFileName(), &modFileSize );

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "Including mods from file = %s\n", getModFileName() );
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "Mods file size is = %d\n", modFileSize );
   }

   if ( modFileSize <= 0 || numMods <= 0 )
      numMods = 1;  //need to set array size for vm126

   float *vm126 = (float*)calloc( numMods, sizeof(float) );

   modelexecinit_( &modelTimeStep, &modFileSize, vm126, &numMods, 
	           &regTech );
   
   /* Close Mod file */
   close_mod_file();

   /* Execute the RES-SNGL (Single Reservoir) operation - #26 */
   ex26_( POcurrent, COcurrent, arrayTS, dataIndices, WK, vm126, numHrYrArray );
   
   /* Free memory for WK and vm126, numHrYrArray */
   if ( vm126 != NULL )
      free ( vm126 );

   if ( numHrYrArray != NULL )
      free ( numHrYrArray );

   if ( WK != NULL )
      free ( WK );

   /* Write state(s) at end of model run to StatesO.txt file */
   writeStateRessngl( POcurrent, COcurrent, getOutputStateFileName() );

   /* Open the output file */
   outputTsFilePtr = fopen( getOutputTsFileName(), "w+" );

   if ( outputTsFilePtr == NULL )
   {
      logMessageWithArgsAndExitOnError( FATAL_LEVEL,
      "ERROR: Could not open %s\n", getOutputTsFileName() );
   }

   /* Write the output time series returned from ex26_() to output file 
    * (output.txt) 
    */
   for ( i = 0; i < numTS; i++ )
   {
      if ( dataInOut[i] == TSOUTPUT )
      {
         float *dataValue = (float *)calloc( desiredTimeStep[i]+1, 
	                                     sizeof(float) );
	 
         if ( dataValue == NULL )
	 {
	    logMessageWithArgsAndExitOnError( FATAL_LEVEL,
		  "Error allocating memory for dataValue");
         }
	 
         getCurr_IdTypeTStep( dataIds[i], dataTypes[i], dataTimeSteps[i],
                              currId, currType, &currTimeStep );

         currIndex = dataIndices[i] - 1;
         
         if ( getFewsDebugFlag() > 3 ) 
	 {    
            logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	    "dataIds[%d] = %s\tdataTypes = %s\tdataTimeSteps = %d\n",
  	    i, currId, currType, currTimeStep );
	 }

         /* Copy output from TS array into local array */
         memcpy( dataValue, arrayTS + currIndex, 
	         sizeof(float) * desiredTimeStep[i] ); 
	 
	 /* Write the result to output file */
	 writeOneTimeSeries( outputTsFilePtr, currId, currType, currTimeStep,
	                     dataValue, desiredTimeStep[i] );

         /* Free memory */
         if ( dataValue != NULL ) 
	 {
            free( dataValue );
	    dataValue = NULL;
	 }
   
      } /* if ( dataInOut[i] == 1 ) */
   } /* end for i loop */

   /* Free memory for TS array */
   if ( arrayTS != NULL )
      free( arrayTS );

   freeTimeSeries();

   freeStates();

   /* Close the output file */
   fclose( outputTsFilePtr );

   logMessage( DEBUG_LEVEL, "Exit RESSNGL program" ); 

   /* Compute CPU time for model run */
   end = clock();
   cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
   logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
   "===> TOTAL RUN TIME FORTRAN MODEL: %.2f (seconds)", cpu_time_used );

   closeDiagFile();

   return ( SUCCESS );

}/* main() ----------------------------------------------------------------- */

/******************************************************************************
   Module: getCurr_IdTypeTStep()

   Description:
   ============
      This function will store the time series information.

   Input:
   ------
   char*    dataId	   - TS identifier
   char*    dataType	   - data type
   int      dataTimeStep   - time step

   Output:
   -------
   char[]   currId	   - current TS identifier
   char[]   currType	   - current data type
   int*	    timeStep	   - current time step

   Constant/Global variable
   NWSRFS_ID_LEN     8
   NWSRFS_TYPE_LEN   4

   Change History
   ==============

   DATE          VERSION    PROGRAMMERS           NOTES
   ----------    -------    -----------------     ----------------------
   05/09/08      1          Cham Pham             Initial implementation

******************************************************************************/
void getCurr_IdTypeTStep( char *dataId, char *dataType, int dataTimeStep,
                          char currId[], char currType[], int *timeStep )
{
   strncpy( currId, dataId, NWSRFS_ID_LEN );
   currId[NWSRFS_ID_LEN] = '\0';
 
   strncpy( currType, dataType, NWSRFS_TYPE_LEN );
   currType[NWSRFS_TYPE_LEN] = '\0';

   *timeStep = dataTimeStep;

} /* getCurr_IdTypeTStep() --------------------------------------------------*/
