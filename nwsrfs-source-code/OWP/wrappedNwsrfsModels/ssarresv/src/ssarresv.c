/******************************************************************************
   Filename: ssarresv main.c

   Description:
   ============
      The main function for the SSARESV operation (#51).
      1. Reads arguments passed in from fews
      2. Reads time series from ts.txt file
      3. Sets up common blocks to pass in FORTRAN input file.
      4. Reads parameters from params.txt file using the PIN51  routine
      5. Stores time series that read in from ts.txt into D array 
      6. Reads states (carryover) from statesI.txt file 
      7. Initializes the FCTIME common block to use by EX51 routine
      8. Executes the SSARRESV operation - #51 
      9. Saves carryover information to statesO.txt file 
     10. Writes output time series to output.txt file 
       
   Inputs: 
   
   Outputs:

   Change History
   ==============
   DATE          VERSION    PROGRAMMERS        NOTES
   ----------    -------    -----------------  ----------------------
   08/09/08      1          Lee C./ Raymond C./ Ai V.  Initial implementation
   01/14/09                 Refactor code - added readStates and modified 
                                            ssarresv_write_states_files
   05/09/10      2          Cham P.       - Added logic to reallocate memory 
                                            for workArray
   07/22/10      3          Cham/Varalakshmi - Fixed memory leak
******************************************************************************/

#include "ssarresv.h"
#include "fortranCommonIncludes.h"
#include <time.h>

int main (int argc, char** argv)
{            
      clock_t start, end;
      double cpu_time_used;
      
      int i,j;      
      int modelTimeStep;
      int pIndex;
      int numberOfTimeSeries;     
      int currentTimeStep;
      int currentIndex; 
      int endIndex;
      int numberOfTimeSteps;
      int wArraySize = -1;
      int numMods = 0; 
      int modFileSize = -1;
      
      int dataTimeSteps[MAXINPUT_TS+MAXOUTPUT_TS];      
      int dataIndices[MAXINPUT_TS+MAXOUTPUT_TS];
      int inOrOut[MAXINPUT_TS+MAXOUTPUT_TS];      
      float POcurrent[MAXP];
      float POprevious[MAXP];       
      float COcurrent[MAXC];
      float COprevious[MAXC];      
      float *dataValues = NULL; 
      float *timeSeriesArray = NULL;         
      char currentId[NWSRFS_ID_LENGTH+1];
      char currentType[NWSRFS_TYPE_LENGTH+1];       
      char dataIds[MAXINPUT_TS+MAXOUTPUT_TS][NWSRFS_ID_LENGTH];
      char dataTypes[MAXINPUT_TS+MAXOUTPUT_TS][NWSRFS_TYPE_LENGTH];
      TimeSeries *tsListPtr;
      int outputCount[MAXINPUT_TS+MAXOUTPUT_TS];
      int julianDayForStartOfDriverTSData;
      int julianHourForStartOfDriverTSData;
      int julianDayForEndOfRun;
      int julianHourForEndOfRun;
      int doCarryOverXfer = FALSE;
      int MAXWARRAY = 20000;
      float *workArray = NULL;

      start = clock();

      /* 
       * Check for appropriate input arguments 
       */
      if( readOptions( argc, argv ) < 0 )
      {
         exit(0);
      }
      
      /* Pass input argument and Create diagnostic file */
      setInformationFromArguments( argv[1] );
      
      logMessage( DEBUG_LEVEL, "Entering SARRESV" );

      FILE *outputTsFilePtr = fopen( getOutputTsFileName(), "w+" );
      FILE *outputStateFilePtr = fopen( getOutputStateFileName(), "w+" );
      
      /* Load timeseries from file (ts.txt) */
      tsListPtr = readAllInputTimeSeries();

      if ( tsListPtr == NULL )
      {
          logMessageAndExitOnError(FATAL_LEVEL,
	  " Errror reading input Timeseries"
	  " Exiting SSARRESV.....");
      }
      
      /* sets path for ssarresv to open params.txt file used by pin51 */
      
      char *fileName = getParamFileName();

      model_setup_init_( fileName );
            
      /* reads Timeseries info, carryover and stores data into Work array 
       * (workArray) and PO array
       */
      workArray = (float*) calloc( MAXWARRAY, sizeof( float ) );

      if ( workArray == NULL )
      {
	 logMessageWithArgsAndExitOnError( FATAL_LEVEL,
	 "Error allocating memory for workArray");
      }

      memset(POcurrent, 0, sizeof(POcurrent));
      memset(COcurrent, 0, sizeof(COcurrent));
      memset(POprevious, 0, sizeof(POprevious));
      memset(COprevious, 0, sizeof(COprevious));
      memset(dataIndices, 0, sizeof(dataIndices));
      memset(dataTimeSteps, 0, sizeof(dataTimeSteps));
      memset(inOrOut, 0, sizeof(inOrOut));

      pin51_( POcurrent, COcurrent, workArray, &MAXWARRAY);

      /* if parameters file has been updated, performing Carryover Transfer */
      if( strcmp(getPrevParamsInfo(), "PARAMS_UNCHANGED") != 0 )  
      {
         /* sets path for ssarresv to open params_previous.txt file used by 
	  * pin51 
	  */
         model_setup_init_( getPrevParamsInfo() );

         /* Read the previous/saved states parameter file (params_previous.txt)
         * using the PIN26 routine 
         */
         pin51_( POprevious, COprevious, workArray, &MAXWARRAY);

	 memset( COprevious, 0, sizeof( COprevious ) ); 
 
         /* Read input states from statesI.txt file */
	 readStates( POprevious, COprevious );   

         /* do carryover transfer, comparing current/previous for P array and
          * CO array 
          */
	 cox51_( POprevious, COprevious, POcurrent, COcurrent ); 

	 doCarryOverXfer = TRUE;
      }
      
      /* perform SSARR single operation if there is no changes in input 
       * parameters 
       */
      if(!doCarryOverXfer)
      {
         /* memset COcurrent since the actual states are read from statesI.txt 
	  * unlike the pin routine filling the values from the params file 
	  * - initializing COcurrent
	  */
         memset( COcurrent, 0, sizeof( COcurrent ) );
	 
         /* Read input states from statesI.txt file */  
         readStates( POcurrent, COcurrent );     
      }
      
      /* process input TS */
      modelTimeStep = (int)POcurrent[7-1];

      pIndex = (int)POcurrent[12 -1];      

      numberOfTimeSeries = (int)POcurrent[pIndex-1];      

      create_data_arrays_( POcurrent, dataIds, dataTypes, dataTimeSteps );
      
      store_currentid_type_and_timestep(dataIds[1], dataTypes[1], 
                                        dataTimeSteps[1], currentId, 
					currentType, &currentTimeStep );
      if (getFewsDebugFlag() > 3) 
      {
	   logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
           "currentID = %s currentType = %s currentTimeStep = %d\n",
           currentId, currentType, currentTimeStep );	        
      }	
          
      numberOfTimeSteps = getNumberOfElementsInTimeSeries( currentTimeStep );
      
      if ( numberOfTimeSteps == 0 )
      {
         logMessageAndExitOnError( FATAL_LEVEL,
	 "Error: Number of Time Step equals Zero" );                         
      }

      /* 
       * call routine to set workspace size in P array
       *  this used to be done at end of PIN; was moved 
       *  out because it depends on NTIMESTEPS 
       */
      wksp51_( POcurrent, &wArraySize );

      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
         "==> WorkArray Size = %d\n", wArraySize);
      }
            
      POcurrent[16-1] = wArraySize; /*POcurrent[16] = work array size*/

      create_data_index_( POcurrent, &numberOfTimeSteps, dataIndices, inOrOut );

      timeSeriesArray = (float *) calloc( (numberOfTimeSeries * 
			numberOfTimeSteps)+1, sizeof(float) );

      /* Get input time series information */
      for ( i = 0; i < numberOfTimeSeries; i++ ) 
      {
          if ( inOrOut[i] == TS_INPUT ) 
	  {
	     store_currentid_type_and_timestep( dataIds[i], dataTypes[i], 
	                                        dataTimeSteps[i], currentId, 
						currentType, &currentTimeStep );

	     currentIndex = dataIndices[i] - 1;

	     tsListPtr = getOneTimeSeries( currentId, currentType, 
	                                 currentTimeStep, &numberOfTimeSteps );
             
             if ( tsListPtr != NULL )
	     {
                 memcpy( timeSeriesArray+currentIndex, tsListPtr->value, 
		         sizeof(float)*numberOfTimeSteps );
             }
	  }
      }
    
      /* Read Mod file */
      numMods = parseModsFile( getModFileName(), &modFileSize );

      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	 "Including mods from file = %s\n", getModFileName() );
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	 "Mods file size is = %d\n", modFileSize );
      }

      if ( modFileSize <= 0 || numMods <= 0 )
         numMods = 1; //need to set array size to use by fortran module.

      model_execution_init_( &modelTimeStep, &modFileSize , &numMods );

      /* Close Mod file */
      close_mod_file();

      wArraySize *= 4; 


      if ( wArraySize > MAXWARRAY )
      {
         workArray = (float*)realloc( workArray, sizeof(float)*wArraySize);

         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
            "Reallocating memory for workArray with size[%d]", wArraySize);

         if ( workArray == NULL )
         {
            logMessageWithArgsAndExitOnError( FATAL_LEVEL,
            "Error (re)allocating memory for workArray ");
         }

         memset( workArray + (MAXWARRAY -1) , 0, 
	         sizeof(float)*(wArraySize - MAXWARRAY) );
      }
      	 
      /* Execute the SSARR Reservoir Regulation operation - #51 */
      ex51_( POcurrent, COcurrent, timeSeriesArray, dataIndices, workArray );      
      if ( workArray != NULL )
         free( workArray );

      /* Output states value to statesO.txt */
      ssarresv_write_states_output( outputStateFilePtr, POcurrent, COcurrent );

      /* Process output time series */
      for( i = 0; i < numberOfTimeSeries; i++ ) 
      {
          if(inOrOut[i] == TS_OUTPUT) 
	  {
             dataValues = (float *)calloc(numberOfTimeSteps + 1, sizeof(float));	     

	     if ( dataValues == NULL )
             {
                logMessageWithArgsAndExitOnError( FATAL_LEVEL,
                 "Error allocating memory for dataValue");
             }

             store_currentid_type_and_timestep( dataIds[i], dataTypes[i],
                                               dataTimeSteps[i], currentId,
                                                currentType, &currentTimeStep );
             
	     currentIndex = dataIndices[i] - 1;
 
             memcpy( dataValues, timeSeriesArray + currentIndex,
                     sizeof(float) * numberOfTimeSteps);

             writeOneTimeSeries( outputTsFilePtr, currentId, 
	                        currentType, currentTimeStep,
		                dataValues, numberOfTimeSteps );
			
	      /* Free memory */
             if ( dataValues != NULL )
	     {
                free( dataValues );
		dataValues = NULL;
	     }
	  }
	 
      }
     /* Free memory for all variables */ 
     free( timeSeriesArray );
  	
     freeTimeSeries();

     freeStates();

     /* done reading, close output files */
     fclose( outputTsFilePtr );
     fclose( outputStateFilePtr );      
     
     /* Compute CPU time for model run */
     end = clock();
     cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;

     logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
     "===> TOTAL RUN TIME FORTRAN MODEL: %.2f (seconds)", cpu_time_used );
    
     logMessage( DEBUG_LEVEL, "Exiting SARRESV" );

     closeDiagFile();

     return SUCCESS;
}   

/******************************************************************************
   Module: store_currentid_type_and_timestep()
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
*******************************************************************************/  
void store_currentid_type_and_timestep( char *dataId, char *dataType, 
                                        int dataTimeStep, char currId[], 
					char currType[], int *timeStep )
{
   strncpy( currId, dataId, NWSRFS_ID_LENGTH );
   currId[NWSRFS_ID_LENGTH] = '\0';
 
   strncpy( currType, dataType, NWSRFS_TYPE_LENGTH );
   currType[NWSRFS_TYPE_LENGTH] = '\0';

   *timeStep = dataTimeStep;      
}
/******************************************************************************
input : float *pArray
output: float *cArray
Routine: reads in values from StatesI.txt and fills in CO array
called routine: ssarresv_populate_states()
******************************************************************************/
void readStates( float *pArray, float *cArray )
{
      int stateUnit = -1;
      int numbOfStates = -1;
      int numberOfCarryoverValues;
      int reservoirTypeIndicator;
      int parameterUnit;
      int parmInfoPtr;      

      readStatesFromFile( &numbOfStates, &stateUnit );     

      parameterUnit = (int)pArray[8 - 1];
      numberOfCarryoverValues = (int)pArray[15 - 1];   
      parmInfoPtr = (int)pArray[11 - 1];
      
      if ( stateUnit != parameterUnit )
      {
	 logMessageAndExitOnError( FATAL_LEVEL,"Error: state input unit is not "
	                           " equal to parameter input unit...\n" );
      }   

      //number of input States is not equal to nuber of carry over in P arrays
      //encountered error
      if ( numbOfStates != numberOfCarryoverValues )
      {
          
	 logMessageAndExitOnError( FATAL_LEVEL, " Number of Input States is not"                       
         " equal to Number Carry Over\n" );
      }
      /*number of carryover values stored in position 15 of P array
      there are two cases 5 or 9 values
      for now the carryover is hard-coded for selwe(5) and slmwe(9)
      we want to read these in from our states.txt file */
      reservoirTypeIndicator = (int)pArray[parmInfoPtr-1];

      /* stores states values to CO array */
      ssarresv_populate_states( cArray, numberOfCarryoverValues, 
                                reservoirTypeIndicator );
}
