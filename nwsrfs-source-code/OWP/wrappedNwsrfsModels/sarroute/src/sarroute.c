/******************************************************************************
   Filename: sarroute.c

   Description:
   ============
      1. Use the arguments file passed in from fews
      2. Reads time series from ts.txt file
      3. Reads parameters from params.txt 
      4. Checks if the params are changed. If so read the params_saved.txt file
         and call the cox44 routine to get the carry over. Else read the 
         carry over states from statesI.txt file
      5. Executes the SARROUTE operation - #44
      6. Saves output carryover states.
      7. Writes output time series.

   Inputs:

   Outputs:

   Change History
   ==============
   DATE          VERSION    PROGRAMMERS          NOTES
   ----------    -------    -----------------    ----------------------
                  1         Varalakshmi Rajaram  Initial Implementation
   07/22/10       2         Varalakshmi/Cham     Fixed memory leak
******************************************************************************/

#include "sarroute.h"
#include <time.h>

int main(int argc, char **argv)
{
   clock_t start, end;
   double cpu_time_used;
   
   float pCurrent[1000], cCurrent[1000];
   float pPrevious[1000], cPrevious[1000];
   int doCox = 0;

   /* All begin interval inflow data mentioned in ts.txt*/
   float **beginInflowTsData = (float**) malloc(sizeof(float*)); 
   *beginInflowTsData = NULL;
   /* All end interval inflow data mentioned in ts.txt */
   float **endInflowTsData = (float**) malloc(sizeof(float*)); 
   *endInflowTsData = NULL;

   /* All begin interval outflow data from model output*/ 
   float *beginOutflowTsData = NULL;
   /* All end interval outflow data from model output */
   float *endOutflowTsData = NULL;  

   start = clock();
   
   if( readOptions(argc, argv) < 0 )
   {
     exit(0);
   }

   setInformationFromArguments(argv[1]);

   /* Initialize p and c arrays */
   memset(pCurrent, 0, sizeof(pCurrent));
   memset(cCurrent, 0, sizeof(cCurrent));
   memset(pPrevious, 0, sizeof(pPrevious));
   memset(cPrevious, 0, sizeof(cPrevious));

   /*load the time series first */
   readAllInputTimeSeries();

   char paramsFileName[FILE_NAME_LENGTH];
   memset(paramsFileName, 0, sizeof(FILE_NAME_LENGTH));
   strcpy(paramsFileName, getParamFileName());

   /* read params.txt */
   pin44_(pCurrent, cCurrent, paramsFileName);
   logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "After pin read from[%s]\n", 
                                        paramsFileName);

   char prevParamsInfo[FILE_NAME_LENGTH];
   memset(prevParamsInfo, 0, sizeof(FILE_NAME_LENGTH));
   strcpy(prevParamsInfo, getPrevParamsInfo());

   if (strcmp(prevParamsInfo, "PARAMS_UNCHANGED"))
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "Params changed ...");
      doCox = 1;

      /* read params_saved.txt */
      pin44_(pPrevious, cPrevious, prevParamsInfo);

      logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "After pin read from [%s]\n",
                                                     prevParamsInfo);

      /* memset cCurrent since the actual states are read from statesI.txt 
         unlike the pin routine filling the values from the params file */
      memset(cPrevious, 0, sizeof(cPrevious));

      /* read states which actually matches the old params */
      readStates(pPrevious, cPrevious);

      logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
                       "After carray read from statesI.txt");

      cox44_(pPrevious, cPrevious, pCurrent, cCurrent);

      logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
                       "Carry over transfer is done");

   }
   if (doCox == 0)
   {
      /* memset cCurrent since the actual states are read from statesI.txt 
         unlike the pin routine filling the values from the params file */
      memset(cCurrent, 0, sizeof(cCurrent));

      /* read states which actually matches the params.txt */
      readStates(pCurrent, cCurrent);
   }
   
   /* read input time series from input ts file */

   int driverResolution = getIntegerFromPArray(pCurrent, 38);
   
   getRequiredInputTimeSeries(pCurrent, beginInflowTsData, endInflowTsData, 
                               driverResolution);

   beginOutflowTsData = (float*) calloc(outputCount, sizeof(float));  
   endOutflowTsData = (float*) calloc(outputCount, sizeof(float));  
  
   if(beginOutflowTsData == (float*)NULL)
   {
         logMessage(DEBUG_LEVEL,"beginOutflowTsData is NULL");
   }
   if(endOutflowTsData == (float*)NULL)
   {
         logMessage(DEBUG_LEVEL,"endOutflowTsData is NULL");
   }

   /* call ex routine thereby execute the model */
   ex44_(pCurrent, cCurrent, *beginInflowTsData, *endInflowTsData, 
                              beginOutflowTsData, endOutflowTsData);

   /* write output time series */
   char *outputTsFileName = getOutputTsFileName();
   FILE *outputTsFilePtr = fopen(outputTsFileName, "w");

   char *beginOutflowTimeSeriesId = getTimeSeriesIdFromPArray(pCurrent, 26);
   char *beginOutflowTimeSeriesDataTypeCode = getTimeSeriesCodeFromPArray(pCurrent, 28);


   if(strcmp(beginOutflowTimeSeriesDataTypeCode, "NONE") != 0)
   {
     writeOneTimeSeries(outputTsFilePtr, beginOutflowTimeSeriesId,
			beginOutflowTimeSeriesDataTypeCode, driverResolution,
			beginOutflowTsData, outputCount);
   }

   char *endOutflowTimeSeriesId = getTimeSeriesIdFromPArray(pCurrent, 29);
   char *endOutflowTimeSeriesDataTypeCode = getTimeSeriesCodeFromPArray(pCurrent, 31);

   writeOneTimeSeries(outputTsFilePtr, endOutflowTimeSeriesId,
		      endOutflowTimeSeriesDataTypeCode, driverResolution,
		      endOutflowTsData, outputCount);

   writeStates(pCurrent, cCurrent);

   if ( outputTsFilePtr != NULL )
      fclose( outputTsFilePtr );
   
   if(beginOutflowTsData != NULL)
      free(beginOutflowTsData);

   if(endOutflowTsData != NULL)
      free(endOutflowTsData);

   if(beginInflowTsData != NULL)
      free(beginInflowTsData);

   if(endInflowTsData != NULL)
      free(endInflowTsData);

   freeTimeSeries();

   freeStates();

   /* Compute CPU time for model run */
   end = clock();
   cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
   
   logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
   "===> TOTAL RUN TIME FORTRAN MODEL: %.2f (seconds)", cpu_time_used ); 
   
   logMessage(DEBUG_LEVEL,"Exiting sarroute");

   closeDiagFile();

   return(SUCCESS);
}

/*******************************************************************************
Module :
        getRequiredInputTimeSeries()
Input  :
	float * pArray - pArray populated by the pin routine.
        float **qInStTsData - Initial start inflow timeseries
        float **qInEnTsData  - Initial end inflow timeseries
        int driverResolution - driver resolution

Description :
        Read the pArray info and populates the initial start and
        end inflow timeseries
*******************************************************************************/

void getRequiredInputTimeSeries(float *pArray, 
				float **qInStTsData, float **qInEnTsData,
                                int driverResolution)
{
   char *beginInflowTimeSeriesId = NULL;
   char *beginInflowTimeSeriesDataTypeCode = NULL;
   char *endInflowTimeSeriesId = NULL;
   char *endInflowTimeSeriesDataTypeCode = NULL;

   int count;
   beginInflowTimeSeriesId = getTimeSeriesIdFromPArray(pArray, 20);
   beginInflowTimeSeriesDataTypeCode = getTimeSeriesCodeFromPArray(pArray, 22);
  

   int tempFlag = 0;
   if (beginInflowTimeSeriesId == NULL)
   {
      logMessageWithArgsAndExitOnError(FATAL_LEVEL, 
      "Error: Begin inflow TS Id is null");
   }
   if(beginInflowTimeSeriesDataTypeCode == NULL)
   {
      logMessageWithArgsAndExitOnError(FATAL_LEVEL,
      "Error: Begin inflow DataTypeCode is null");
   }

   logMessageWithArgsAndExitOnError(DEBUG_LEVEL,"%s %s", 
            beginInflowTimeSeriesId, beginInflowTimeSeriesDataTypeCode);

   count = getNumberOfElementsInTimeSeries(driverResolution);
   logMessageWithArgsAndExitOnError(DEBUG_LEVEL,  "Output count = %d",count );
   outputCount = count;
   
   TimeSeries *qInStTs = NULL;
   if(beginInflowTimeSeriesDataTypeCode != NULL)
   {
      if(strcmp(beginInflowTimeSeriesDataTypeCode, "NONE") != 0)
      {
         qInStTs = getOneTimeSeries(beginInflowTimeSeriesId,
                  beginInflowTimeSeriesDataTypeCode, driverResolution, &count);
         if(qInStTs != NULL)
         {
            *qInStTsData = qInStTs->value;
         }
      
      }
   }

   endInflowTimeSeriesId = getTimeSeriesIdFromPArray(pArray, 23);
   endInflowTimeSeriesDataTypeCode = getTimeSeriesCodeFromPArray(pArray, 25);

   if(endInflowTimeSeriesId == NULL)
   {
     logMessageWithArgsAndExitOnError(FATAL_LEVEL,
                "Error: End inflow TS Id is null");
   }
   if(endInflowTimeSeriesDataTypeCode == NULL)
   {
      logMessageWithArgsAndExitOnError(FATAL_LEVEL,
                "Error: End inflow DataTypeCode is null");
   }
   TimeSeries *qInEnTs = getOneTimeSeries(endInflowTimeSeriesId,
                   endInflowTimeSeriesDataTypeCode, driverResolution, &count);
                                    

   if(qInEnTsData != NULL)
   {
     *qInEnTsData = qInEnTs->value;
   }

   logMessageWithArgsAndExitOnError(DEBUG_LEVEL,"%s %s", 
	    endInflowTimeSeriesId, endInflowTimeSeriesDataTypeCode);

}


/******************************************************************************
Module: readStates()

Description : reads statesI.txt and filles the cArray

input : float *pArray

output: float *cArray

Change History: 
Date    Version Programmers         Notes
-----   ------- -----------         -----
                Varalakshmi Rajaram Initial implementation
*******************************************************************************/

void readStates(float* pArray, float *cArray)
{
   /* read states file & populate  carray */
   int numOfStatesKeys, numOfStateUnits;
   float returnStateValue;

   readStatesFromFile(&numOfStatesKeys, &numOfStateUnits);

   int numOfInflowTS =  getIntegerFromPArray(pArray, 32);
   logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
                 "Before reading the states input...Num of inflow TS [%d]", 
                  numOfInflowTS);
   int indexOfCArray = 1;

   if (numOfInflowTS == 1)
   {
  	 returnStateValue = populateFloatStateValue("INITIAL_START_INFLOW", 
                                        cArray, indexOfCArray);

	 logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
                                 "INITIAL_START_INFLOW [%f]", returnStateValue);

         indexOfCArray++;
   }
   else if(numOfInflowTS == 2)
   {
         /* indexOfCArray remains to be 1 as initialized in code above */
   }

   int numOfRoutingPhases = getIntegerFromPArray(pArray, 34);
   logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
             "Before reading the states input...Num of routing phases[%d]", 
              numOfRoutingPhases);

   char key[100];
   char tempStr[10];
   int index;
   
   memset(key, '\0', sizeof(key));
   memset(tempStr, '\0', sizeof(tempStr));

   strcpy(key, "PHASE_FLOW_VALUE_FOR_REACH");
   populateArrayOfFloatStateValues(key, numOfRoutingPhases, cArray, 
                                   indexOfCArray);
}

/******************************************************************************
Module: writeStates()

Description : read the cArrayy write statesO.txt 

input : float *pArray
        float *cArray

Change History: 
Date    Version Programmers         Notes
-----   ------- -----------         -----
                Varalakshmi Rajaram Initial implementation
*******************************************************************************/
void writeStates(float* pArray, float *cArray)
{

   /* write output states */
   FILE *outputStateFilePtr = fopen(getOutputStateFileName(), "w+");
   
   int numOfInflowTS =  getIntegerFromPArray(pArray, 32);
   logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
             "Before writing the states output...Num of inflow TS [%d]", 
              numOfInflowTS);
   int indexOfCArray = 1;
  
   int numOfRoutingPhases = getIntegerFromPArray(pArray, 34);
   logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
             "Before writing the states output...Num of routing phases[%d]", 
              numOfRoutingPhases);
  
   if(numOfInflowTS == 1)
   {
         writeStringStateToFile(outputStateFilePtr, "UNIT", "ENGLISH");
         writeFloatStateToFile(outputStateFilePtr, "INITIAL_START_INFLOW", 
                               cArray, indexOfCArray);

         logMessage(DEBUG_LEVEL,"Writing initial state values sarroute");

         indexOfCArray++;

         logMessage(DEBUG_LEVEL,"Writing reach values sarroute");

         writeArrayOfFloatStatesToFile(outputStateFilePtr, 
                                "PHASE_FLOW_VALUE_FOR_REACH",
                                 cArray, indexOfCArray, numOfRoutingPhases);
   }
   else if(numOfInflowTS == 2)
   {
         /* indexOfCArray remains to be 1 as initialized in code above */
 
         logMessage(DEBUG_LEVEL,"Writing reach values sarroute");

         writeStringStateToFile(outputStateFilePtr, "UNIT", "ENGLISH");

         writeFloatStateToFile(outputStateFilePtr, "INITIAL_START_INFLOW", 
                               cArray, indexOfCArray);

         writeArrayOfFloatStatesToFile(outputStateFilePtr, 
                                 "PHASE_FLOW_VALUE_FOR_REACH",
                                 cArray, indexOfCArray, numOfRoutingPhases);
   }
   
   fclose(outputStateFilePtr);
}

