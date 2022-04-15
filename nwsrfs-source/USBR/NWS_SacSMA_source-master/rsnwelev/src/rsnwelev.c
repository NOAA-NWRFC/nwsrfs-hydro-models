/******************************************************************************
   Filename: rsnwelev.c

   Description:
   ============
      1. Use the arguments file passed in from fews
      2. Reads time series from ts.txt file
      3. Calls initcommonblockspin_ routine and then reads parameters from params.txt
      4. Checks if the params are changed. If so read the params_saved.txt file
         and call the cox42 routine to get the carry over. Else read the
         carry over states from statesI.txt file
      5. Executes the RSNWELEV operation - #42
      6. Saves output carryover states.
      7. Writes output time series.

   Inputs:

   Outputs:

   Change History
   ==============
   DATE          VERSION    PROGRAMMERS        NOTES
   ----------    -------    -----------------  ----------------------
                  1         Varalakshmi Rajaram  Initial Implementation
   07/22/10       2         Varalakshmi/Cham     Fixed memory leak
******************************************************************************/

#include "rsnwelev.h"
#include <time.h>

int main(int argc, char **argv)
{

   clock_t start, end;
   double cpu_time_used;
   
   float parray[1000], carray[1000];

   start = clock();
   
   if( readOptions(argc, argv) < 0 )
   {
      exit(0);
   }

   setInformationFromArguments(argv[1]);

   //load the time series first
   readAllInputTimeSeries();

   // read params.txt
   initcommonblockspin_(getParamFileName());
   pin42_(parray, carray);

   /* memset cCurrent since the actual states are read from statesI.txt unlike 
    * the pin routine filling the values from the params file */
   memset(carray, 0, sizeof(carray));

  // read input time series from input ts file
   float **airTempTsData = (float **)malloc(sizeof(float *));
   *airTempTsData = NULL;
   float **rnswElevTsData = (float **)malloc(sizeof(float *));
   *rnswElevTsData = NULL;

   int driverResolution = getIntegerFromPArray(parray, 2);

   //check if freezing level time series indicator is on.
   int freezingLevelFlag = (int) parray[11];
	
   int nStates, stateUnits;	
   if (freezingLevelFlag != 0)
   {			
       readStatesFromFile(&nStates, &stateUnits);
       populateFloatStateValue("FREEZING_LEVEL_TS", carray,1);			
   }		

   getRequiredInputTimeSeries(parray, airTempTsData, rnswElevTsData);

   //outputCount is figured out in getRequiredInputTimeSeries
   float *outputTsData = (float *)(calloc(outputCount+1, sizeof(float)));
			 
   ex42_(parray,carray,*airTempTsData,outputTsData, *rnswElevTsData);

   // write output time series
   char *outputId = 	getTimeSeriesIdFromPArray(parray, 9);
   char *outputTypeCod = getTimeSeriesCodeFromPArray(parray,11);

   FILE *outputTsFilePtr = fopen(getOutputTsFileName(), "w");

   writeOneTimeSeries(outputTsFilePtr,outputId,outputTypeCod,driverResolution,
	              outputTsData,outputCount);

   FILE *outputStateFilePtr = fopen(getOutputStateFileName(), "w+");
	
   if (freezingLevelFlag != 0)
   {
      writeStringStateToFile(outputStateFilePtr, "UNIT", "METRIC");			
      writeFloatStateToFile(outputStateFilePtr, "FREEZING_LEVEL_TS", 
	                    *rnswElevTsData, outputCount);
   }

   if(airTempTsData != NULL)
      free(airTempTsData);
   if(rnswElevTsData != NULL)
      free(rnswElevTsData);
   if(outputTsData != NULL)
      free(outputTsData);

   freeTimeSeries();
   freeStates();

   fclose(outputStateFilePtr);
   fclose(outputTsFilePtr);

   /* Compute CPU time for model run */
   end = clock();
   cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
   logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
   "===> TOTAL RUN TIME FORTRAN MODEL: %.2f (seconds)", cpu_time_used );
   
   logMessage(DEBUG_LEVEL, "Exiting Rsnwelev");

   closeDiagFile();

   return SUCCESS;
}


/******************************************************************************
Module :
        getRequiredInputTimeSeries()
Input  :
        float * parray - parray populated by the pin routine.
        float **airTempTsData - Air Temperature timeseries
        float **rsnwElevTsData  - Rain Snow Elevation timeseries
        int driverResolution - driver resolution

Description :
        Read the parray info and populates Air Temperature and 
        Rain Snow Elevation timeseries
******************************************************************************/

void getRequiredInputTimeSeries(float *parray, float **airTempTsData, 
                                float **rnswElevTsData)
{
   char *airTempId = NULL;
   char *airTempCd = NULL;
   char *rnswElevId = NULL;
   char *rnswElevCd = NULL;

   int resolution;
   int count;
   TimeSeries *airTempTs = NULL;
   TimeSeries *rnswTs = NULL;

   //This TS is the driver for this model
   airTempId = getTimeSeriesIdFromPArray(parray, 3);
   airTempCd = getTimeSeriesCodeFromPArray(parray, 5);	
   resolution = getIntegerFromPArray(parray, 2);	
		
   outputCount = getNumberOfElementsInTimeSeries(resolution);
   count = outputCount;
	
   airTempTs = getOneTimeSeries(airTempId, airTempCd, resolution, &count);
   if(airTempTs != NULL)
   {
      *airTempTsData = airTempTs->value;
   }

   //check if freezing level time series indicator is on.
   int freezingLevelFlag = (int) parray[11];	

   if (freezingLevelFlag != 0)
   {
      //This TS is the driver for this model
      // Inflow time series identifier
      rnswElevId = getTimeSeriesIdFromPArray(parray, 13);

      // Inflow time series data type
      rnswElevCd = getTimeSeriesCodeFromPArray(parray, 15);
      // inflow time series data interval (units of HR)
      resolution = getIntegerFromPArray(parray, 2);
		
      count = getNumberOfElementsInTimeSeries(resolution);	
		
      rnswTs = getOneTimeSeries(rnswElevId, rnswElevCd, resolution, &count);	
      if(rnswTs != NULL)
      {
        *rnswElevTsData = rnswTs->value;
      }
   }
	
}
