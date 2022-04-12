/******************************************************************************
   Filename: chanloss.c

   Description:
   ============
      1. Use the arguments file passed in from fews
      2. Reads time series from ts.txt file
      3. Reads parameters from params.txt 
      4. Checks if the params are changed. If so read the params_saved.txt file
         and call the cox8 routine to get the carry over. Else read the 
         carry over states from statesI.txt file
      5. Executes the CHANLOSS operation - #8
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

#include "chanloss.h"
#include <time.h>

int main(int argc, char **argv)
{

   clock_t start, end;
   double cpu_time_used;
    
   float pCurrent[1000], cCurrent[1]; // carray has only one value for this model
   float pPrevious[1000], cPrevious[1]; // carray has only one value for this model
   int doCox = 0;

   start = clock();
   
   if( readOptions(argc, argv) < 0 )
   {
     exit(0);
   }

   setInformationFromArguments(argv[1]);

   char *diagFileName = getDiagFileName();
   logMessageWithArgsAndExitOnError(DEBUG_LEVEL,  "diagFileName = %s",diagFileName );

   memset(pCurrent,0, sizeof(pCurrent));
   memset(cCurrent,0, sizeof(cCurrent));
   memset(pPrevious,0, sizeof(pPrevious));
   memset(cPrevious,0, sizeof(cPrevious));

   //load the time series first
   readAllInputTimeSeries();

   char *paramsFileName =(char*) calloc(FILE_NAME_LENGTH, sizeof(char));
   strcpy(paramsFileName, getParamFileName());

   // read the params.txt
   pin8_(pCurrent, cCurrent, paramsFileName);
   logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "After pin read from[%s]\n", paramsFileName);

   char prevParamsInfo[FILE_NAME_LENGTH];
   memset(prevParamsInfo, 0, sizeof(FILE_NAME_LENGTH));
   strcpy(prevParamsInfo, getPrevParamsInfo());

   if (strcmp(prevParamsInfo, "PARAMS_UNCHANGED"))
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "Params changed ...");
      doCox = 1;

     // read params_saved.txt
      pin8_(pPrevious, cPrevious, prevParamsInfo);

      logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "After pin read from [%s]\n", prevParamsInfo);

      /* memset cCurrent since the actual states are read from statesI.txt unlike the pin routine filling the
      values from the params file */
      memset(cPrevious, 0, sizeof(cPrevious));

      // read states which actually matches the old params
      readStates(pPrevious, cPrevious);

      logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "After carray read from statesI.txt");

      cox8_(pPrevious, cPrevious, pCurrent, cCurrent);

      logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "Carry over transfer is done");

   }

   if (doCox == 0)
   {

   /* memset cCurrent since the actual states are read from statesI.txt unlike the pin routine filling the
      values from the params file */
   memset(cCurrent, 0, sizeof(cCurrent));
    // read states which actually matches the params.txt
      readStates(pCurrent, cCurrent);
   }

  
   // declaration for variable that hold input time series from input ts file
   float **instDischargeTsData = (float**) malloc(sizeof(float*)); // Instantaneous discharge data mentioned in ts.txt
   *instDischargeTsData = NULL;
   float **potEvapTsData = (float**) malloc(sizeof(float*));  // Potential Evaporation data mentioned in ts.txt
   *potEvapTsData = NULL;

   int driverResolution =  getIntegerFromPArray(pCurrent, 10);

   int potEvapdataFlag = getPotEvapDataFlag(pCurrent);

   logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "PEDATA Flag [%d]", potEvapdataFlag);

   getRequiredInputTimeSeries(pCurrent, potEvapdataFlag, driverResolution, potEvapTsData, instDischargeTsData);
  
 
   // call ex routine thereby execute the model
   ex8_(pCurrent, cCurrent, *instDischargeTsData, *potEvapTsData);
   

   // write output time series
   char *outputTsFileName = getOutputTsFileName();
   FILE *outputTsFilePtr = fopen(outputTsFileName, "w");

   // instantaneous discharge time series is the only output time series
   // the model write the output in instDischargeTsData itself.
   char *instDischargeTimeSeriesId = getTimeSeriesIdFromPArray(pCurrent, 7);
   char *instDischargeTimeSeriesDataTypeCode = getTimeSeriesCodeFromPArray(pCurrent, 9);
   writeOneTimeSeries(outputTsFilePtr, instDischargeTimeSeriesId,
                        instDischargeTimeSeriesDataTypeCode, driverResolution,
                        *instDischargeTsData, outputCount);
   
   /* Free memory and close time series file*/

   if(potEvapTsData != NULL)
      free(potEvapTsData);

   if(instDischargeTsData != NULL)
      free(instDischargeTsData);

   freeTimeSeries();

   fclose(outputTsFilePtr);

   /* Write state values to file*/
   writeStates(pCurrent, cCurrent);

   freeStates();
   

   /* Compute CPU time for model run */
   end = clock();
   cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
   logMessageWithArgsAndExitOnError( DEBUG_LEVEL,"===> TOTAL RUN TIME FORTRAN MODEL: %.2f (seconds)", cpu_time_used );
   
   logMessage(DEBUG_LEVEL,"Exiting chanloss");

   closeDiagFile();

   exit(SUCCESS);

}


/*********************************************************************************
Module :
        getRequiredInputTimeSeries()
Input  :
        float * pArray - pArray populated by the pin routine.
        int potEvapDataFlag - Flag to inform whether PotEvapData is present
        int driverResolution - driver resolution
        float **potEvapData - Potential Evaopration Data
        float **instDischargeTsData  - Instantaneous discharge data

Description :
        Read the pArray info and populates the instantaneous discharge data and
        potential evaopration data 
**********************************************************************************/
void getRequiredInputTimeSeries(float *pArray, int potEvapDataFlag, int driverResolution,
                                float **potEvapData, float **instDischargeTsData)
{
   int  count = getNumberOfElementsInTimeSeries(driverResolution);
   logMessageWithArgsAndExitOnError(DEBUG_LEVEL,  "Output count = %d",count );
   outputCount = count;

   TimeSeries *ts = NULL;

   // get potential Evaporation time series 
   int instDischargeTimeInterval =  driverResolution; 
   char *instDischargeTimeSeriesId = getTimeSeriesIdFromPArray(pArray, 7);
   char *instDischargeTimeSeriesDataTypeCode = getTimeSeriesCodeFromPArray(pArray, 9);
   
   if(instDischargeTimeSeriesId == NULL)
   {
     logMessage(FATAL_LEVEL, "ERROR: Instantaneous Discharge TimeSeries Id is NULL");
   }
   if(instDischargeTimeSeriesDataTypeCode == NULL)
   {
     logMessage(FATAL_LEVEL, "ERROR: Instantaneous Discharge TimeSeries Data Type Code is NULL");
   }

   logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "Instantaneous Discharge TimeSeries Id is [%s]", instDischargeTimeSeriesId);
   logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "Instantaneous Discharge TimeSeries  Data Type Code is [%s]", 
                                                 instDischargeTimeSeriesDataTypeCode);

   // read instantaeous discharge time series data
   ts = getOneTimeSeries(instDischargeTimeSeriesId, instDischargeTimeSeriesDataTypeCode,
                         instDischargeTimeInterval, &count);
   
   if(ts !=NULL)
   {
     *instDischargeTsData = ts->value;
   }

   if(potEvapDataFlag == 1)
   {
      // get potential Evaporation time series 
      int  potEvapDataTimeInterval = 24; // if its not 24 the pin routine will itself terminate saying error
      char *potEvapTimeSeriesId = getTimeSeriesIdFromPArray(pArray, 13);
      char *potEvapTimeSeriesDataTypeCode = getTimeSeriesCodeFromPArray(pArray, 15);

      if(potEvapTimeSeriesId == NULL)
      {
        logMessage(FATAL_LEVEL, "ERROR:  Potential Evaporation TimeSeries Id is NULL");
      }
      if(potEvapTimeSeriesDataTypeCode == NULL)
      {
        logMessage(FATAL_LEVEL, "ERROR:  Potential Evaporation TimeSeries Data Type Code is NULL");
      }

      logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "Potential Evaporation TimeSeries Id is [%s]", potEvapTimeSeriesId);
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "Potential Evaporation TimeSeries  Data Type Code is [%s]", 
                                                   potEvapTimeSeriesDataTypeCode);

      count = getNumberOfElementsInTimeSeries(potEvapDataTimeInterval);
      ts = getOneTimeSeries(potEvapTimeSeriesId, potEvapTimeSeriesDataTypeCode,
                                     potEvapDataTimeInterval, &count);

      if(ts !=NULL)
      {
        *potEvapData = ts->value;
      }
   }
   
}


/**********************************************************
Module: readStates()

Description : reads statesI.txt and filles the cArray

input : float *pArray

output: float *cArray
************************************************************/
void readStates(float* pArray, float *cArray)
{
   int potEvapdataFlag = getPotEvapDataFlag(pArray);

   logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "PEDATA Flag [%d]", potEvapdataFlag);

   if(potEvapdataFlag == 1)
   {
       // read states file & populate  carray
       int numOfStatesKeys, numOfStateUnits;
       readStatesFromFile(&numOfStatesKeys, &numOfStateUnits);

       float returnStateValue = populateFloatStateValue("LAST_PE_VALUE", cArray,1); 

       logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "LAST_PE_VALUE = [%f]", returnStateValue);

   }
}


/**********************************************************
Module: writeStates()

Description : read the cArrayy write statesO.txt

input : float *pArray
        float *cArray
************************************************************/
void writeStates(float* pArray, float *cArray)
{
   int potEvapdataFlag = getPotEvapDataFlag(pArray);

   logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "PEDATA Flag [%d]", potEvapdataFlag);

   // output states should be created as its requuired by fews. 
   // The file can be empty also, but needs to be created and made available for fews.
   FILE *outputStateFilePtr = fopen(getOutputStateFileName(), "w+");

   // write output states only if there is pot evap data given as input and carry over is accomodated
   if(potEvapdataFlag == 1)
   {
      writeStringStateToFile(outputStateFilePtr, "UNIT", "METRIC");
      writeFloatStateToFile(outputStateFilePtr, "LAST_PE_VALUE", cArray, 1);
   }

   fclose(outputStateFilePtr);
}


int getPotEvapDataFlag(float *pArray)
{
   int potEvapDataFlag = 0;

   float waterSurfaceArea = getFloatFromPArray(pArray, 11);
   int ssoutValue = getIntegerFromPArray(pArray, 12);

   if(waterSurfaceArea > 0.0 && ssoutValue == 1)
      potEvapDataFlag = 1;
   logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "here waterSurfaceArea = [%f]", waterSurfaceArea);
   logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "here ssoutValue = [%f]", ssoutValue);

   return potEvapDataFlag;
}

