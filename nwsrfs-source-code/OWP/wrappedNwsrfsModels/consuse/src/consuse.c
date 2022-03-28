/******************************************************************************
   Filename: consuse.c

   Description:
   ============
      1. Use the arguments file passed in from fews
      2. Reads time series from ts.txt file
      3. Reads parameters from params.txt
      4. Checks if the params are changed. If so read the params_saved.txt file
         and call the cox57 routine to get the carry over. Else read the
         carry over states from statesI.txt file
      5. Executes the CONSUSE operation - #57
      6. Saves output carryover states.
      7. Writes output time series.

   Inputs:

   Outputs:

   Change History
   ==============
   DATE          VERSION    PROGRAMMERS         NOTES
   ----------    -------    -----------------  ----------------------
                            Sudha Rangan        Initial Implementation
                            Lee Cajina          Made changes to accomodate the 
                                                adapter and other changes
                            Varalakshmi Rajaram Carry Over Implementation and
                                                refactor
   09/09/09                 Cham/Varalakshmi    Close output TS and State files
                                                and refactored code to work on
                                                LX6 (Varalakshmi Rajaram Carry
Over Implementation and
                                                refactor
   09/09/09                 Cham/Varalakshmi    Close output TS and State files
                                                and refactored code to work on
                                                LX6 (Redhat 5). 
   07/22/10                 Varalakshmi/Cham    Fixed memory leak
******************************************************************************/

#include "consuse.h"
#include <time.h>

int main(int argc, char **argv)
{
    clock_t start, end;
    double cpu_time_used;
    
    int driverResolution;   
    int doCox = 0;
    
    /* carray has only one value for this model*/
    float pCurrent[1000], cCurrent[1];
    float pPrevious[1000], cPrevious[1];

    float **sqmeTsData = (float **)malloc(sizeof(float *));
    *sqmeTsData = NULL;
    float **matTsData = (float **)malloc(sizeof(float *));
    *matTsData = NULL;
    float **peTsData = (float **)malloc(sizeof(float *));
    *peTsData = NULL;

    /* 	In the case of the consumptive use model, all outputs are at
	the same resolution as the input SQME, so the outputCount
        will be the same as SQME input, so that was saved earlier.
	However, if this varies based on resolution for output defined in
	the OPT file, then you can calculate the outputcount based on the
	start,end time of the run and the resolution for the given output 
	time series using the getNumberOfElementsInTimeSeries() call
     */

     float rfstor; 
     int numElementsInArray, i;
     int nStates, stateUnits;

     start = clock();
     
     if ( readOptions(argc, argv) < 0 )
     {
        exit(0);
     }

     setInformationFromArguments(argv[1]);

     char *diagFileName = getDiagFileName();
     logMessageWithArgsAndExitOnError(DEBUG_LEVEL, "diagFileName = %s",diagFileName );

     /* Read the time series first */
     readAllInputTimeSeries();

     char paramsFileName[FILE_NAME_LENGTH];
     memset(paramsFileName, 0, sizeof(FILE_NAME_LENGTH));
     strcpy(paramsFileName, getParamFileName());

     /* read the params.txt */
     pin57_(pCurrent, cCurrent, paramsFileName);
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
         pin57_(pPrevious, cPrevious, prevParamsInfo);

         logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
                         "After pin read from [%s]\n", prevParamsInfo);

         /* memset cCurrent since the actual states are read from statesI.txt 
            unlike the pin routine filling the values from the params file */
         memset(cPrevious, 0, sizeof(cPrevious));

         /* read states which actually matches the old params */
	 readStatesFromFile(&nStates, &stateUnits);

	 populateFloatStateValue("RFSTOR", cPrevious,1);

         logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
                         "After carray read from statesI.txt");

         logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
                         "Before Carry over transfer is done CPrev=%f, CCur=%f", 
                          cPrevious[0], cCurrent[0]);

         cox57_(cPrevious, cCurrent);

         logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
                         "Carry over transfer is done CPrev=%f, CCur=%f", 
                          cPrevious[0], cCurrent[0]);
     }
     
     if (doCox == 0)
     {
        /* memset cCurrent since the actual states are read from statesI.txt 
           unlike the pin routine filling the values from the params file */
        memset(cCurrent, 0, sizeof(cCurrent));

	readStatesFromFile(&nStates, &stateUnits);

	populateFloatStateValue("RFSTOR", cCurrent,1);
     }

     /* while reading the input time series, we want to set the julian day and 
        hour for the start of the run (which is driverInterval before the 
        driverTs time (i.e. one timestep behind the start of the data),
	so we need to adjust it , in the case of Consumptive use, 
        driver timestep = 24 hours, this number can be assumed here based 
        on the documentation */
     driverResolution = NUMBEROFHOURSINDAY;

     getRequiredInputTimeSeries(pCurrent, sqmeTsData, matTsData, peTsData);

     /*outputCount is figured out in getRequiredInputTimeSeries */
     float *qAdjTsData = (float *)(calloc(outputCount+1, sizeof(float)));
     float *qDivTs = (float *)(calloc(outputCount+1, sizeof(float)));
     float *qRfinTs =  (float *)(calloc(outputCount+1, sizeof(float)));
     float *qRfoutTs = (float *)(calloc(outputCount+1, sizeof(float)));
     float *qolTs = (float *)(calloc(outputCount+1, sizeof(float)));
     float *qcdTs = (float *)(calloc(outputCount+1, sizeof(float)));
     float *ceTs = (float *)(calloc(outputCount+1, sizeof(float)));

     ex57_(pCurrent,cCurrent,*matTsData,*peTsData,*sqmeTsData,qAdjTsData,qDivTs,
                              qRfinTs, qRfoutTs,qolTs, qcdTs, ceTs);

     /* The driverResolution is required because different outputs may be at 
        different resolutions for a given model. For Consumptive Use they are 
        all at the same resolution.  Since the time written out for the first 
        data point depends on this resolution (it is relative to the
        actual start run time of the model (different from the first data 
        point of the driving TS - it is driving ts resolution hours less 
        than the first time step of the driving ts. */

      /* Write outputs.  Get the id, timestep and type for each output ts 
         and then write it. The index sent in is the FORTRAN index 
         (1 will be subtracted in the getTimeSeriesIdFromPArray*/
      char *qAdjId = 	getTimeSeriesIdFromPArray(pCurrent, 29);
      char *qAdjCd = getTimeSeriesCodeFromPArray(pCurrent,31);

      /* open the output file here rather than in write, 
         since we're going to write out individually
	 and need to keep appending to the same file */	
	
      FILE *outputTsFilePtr = fopen(getOutputTsFileName(), "w");

      if (strcmp(qAdjId,"NONE"))
      {
	 writeOneTimeSeries(outputTsFilePtr,qAdjId,qAdjCd,
                 driverResolution,qAdjTsData,outputCount);
      }

      char *qDivId = 	getTimeSeriesIdFromPArray(pCurrent, 32);
      char *qDivCd = 	getTimeSeriesCodeFromPArray(pCurrent,34);
      if (strcmp(qDivId,"NONE"))
      {
	 writeOneTimeSeries(outputTsFilePtr,qDivId,qDivCd,
                 driverResolution,qDivTs,outputCount);
      }

      char *qRfinId = getTimeSeriesIdFromPArray(pCurrent, 35); 	
      char *qRfinCd = getTimeSeriesCodeFromPArray(pCurrent,37);
      if (strcmp(qRfinId,"NONE"))
      {
         writeOneTimeSeries(outputTsFilePtr,qRfinId,qRfinCd,
                 driverResolution,qRfinTs,outputCount);
      }

      char *qRfoutId = getTimeSeriesIdFromPArray(pCurrent, 38);
      char *qRfoutCd = getTimeSeriesCodeFromPArray(pCurrent,40);
      if (strcmp(qRfoutId,"NONE"))
      {
         writeOneTimeSeries(outputTsFilePtr,qRfoutId,qRfoutCd,
                 driverResolution,qRfoutTs,outputCount);
      }

      char *qolId = 	getTimeSeriesIdFromPArray(pCurrent, 41);
      char *qolCd = 	getTimeSeriesCodeFromPArray(pCurrent,43);
      if (strcmp(qolId,"NONE"))
      {
         writeOneTimeSeries(outputTsFilePtr,qolId, qolCd,
                 driverResolution,qolTs,outputCount);
      }

      char *qcdId = 	getTimeSeriesIdFromPArray(pCurrent, 44);	
      char *qcdCd =	getTimeSeriesCodeFromPArray(pCurrent,46); 	
      if (strcmp(qcdId,"NONE"))
      {
         writeOneTimeSeries(outputTsFilePtr,qcdId, qcdCd,
                 driverResolution,qcdTs,outputCount);
      }

      char *ceId = 	getTimeSeriesIdFromPArray(pCurrent, 47);
      char *ceCd = 	getTimeSeriesCodeFromPArray(pCurrent,49);
      if (strcmp(ceId,"NONE"))
      {
	 writeOneTimeSeries(outputTsFilePtr,ceId, ceCd,
                 driverResolution,ceTs,outputCount);
      }
        
      /* Close output ts file */
      if ( outputTsFilePtr != NULL )
	 fclose( outputTsFilePtr );

      /* Open output state file */
      FILE *outputStateFilePtr = fopen(getOutputStateFileName(), "w+");

      writeStringStateToFile(outputStateFilePtr,"UNIT","METRIC");
      writeFloatStateToFile(outputStateFilePtr,"RFSTOR",cCurrent,1);

      /* Close output state file */
      if ( outputStateFilePtr != NULL )
         fclose ( outputStateFilePtr );

      if(qAdjTsData != NULL)
	 free(qAdjTsData);
      if(qDivTs != NULL)
	 free(qDivTs);
      if(qRfinTs != NULL)
	 free(qRfinTs);
      if(qRfoutTs  != NULL)
	 free(qRfoutTs);
      if(qolTs != NULL)
	 free(qolTs);
      if(qcdTs != NULL)
	 free(qcdTs);
      if(ceTs != NULL)
	 free(ceTs);

       if(sqmeTsData != NULL)
	 free(sqmeTsData);
       if(matTsData != NULL)
	 free(matTsData);
       if(peTsData != NULL)
	 free(peTsData);

      freeTimeSeries();
      freeStates();

      /* Compute CPU time for model run */
      end = clock();
      cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;

      logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
      "===> TOTAL RUN TIME FORTRAN MODEL: %.2f (seconds)", cpu_time_used );
      
      logMessage(DEBUG_LEVEL,"Exiting Consuse");

      closeDiagFile();

      return SUCCESS;
}

/*****************************************************************************
Module :
        getRequiredInputTimeSeries()
Input  :
        float * pArray - pArray populated by the pin routine.
        float **sqmeTsData - Runoff timeseries
        float **matTsData  - Mean Areal Temp timeseries
        float **peTsData  - Potential Evaporation timeseries
        int driverResolution - driver resolution

Description :
        Read the pArray info and populates Runoff, Mean Areal Temp,
        and Potential Evaporation timeseries

Change History:
Date      Version   Programmers         Notes
-----     -------   -----------         -----
                    Sudha Rangan        Initial implementation
                    Varalakshmi Rajaram Code cleanup
09/09/09            Cham/Varalakshmi    removed unused variable "char *message"
                                        and refactored code to work on LX6
                                        (Redhat 5).
******************************************************************************/

void getRequiredInputTimeSeries(float *pArray, float **sqmeTsData, 
                                float **matTsData, float** peTsData)
{
      int resolution;
      int count;
      TimeSeries *matTs = NULL;
      TimeSeries *sqmeTs = NULL;
      TimeSeries *peTs = NULL;

      /*This TS is the driver for this model */
      char *sqmeCd = getTimeSeriesCodeFromPArray(pArray, 28);
      char *sqmeId = getTimeSeriesIdFromPArray(pArray, 26);

      /*if this is stored in pArray, then get it
        in the case of this model, it is assumed, since
        there can be only one resolution */
      resolution = 24;
      
      /*this is the end and start hour of the driver time series */
      int tempStartDay, tempStartHour, tempEndDay, tempEndHour;
      
      count = getNumberOfElementsInTimeSeries(resolution);
      outputCount = count;

      sqmeTs = getOneTimeSeries(sqmeId, sqmeCd, resolution, &count);
      *sqmeTsData = sqmeTs->value;
      
      char *matCd = getTimeSeriesCodeFromPArray(pArray, 22);
      
      if (matCd == NULL)
      {
	 logMessageAndExitOnError(FATAL_LEVEL,"Error reading pArray for matCd");
      }
      
      /*get matcd */
      if (strcmp(matCd,"NONE"))
      {
	 char *matId = getTimeSeriesIdFromPArray(pArray,20);

	 if(matId == NULL)
	 {
	    logMessageAndExitOnError(FATAL_LEVEL,
		                     "Error reading pArray for matId ");
	 }
      
  	 /*if this is stored in pArray, then get it with getIntegerFromPArray
	   In the case of the Consumptive Use Model, it is assumed, since
           there can be only one resolution */
	 resolution = 6;

	 count = getNumberOfElementsInTimeSeries(resolution);

	 matTs = getOneTimeSeries(matId, matCd, resolution, &count);

         if (matTs != NULL)
         {
	    *matTsData = matTs->value;
         }
      }
      else
      {
  	char *peCd = getTimeSeriesCodeFromPArray(pArray, 25);

	if (peCd == NULL)
	{
	  logMessageAndExitOnError(FATAL_LEVEL,"Error reading pArray for peCd");
	}

	if (strcmp(peCd,"NONE"))
	{
	   char *peId = getTimeSeriesIdFromPArray(pArray, 23);

	   if (peId == NULL)
	   {
	      logMessageAndExitOnError(FATAL_LEVEL,
                                       "Error reading pArray for peId");
	   }
	   /* If this is stored in pArray, then get it with getIntegerFromPArray
	      In the case of the Consumptive Use Model, it is assumed, since
              there can be only one resolution */
	   resolution = 24;
	
	   /* this is the end and start hour of the driver time series */
	   count = getNumberOfElementsInTimeSeries(resolution);
	   peTs = getOneTimeSeries(peId, peCd, resolution, &count);

           if (peTs != NULL)
           {
	      *peTsData = peTs->value;
           }
	}
      }
}
