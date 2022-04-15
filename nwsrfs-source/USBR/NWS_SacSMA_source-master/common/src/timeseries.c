/*.............................................................................
* File: timeseries.c
* Author(s): CHPS/FEWS Team
* Date Created: 5/5/08
*
* Development group: OHD HSEB
*
* Purpose: various routines to read write time series dataa
*          for legacy (fortran, c, and c++) models
*                                   
* Module(s): 
*     getOneTimeSeries
*     readAllInputTimeSeries
*     writeOneTimeSeries
*                                
* Date Modified:                             
*
*............................................................................*/

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include <malloc.h>
#include <stdlib.h>
#include <errno.h>

#include "logging.h"
#include "utilities.h"
#include "timeseries.h" 
#include "commonInclude.h"

static TimeSeries *globalTsListPtr = NULL;
static int numberOfInputTimeSeries = 0;
extern int errno;

/*****************************************************************************
 * Module Name: getOneTimeSeries()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *    Gets one time series
 *
 * Calling Arguments:
 *
 *    Name       Input/Output        Type       Description
 *    basinId    In                  char*      basin id
 *    tsType     In                  char*      ts data type
 *    timeStep   In                  int        time step
 *    count      In                  int        count number of time series
 *
 * Return Value: the TimeSeries struct pointer
 *           
 * Local Variables:
 *    Type       Name                           Description
 *     
 * Global Variables Used:
 *    Type       Name                   Origin  Description
 *    static int numberOfInputTimeSeries        Number of input ts
 *
 * Constant and Macro Substitutions:
 *    Name       Header File                    Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

TimeSeries *getOneTimeSeries(char *basinId, char *tsType, int timeStep, 
                             int *count)
{
   /* Put in some checks to check lengths of input pointers */
   TimeSeries *retTs;
   int i;
   
   for (i = 0; i < numberOfInputTimeSeries; i ++)
   {
      if ( getFewsDebugFlag() > 0 )
      {
         logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
               "basinid = %s, globalTsListPtr[i].id = %s, tsType=%s, "
               "globalTsListPtr[i].type = %s, timeStep = %d, "
               "globalTsListPtr[i].timeStep = %d\n", 
               basinId, globalTsListPtr[i].id, tsType, globalTsListPtr[i].type, 
               timeStep, globalTsListPtr[i].timeStep);
      }
     
      if (!strcmp(trim(basinId, 9), globalTsListPtr[i].id) && 
          !strcmp(trim(tsType, 5), globalTsListPtr[i].type) && 
          (timeStep == globalTsListPtr[i].timeStep))
      {
         /* Don't worry, this isn't a local address, it's on the heap and was 
          * previously allocated, just returning it - see no point in making 
          * copies of input data that is read in only one time
          */
         retTs = &(globalTsListPtr[i]);
     
         if ( getFewsDebugFlag() > 0 )
         {   
            logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
	    "calculated count = %d, number of elements in"
	    " input time series  = %d",
            *count, globalTsListPtr[i].count);
         }

         if (*count != globalTsListPtr[i].count)
         {
            logMessageWithArgsAndExitOnError(FATAL_LEVEL,
	    "Number of values in time series %s-%s-%d is incorrect\n", 
	    globalTsListPtr[i].id, globalTsListPtr[i].type, 
	    globalTsListPtr[i].timeStep);
         }
         break;
      }
   }

   if (i == numberOfInputTimeSeries)
   {
      logMessageWithArgsAndExitOnError(FATAL_LEVEL,"Time Series %s %s %d not found\n",basinId,tsType,timeStep  );
   }
   
   return retTs;
   
} /* getOneTimeSeries() --------------------------------------------------- */

/*****************************************************************************
 * Module Name: readAllInputTimeSeries()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *    Reads all input time series.
 *
 * Calling Arguments:
 *
 *    Name     Input/Output    Type       Description
 *
 * Return Value: the TimeSeries struct pointer 
 *           
 * Local Variables:
 *    Type     Name                       Description
 *
 * Global Variables Used:
 *    Type     Name         Origin        Description
 *    int      numberOfInputTimeSeries    Number of time series
 *
 * Constant and Macro Substitutions:
 *    Name           Header File          Description
 *    FATAL_LEVEL    logging.h
 *    FAILURE        logging.h
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

TimeSeries * readAllInputTimeSeries()
{
   TimeSeries *ltsListPtr;
   int fileRetVal;
   FILE *tsFilePtr = fopen(getTsFileName(), "r");
   int eof = 0;

   if (tsFilePtr == NULL) 
   {
      logMessageWithArgsAndExitOnError(FATAL_LEVEL,
      "Cannot open input TS file[%s]. Error[%s]\n", getTsFileName(), 
      strerror(errno));
   }

   if (fscanf(tsFilePtr , "%d", &numberOfInputTimeSeries) == EOF)
   {
      logMessageAndExitOnError(FATAL_LEVEL,
      "Error reading number of time series contained in file\n");
   }

   if (numberOfInputTimeSeries == 0)
   {
      logMessageWithArgsAndExitOnError(FATAL_LEVEL,
      "Missing indications for # of input time-series in TimeSeries file %s\n",
      getTsFileName());
   }

   /* Allocate memory for each time series */
   ltsListPtr = (TimeSeries *)(calloc(numberOfInputTimeSeries,
	                              sizeof(TimeSeries)));

   /* Can log if there is more data in the file than required, indicating 
    * something may be wrong
    */
   int valCount;
   int tsCount;
   //char *a = (char *)malloc(sizeof(char)*10);
   //char *b = (char *)malloc(sizeof(char)*10);
   
   for (tsCount = 0; tsCount < numberOfInputTimeSeries; tsCount++)
   {
      if (fscanf(tsFilePtr, "%s %s %2d %d %s %s", 
         ltsListPtr[tsCount].id, ltsListPtr[tsCount].type, 
         &(ltsListPtr[tsCount].timeStep), &(ltsListPtr[tsCount].count), 
         ltsListPtr[tsCount].dimensions, ltsListPtr[tsCount].units) == EOF)
      {
     
 	 /* Less than max number of time series, keep track */
         logMessageAndExitOnError(DEBUG_LEVEL,"End of file reached\n");
	 break;
      }
	
      if ( getFewsDebugFlag() > 0 )
      {	
         logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
	 "READ basinid = %s, tsType=%s timeStep = %d\n", 
	 ltsListPtr[tsCount].id, ltsListPtr[tsCount].type, 
	 ltsListPtr[tsCount].timeStep);
      }
      
      /* Allocate memory for the values/dates in each time series */
      ltsListPtr[tsCount].value = (float*)(calloc(
  	                                 (ltsListPtr[tsCount].count), sizeof(float)));
 
      /* Allocate memory for the values/dates in each time series */
      ltsListPtr[tsCount].dateTime = (int*)(calloc(
	                                  (ltsListPtr[tsCount].count), sizeof(int)));

      /* Read the two column time series file */
      for (valCount = 0; valCount < ltsListPtr[tsCount].count; valCount++)
      {
         eof = fscanf(tsFilePtr, "%f %10d", 
	              &ltsListPtr[tsCount].value[valCount], 
	              &ltsListPtr[tsCount].dateTime[valCount]);
	  		 
         /* Reached end of file before fully reading expected data */
         if (eof == EOF)
	 {
            logMessageWithArgsAndExitOnError(FATAL_LEVEL,
	    "TimeSeries file %s is incomplete, please check data\n", 
	    getTsFileName());
	 }
      } /* end valCount loop */

   } /* end tsCount loop */
	
   if (tsCount < numberOfInputTimeSeries)
   {
      logMessageWithArgsAndExitOnError(FATAL_LEVEL,
                "Fewer time series contained than needed\n");
   }
   
   globalTsListPtr = ltsListPtr;
   
   if (tsFilePtr != NULL) 
      fclose( tsFilePtr );

   return ltsListPtr;
   
} /* readAllInputTimeSeries() --------------------------------------------- */

/*****************************************************************************
 * Module Name: writeOneTimeSeries()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *    Writes one time series.
 *
 * Calling Arguments:
 *
 *    Name             Input/Output   Type       Description
 *    outputTsFilePtr  In             FILE*      file descriptor
 *    tsId             In             char*      time series id
 *    tsType           In             char*      ts data type
 *    timeStep         In             int        time step
 *    qs               In             float*     contains time series values
 *    count            In             int        count number of time series
 *
 * Return Value: None
 *           
 * Local Variables:
 *    Type             Name                      Description
 *
 * Global Variables Used:
 *    Type             Name          Origin      Description
 *
 * Constant and Macro Substitutions:
 *    Name             Header File               Description
 *    FATAL_LEVEL      logging.h
 *    FAILURE          logging.h
 *    DATESTRINGLENGTH commonInclude.h
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void writeOneTimeSeries(FILE *outputTsFilePtr, char * tsId, char * tsType, 
                        int timeStep, float * qs, int count)
{
   /* Use the global start Date of run + timestep  etc. to figure out the 
    * timeStamp form the date for the first data point
    */
   char dateString[DATESTRINGLENGTH];
   char myUnits[10];
   int  i, j;
   int  fileRetVal;

   int  julianHours = getJulianDayForStartOfRun() * 24 + 
                      getJulianHourForStartOfRun() + timeStep; 
   
   if ( getFewsDebugFlag() > 0 )
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
      "total julianHours = %d, timeStep = %d, julianHourForStartOfRun = %d\n",
      julianHours, timeStep, getJulianHourForStartOfRun());
   }
   
   convertFromJulianHourToMMDDYYYYHHZ(julianHours,dateString);
   
   strcpy(myUnits,"mm");

   fprintf(outputTsFilePtr, "%s %s %2d %s %4d\n",tsId, tsType, timeStep, 
	                                         dateString, count);
   /* Since upgrade from Redhat 4 to 5, some how the i always reset to 0 after call the
      convertFromJulianHourToMMDDYYYYHHZ. I don't know WHY?!
      I changed to j, everthing is OK. By RHC, August, 2009
   */
   i = 0;
   for (j = 0; j < count; j++)
   {
      convertFromJulianHourToMMDDYYYYHHZ(julianHours,dateString);
      fprintf(outputTsFilePtr, "%f %s\n", qs[j], dateString);
      julianHours += timeStep;
   }
   
} /* writeOneTimeSeries() -------------------------------------------------- */



void freeTimeSeries()
{
   if(globalTsListPtr != NULL)
   {
      int tsCount;
      int valCount;
      if ( numberOfInputTimeSeries > 0 )
      {
         for (tsCount = 0; tsCount < numberOfInputTimeSeries; tsCount++)
         {
             if ( globalTsListPtr[tsCount].value != NULL )
	        free(globalTsListPtr[tsCount].value);

	     if ( globalTsListPtr[tsCount].dateTime != NULL )
	        free(globalTsListPtr[tsCount].dateTime);
         }
         free(globalTsListPtr);
      }
      globalTsListPtr = NULL;
   }
}
