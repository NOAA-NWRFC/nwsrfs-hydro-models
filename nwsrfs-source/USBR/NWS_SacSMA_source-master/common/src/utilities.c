/*............................................................................
* File: utilities.c
* Author(s): CHPS/FEWS Team
* Date Created: 5/5/08

* Development group: OHD HSEB
*
* Purpose: various utility routines for implementing legacy (fortran, c, 
*          and c++) code in FEWS                         
* Module(s): 
*     replaceNewLineCharWithNullChar
*     readModelArgs
*     logModelArgs
*     getArgsFileName
*     setArgsFileName
*     setInformationFromArguments
*     trim
*     padWithSpaces
*     getTimeSeriesIdFromPArray
*     getIntegerFromPArray
*     getNumberOfElementsInTimeSeries  
*     numberoftimesteps_ 
*     calculateJulianDay 
*     calculatejulday_ 
*     readNwsrfsDataTypeMappingFile
*     getDimensionAndUnitAndTimeCode 
*     getdimensionandunitinfortran_
*     getParamFileName 
*     getTsFileName 
*     getOutputTsFileName 
*     getStateFileName 
*     getOutputStateFileName 
*     getJulianDayForStartOfDriverTSData 
*     getJulianHourForStartOfDriverTSData
*     getJulianDayForStartOfRun 
*     getJulianHourForStartOfRun 
*     getJulianDayForEndOfRun 
*     getJulianHourForEndOfRun
*     getFewsDebugFlag 
*     setFewsDebugFlag
*     setDataTypeFileName
*     getDataTypeFileName
*     setDataUnitFileName
*     getDataUnitFileName
*     readOptions
*     printUsage 
*     convertFromJulianHourToMMDDYYYYHHZ
*     getversioninfofromfortran_
*     readArgs
*                                
* Date Modified:                             
*     07/1/2010
*............................................................................*/

#include "utilities.h"

static char datatypeFileName[FILE_NAME_LENGTH];
static char dataunitFileName[FILE_NAME_LENGTH];
static char modunitFileName[FILE_NAME_LENGTH];
static char argumentsFileName[FILE_NAME_LENGTH];
static struct ModelArgs modelArgs;

static NwsrfsDataTypeMapping nwsrfsDataTypeMapping[NWSRFS_MAPPING_FILE_SIZE];
static int nwsrfsDataTypeMappingCount = 0;
static int fewsDebugFlag = 0;
static int StartOfDay = 12;  /* The default is NWSRFS forcasting mode, 12Z. */

/*****************************************************************************
 * Module Name: replaceNewLineCharWithNullChar()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 09/30/08
 *
 * Description:
 *    Replaces the last char in the string to \0
 *
 * Calling Arguments:
 *
 *    Name       Input/Output    Type      Description
 *    args       In              char*     string 
 *
 * Return Value: None
 *
 * Local Variables:
 *    Type       Name                      Description
 *
 * Global Variables Used:
 *    Type       Name            Origin    Description
 *
 * Constant and Macro Substitutions:
 *    Name           Header File           Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void replaceNewLineCharWithNullChar(char* str)
{
  if(str != NULL)
  {
    str[strlen(str)-1] = '\0';
  }
}
/****************************************************************************
   Module Name: getModelArgs()
   Description: return ModelArgs 
*****************************************************************************/
struct ModelArgs getModelArgs()
{
  return modelArgs;
}

/****************************************************************************
   Module Name: freeProperties()
   Description: free memories for the Properties structure
*****************************************************************************/
void freeProperties(Properties *properties, int numOfProperties)
{
    int count = 0;
    if(properties != NULL)
    {
      for(count = 0; count < numOfProperties; count++)
      {
            if(properties[count].name != NULL)
	         free(properties[count].name);
            if(properties[count].value != NULL)
	         free(properties[count].value);
      }
      free(properties);
    }

}

/****************************************************************************
   Module Name: readArgs()
   Description: Retreive the data from arguments.txt file,store them in
   the Properties structure.
*****************************************************************************/
Properties* readArgs(int* numOfProperties)
{

    int dummy;
    Properties* properties = NULL;    
  
     /* malloc properties */
     properties = (Properties *) malloc(sizeof(Properties)*MAX_PROPERTIES);

     readPropertiesFromFile(numOfProperties,
                            getArgsFileName(), properties,
                            &dummy);
			    
     return properties;			    

}
/*****************************************************************************
 * Module Name: readModelArgs()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 09/30/08
 *
 * Description:
 *    Opens the argument file and reads each line and sets the model arguments
 *
 * Calling Arguments:
 *
 *    Name       Input/Output    Type      Description
 *
 * Return Value: None
 *
 * Local Variables:
 *    Type       Name                      Description
 *
 * Global Variables Used:
 *    Type       Name            Origin    Description
 *
 * Constant and Macro Substitutions:
 *    Name           Header File           Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void readModelArgs()
{
     int dummy, numOfProperties, i;
  
     Properties* properties = (Properties *) 
                          malloc(sizeof(Properties)*MAX_PROPERTIES);

     readPropertiesFromFile(&numOfProperties,
                            getArgsFileName(), properties,
                            &dummy);

     for (i = 0; i < numOfProperties; i++)
     {
         if (!strcmp(properties[i].name, TS_FILE_NAME))
         {
       	    strcpy(modelArgs.tsFile, properties[i].value);
         }
         else if (!strcmp(properties[i].name, PARAM_FILE_NAME))
         {
             strcpy(modelArgs.paramsFile, properties[i].value);
         }
         else if (!strcmp(properties[i].name, STATE_FILE_NAME))
         {
       	     strcpy(modelArgs.inputStatesFile, properties[i].value);
         }
         else if (!strcmp(properties[i].name, OUTPUT_TS_FILE_NAME))
         {
       	     strcpy(modelArgs.outputTsFile, properties[i].value);
         }
         else if (!strcmp(properties[i].name, OUTPUT_STATE_FILE_NAME))
         {
             strcpy(modelArgs.outputStatesFile, properties[i].value);
         }
         else if (!strcmp(properties[i].name, DIAG_FILE_NAME))
         {
             strcpy(modelArgs.diagFile,properties[i].value);
         }
         else if (!strcmp(properties[i].name, DEBUG_FLAG))
         {
             modelArgs.printDebugFlag = atoi(properties[i].value);
         }
         else if (!strcmp(properties[i].name, JULIAN_DAY_FOR_START_OF_RUN))
         {
             modelArgs.julianDayForStartOfRun = atoi(properties[i].value);
         }
         else if (!strcmp(properties[i].name, JULIAN_HOUR_FOR_START_OF_RUN))
         {
             modelArgs.julianHourForStartOfRun = atoi(properties[i].value);
         }
         else if (!strcmp(properties[i].name, JULIAN_DAY_FOR_START_OF_DRIVER_TS_DATA))
         {
             modelArgs.julianDayForStartOfDriverTSData = atoi(properties[i].value);
         }
         else if (!strcmp(properties[i].name, JULIAN_HOUR_FOR_START_OF_DRIVER_TS_DATA))
         {
             modelArgs.julianHourForStartOfDriverTSData = atoi(properties[i].value);
         }
         else if (!strcmp(properties[i].name, JULIAN_DAY_FOR_END_OF_RUN))
         {
       	     modelArgs.julianDayForEndOfRun = atoi(properties[i].value);
         }
         else if (!strcmp(properties[i].name, JULIAN_HOUR_FOR_END_OF_RUN))
         {
             modelArgs.julianHourForEndOfRun = atoi(properties[i].value);
         }
         else if (!strcmp(properties[i].name, JULIAN_DAY_FOR_LAST_OBS_DATE))
         {
             modelArgs.julianDayForLastObsDate = atoi(properties[i].value);
         }
         else if (!strcmp(properties[i].name, JULIAN_HOUR_FOR_LAST_OBS_DATE))
         {
             modelArgs.julianHourForLastObsDate = atoi(properties[i].value);
         }
         else if (!strcmp(properties[i].name, PREV_PARAMS_INFO))
         {
             strcpy(modelArgs.prevParamsInfo, properties[i].value);
         }
         else if (!strcmp(properties[i].name, LOCAL_HOUR_OFFSET))
         {
             modelArgs.localHourOffset = atoi(properties[i].value);
         }
         else if (!strcmp(properties[i].name, DATA_TYPE_FILE_NAME))
         {
             strcpy(modelArgs.datatypeFile, properties[i].value);
         }
         else if (!strcmp(properties[i].name, MOD_FILE_NAME))
         {
             strcpy(modelArgs.modFile, properties[i].value);
         }
         else if (!strcmp(properties[i].name, DATA_UNIT_FILE_NAME))
         {
             strcpy(modelArgs.dataUnitFile, properties[i].value);
         }
         else if (!strcmp(properties[i].name, BASEFLOW_MODEL_TIME_INTERVAL))
         {
             modelArgs.baseflowModelTimeInterval = atoi(properties[i].value);
         }
         else if (!strcmp(properties[i].name, START_OF_DAY))
	 {
	     modelArgs.startOfDay = atoi(properties[i].value);
	 }
         else if (!strcmp(properties[i].name, FFG_DURATION_HR))
         {
             modelArgs.FFGdurationHR = atoi(properties[i].value);
         }	 
         else if (!strcmp(properties[i].name, RESSNGL_TECHNIQUE))
         {
             modelArgs.ressnglTechnique = atoi(properties[i].value);
         }
	 else if (!strcmp(properties[i].name, MAPE_TIMESTEP))
         {
             modelArgs.mapeTimeStep = atoi(properties[i].value);
         }
	 
       }

      freeProperties(properties, numOfProperties);

}



/*****************************************************************************
 * Module Name: printModelArgs()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 10/14/08
 *
 * Description:
 *    logs the model arguments
 *
 * Calling Arguments:
 *
 *    Name       Input/Output    Type      Description
 *
 * Return Value: None
 *
 * Local Variables:
 *    Type       Name                      Description
 *
 * Global Variables Used:
 *    Type       Name            Origin    Description
 *
 * Constant and Macro Substitutions:
 *    Name           Header File           Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
void logModelArgs()
{
   if ( getFewsDebugFlag() > 0 )
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL, \
      "standard arguments are:\n" 
      "TSFILENAME = %s\nPARAMFILENAME = %s\nSTATEFILENAME = %s\n"
      "OUTPUTTSFILENAME = %s\nOUTPUTSTATEFILENAME = %s\n"
      "DIAGFILENAME = %s\nDEBUGFLAG = %d\n"
      "JULIANDAYFORSTARTOFRUN = %d\nJULIANHOURFORSTARTOFRUN = %d\n"
      "JULIANDAYFORSTARTOFDRIVERTSDATA = %d\n"
      "JULIANHOURFORSTARTOFDRIVERTSDATA = %d\n"
      "JULIANDAYFORENDOFRUN = %d\n"
      "JULIANHOURFORENDOFRUN = %d\n"
      "JULIANDAYFORLASTOBSDATE = %d\n"
      "JULIANHOURFORLASTOBSDATE = %d\n"
      "PARAMS_SAVED_INFO = %s\n"
      "LOCAL_HOUR_OFFSET = %d\n"
      "START_OF_DAY = %d\n"
      "DATA_TYPE_FILENAME = %s\n",
       modelArgs.tsFile, modelArgs.paramsFile, 
       modelArgs.inputStatesFile, modelArgs.outputTsFile, 
       modelArgs.outputStatesFile, modelArgs.diagFile, 
       modelArgs.printDebugFlag, 
       modelArgs.julianDayForStartOfRun, 
       modelArgs.julianHourForStartOfRun, 
       modelArgs.julianDayForStartOfDriverTSData, 
       modelArgs.julianHourForStartOfDriverTSData, 
       modelArgs.julianDayForEndOfRun, 
       modelArgs.julianHourForEndOfRun, 
       modelArgs.julianDayForLastObsDate, 
       modelArgs.julianHourForLastObsDate, 
       modelArgs.prevParamsInfo, 
       modelArgs.localHourOffset, 
       modelArgs.startOfDay, 
       modelArgs.datatypeFile);
   }

}

/*****************************************************************************
 * Module Name: getArgsFileName()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 09/30/08
 *
 * Description:
 *   Returns the argument file name.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output    Type      Description
 *
 * Return Value: None
 *
 * Local Variables:
 *    Type       Name                      Description
 *
 * Global Variables Used:
 *    Type       Name            Origin    Description
 *
 * Constant and Macro Substitutions:
 *    Name           Header File           Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

char* getArgsFileName()
{
   return argumentsFileName;
}
/*****************************************************************************
 * Module Name: setArgsFileName()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 09/30/08
 *
 * Description:
 *    Sets the argument file name.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output    Type      Description
 *    fileName   In              char*     arguments file name for the model
 *
 * Return Value: None
 *
 * Local Variables:
 *    Type       Name                      Description
 *
 * Global Variables Used:
 *    Type       Name            Origin    Description
 *
 * Constant and Macro Substitutions:
 *    Name           Header File           Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void setArgsFileName(char *fileName)
{
   strcpy(argumentsFileName, fileName);
   argumentsFileName[strlen(argumentsFileName)] = '\0';
}

/*****************************************************************************
 * Module Name: setInformationFromArguments()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 09/30/08
 *
 * Description:
 *    Sets information from arguments.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output    Type      Description
 *    args       In              char**    argument file name passed
 *
 * Return Value: None
 *           
 * Local Variables:
 *    Type       Name                      Description
 *     
 * Global Variables Used:
 *    Type       Name            Origin    Description
 *
 * Constant and Macro Substitutions:
 *    Name           Header File           Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
void setInformationFromArguments(char *argsFileName)
{
   int local_debugflag;
   int localJulianDayForStartOfData, 
       localJulianHourForStartOfData,
       localJulianDayForEndOfRun,
       localJulianHourForEndOfRun;
   int lastObsJulianDay,
       lastObsJulianHour,
       localHourOffset;
       
   setArgsFileName(argsFileName);
   readModelArgs();

   /* set diag file name and debug flag # */
   setDiagFileName(modelArgs.diagFile);
   setFewsDebugFlag(modelArgs.printDebugFlag);
   setDataTypeFileName(modelArgs.datatypeFile);
   setDataUnitFileName(modelArgs.dataUnitFile);
   setModFileName(modelArgs.modFile);
   
   /* log version information */
   logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
   "Using OHD Model Version: %s%s\n", VERSION_NAME, VERSION_NUMBER);
   
   local_debugflag = getFewsDebugFlag();   /* used by *.c files */

   /* Print out model arguments to diagnostic file */
   logModelArgs();                       
    
   localJulianDayForStartOfData = getJulianDayForStartOfDriverTSData();
   localJulianHourForStartOfData = getJulianHourForStartOfDriverTSData();
   localJulianDayForEndOfRun = getJulianDayForEndOfRun();
   localJulianHourForEndOfRun = getJulianHourForEndOfRun();   
   lastObsJulianDay = modelArgs.julianDayForLastObsDate;
   lastObsJulianHour = modelArgs.julianHourForLastObsDate;
   localHourOffset = modelArgs.localHourOffset; 
   StartOfDay = modelArgs.startOfDay;
   
   initializefortrancommonblocks_( &localJulianDayForStartOfData,
                                   &localJulianHourForStartOfData,
                                   &localJulianDayForEndOfRun,
                                   &localJulianHourForEndOfRun,	  
                                   &lastObsJulianDay,
                                   &lastObsJulianHour,
                                   &local_debugflag,
				   &localHourOffset,
	                           &StartOfDay );
				   
   /* Read data type mapping file */				   
   readDataTypeMappingFile();
  
}

/*****************************************************************************
 * Module Name: trim()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    This function will set empty space to carry return character.
 *
 * Calling Arguments:
 *
 *    Name         Input/Output      Type       Description
 *    inputString  In/Out            char*      contains string
 *    maxLen       In                int        maximum length
 *
 * Return Value: the input string 
 *           
 * Local Variables:
 *    Type         Name                         Description
 *     
 * Global Variables Used:
 *    Type         Name              Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name         Header File                  Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

char *trim(char *inputString, int maxLen)
{
   int i;
   for ( i = 0; i < maxLen; i++ )
   {
      if ( inputString[i] == ' ' )
      {
         inputString[i] = '\0';
      }
   }
   return inputString;
}

/*****************************************************************************
 * Module Name: padWithSpaces()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Pads input string with spaces.
 *
 * Calling Arguments:
 *
 *    Name          Input/Output  Type       Description
 *    inputString   In/Out        char*      contains string
 *    maxLen        In            int        maximum length
 *
 * Return Value: the input string 
 *           
 * Local Variables:
 *    Type          Name                     Description
 *     
 * Global Variables Used:
 *    Type          Name          Origin     Description
 *
 * Constant and Macro Substitutions:
 *    Name          Header File              Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
void padWithSpaces(char *inputString, int maxLen)
{   
   int i;
   if ( inputString != NULL )
   {
      for ( i = strlen(inputString); i < maxLen; i++ )
      {
         inputString[i] = ' ';
      }
   }
}

/*****************************************************************************
 * Module Name: getTimeSeriesIdFromPArray()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Gets time series id.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output    Type       Description
 *    pArray     In              float*     parameter array
 *    startIndex In              int        start index
 *
 * Return Value: the TS id string
 *           
 * Local Variables:
 *    Type       Name                       Description
 *     
 * Global Variables Used:
 *    Type       Name            Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name       Header File                Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

char* getTimeSeriesIdFromPArray(float *pArray, int startIndex)
{
   /* Ideally we would declare this type as a char pointer
    * and return it and its the user's responsibility to
    * free the memory. Since there are many model's using this 
    * function and we are likely to forget to free this memory
    * we chose to declare this type variable as static local variable
    * so we can return its address to the caller.
    * Note: the memset ensures that the type variable is set
    * to empty string each time the function is revisited. Please
    * dont remove it.
    */
   static char id[9];
   
   memset(id, '\0', sizeof(id));

   strncpy(id,(char *)(&(pArray[startIndex-1])),8);
    
   id[8]='\0';

   return id;

} /* getTimeSeriesIdFromPArray() ------------------------------------------- */
/*****************************************************************************
 * Module Name: getTimeSeriesCodeFromPArray()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Gets data type from parameter array.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output    Type       Description
 *    pArray     In              float*     parameter array
 *    startIndex In              int        start index
 *
 * Return Value: the data type string
 *           
 * Local Variables:
 *    Type       Name                       Description
 *     
 * Global Variables Used:
 *    Type       Name            Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name       Header File                Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

char* getTimeSeriesCodeFromPArray(float *pArray, int startIndex)
{
   /* Ideally we would declare this type as a char pointer
    * and return it and its the user's responsibility to
    * free the memory. Since there are many model's using this 
    * function and we are likely to forget to free this memory
    * we chose to declare this type variable as static local variable
    * so we can return its address to the caller.
    * Note: the memset ensures that the type variable is set
    * to empty string each time the function is revisited. Please
    * dont remove it.
    */

   static char type[5];
   
   memset(type, '\0', sizeof(type));
   
   strncpy(type,(char *)(&(pArray[startIndex-1])),4);
   
   type[4]='\0';

   return type;

} /* getTimeSeriesCodeFromPArray() ---------------------------------------- */

/*****************************************************************************
 * Module Name: getIntegerFromPArray()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Gets resolution from parameter array.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output    Type       Description
 *    pArray     In              float*     parameter array
 *    startIndex In              int        start index
 *
 * Return Value: the resolution value
 *           
 * Local Variables:
 *    Type       Name                       Description
 *     
 * Global Variables Used:
 *    Type       Name            Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name       Header File           Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

int getIntegerFromPArray(float *pArray, int startIndex)
{
   
   int resolutionString; 
   
   resolutionString = (int) (pArray[startIndex-1]);
   
   return resolutionString;

} /* getIntegerFromPArray() ------------------------------------------------ */

float getFloatFromPArray(float *pArray, int startIndex)
{
   
   float resolutionString; 
   
   resolutionString = (float) (pArray[startIndex-1]);
   
   return resolutionString;

} /* getIntegerFromPArray() ------------------------------------------------ */
/*****************************************************************************
 * Module Name: getNumberOfElementsInTimeSeries()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Gets number of elements in time series
 *
 * Calling Arguments:
 *
 *    Name        Input/Output     Type       Description
 *    resolution  In               int        resolution
 *
 * Return Value: the number of time series
 *           
 * Local Variables:
 *    Type        Name                        Description
 *     
 * Global Variables Used:
 *    Type        Name             Origin     Description
 *
 * Constant and Macro Substitutions:
 *    Name                Header File         Description
 *    NUMBEROFHOURSINDAY  utilities.h         indicates 24
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

int getNumberOfElementsInTimeSeries(int resolution)
{
   int  count = 0;
   int  totalJulianHours = 0;
   int  julianHours = 0;
   char dateString[DATESTRINGLENGTH];

   julianHours = getJulianDayForEndOfRun()*24 + getJulianHourForEndOfRun() +
                 resolution;

   memset(dateString,'\0', DATESTRINGLENGTH);
   convertFromJulianHourToMMDDYYYYHHZ(julianHours,dateString);

   if ( getFewsDebugFlag() > 0 )
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
      "endJulianDayOfRun = %d, endDayOfRun= %s\n",
      getJulianDayForEndOfRun(),  dateString);
   }

   julianHours = getJulianDayForStartOfDriverTSData()*24 +
                 getJulianHourForStartOfDriverTSData() + resolution;

   convertFromJulianHourToMMDDYYYYHHZ(julianHours,dateString);

   if ( getFewsDebugFlag() > 0 )
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
      "julianDayForStartOfDriverTSData = %d,"
      " DayForStartOfDriverTSData(YYYYMMDDHH)= %s\n",
      getJulianDayForStartOfDriverTSData(), dateString);
   }

   totalJulianHours = ((getJulianDayForEndOfRun()-getJulianDayForStartOfRun())+
	                1)*NUMBEROFHOURSINDAY;

   if ( getFewsDebugFlag() > 0 )
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
      "count = %d\n", totalJulianHours );
   }
   
   totalJulianHours -= (resolution);
   totalJulianHours -= (getJulianHourForStartOfRun());

   if ( getFewsDebugFlag() > 0 )
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
      "totalJulianHours =  %d, endJulianHourOfRun = %d\n",
      totalJulianHours, getJulianHourForEndOfRun());
   }

   totalJulianHours -= (24-getJulianHourForEndOfRun());
   
   if ( getFewsDebugFlag() > 0 )
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL,"totalJulianHours =  %d\n",
      totalJulianHours);
   }
   
   count = totalJulianHours/resolution;

   if ( getFewsDebugFlag() > 0 )
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
      "count of data points = %d\n", count + 1);
   }

   return count+1;

} /* logMessageWithArgsAndExitOnError() ------------------------------------ */

/*****************************************************************************
 * Module Name: numberoftimesteps()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Used to return number of time steps using driving resolution.  It reused
 *    getNumberOfElementsInTimeSeries().
 *
 * Calling Arguments:
 *
 *    Name              Input/Output   Type       Description
 *    driverResolution  In             int*       driver resolution
 *    numberOfTimeSteps In             int*       number of time step
 *
 * Return Value: None
 *           
 * Local Variables:
 *    Type              Name                      Description
 *     
 * Global Variables Used:
 *    Type              Name           Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name              Header File               Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void numberoftimesteps_(int *driverResolution, int *numberOfTimeSteps) 
{
   *numberOfTimeSteps = getNumberOfElementsInTimeSeries(*driverResolution);

} /* numberoftimesteps_() -------------------------------------------------- */

/*****************************************************************************
 * Module Name: calculateJulianDay()
 *
 * Original Author: CHPS/FEWS Team 
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Calculates and displays julian dates and difference.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output     Type       Description
 *    month1     In               int        month
 *    day1       In               int        day
 *    year1      In               int        year
 *
 * Return Value: the julian day
 *           
 * Local Variables:
 *    Type       Name                        Description
 *     
 * Global Variables Used:
 *    Type       Name             Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name       Header File                 Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

int calculateJulianDay(int month1, int day1, int year1)
{

   float res1, res2, res3, res4, res5, res6;
   long  jdn1, jdn2;
   long  a, y, m;
   float julianDifference;
   float month2, day2, year2;
        
   /* Since we're using some kind of modified julian day (from 1900), first 
    * calculate the julian day representation of 1900 and subtract it from 
    * the time of day to us
    */
   month2 = 1.;
   day2 = 1.;
   year2 = 1900.;

   a = (14 - month1)/12;
   y = year1 + 4800 - a;
   m = month1 + 12*a - 3;
   jdn1 = day1 +(153*m+2)/5 + 365*y + y/4 - y/100 + y/400- 32045;

   /* Calculates second julian date */
   res4 = ((2. - year2 / 100.) + (year2 / 400.));
   res5 = (int)(365.25 * year2);
   res6 = (int)(30.6001 * (month2 + 1.));

   /* since we want the julian day since 1900, and technically should be 
    * subtracting the last day
    * jdn2 is the julian day for year 1900, jan 1
    */
   jdn2 = 2415020;

   jdn1 = (jdn1 - jdn2); 
   
   return (int)jdn1;

} /* calculateJulianDay() -------------------------------------------------- */

/*****************************************************************************
 * Module Name: calculatejulday_()
 *
 * Original Author: CHPS/FEWS Team 
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Calculates julian dates used by FORTRAN module.
 *    
 * Calling Arguments:
 *
 *    Name       Input/Output   Type       Description
 *    month1     In             int*       month
 *    day1       In             int*       day
 *    year1      In             int*       year
 *    julday     Out            int*       julian date
 *
 * Return Value: None
 *           
 * Local Variables:
 *    Type       Name                      Description
 *     
 * Global Variables Used:
 *    Type       Name           Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name       Header File               Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
void calculatejulday_(int *month1, int *day1, int *year1, 
                      int *hour1, int *julday)
{
   *julday = -1;
   *julday = calculateJulianDay( *month1, *day1, *year1);
 
   if ( *hour1 == 0 )
   {
      *julday -= 1;
   }
   

} /* calculatejulday_() ---------------------------------------------------- */

/*****************************************************************************
 * Module Name: trimStringEnds()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *
 * Calling Arguments:
 *
 *    Name       Input/Output     Type       Description
 *    str        In               char*      string
 *
 * Return Value: the new string format 
 *           
 * Local Variables:
 *    Type       Name                        Description
 *     
 * Global Variables Used:
 *    Type       Name             Origin     Description
 *
 * Constant and Macro Substitutions:
 *    Name           Header File             Description
 *    INFO_LEVEL     logging.h               user information
 *    SIZE_OF_LINE   utilities.h             maximum size of line
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

char* trimStringEnds(char* string)
{
   int newStringCount = 0;
   int strCount = 0;
   int start = 0;
   int spaceCount = 0;

   char *buffer = NULL;
   
   if ( string == NULL )
   {
      logMessage(DEBUG_LEVEL,"Passed string is NULL. Returning NULL\n");
      return NULL;
   }
  
   buffer = (char*) calloc(SIZE_OF_LINE,sizeof(char));
   
   if ( buffer == NULL )
   {
      logMessage(FATAL_LEVEL,"Error allocating memory in trimStringEnds() "
	                     "for buf...exiting...\n");
      exit(0);
   }
   
   memset(buffer, '\0', SIZE_OF_LINE);
  
   while ( string[strCount] != '\0' )
   {
      if ( start == 0 )
      {
         if ( string[strCount] != ' ' )
         {
            start = 1;
            buffer[newStringCount++] = string[strCount];
         }
      }
      else
      {
         if ( string[strCount] == ' ' )
         {
            spaceCount++;
         }
         else
         {
            if ( spaceCount > 0 )
            {
               for ( ;spaceCount > 0;spaceCount-- )
               {
                  buffer[newStringCount++] = ' ';
               }
	       
               spaceCount = 0;
            }
	    
            buffer[newStringCount++] = string[strCount];
        }
      }
      
      strCount++;
   }
   
   return buffer;
   
} /* trimStringEnds() ------------------------------------------------------ */

/*****************************************************************************
 * Module Name: parseDataTypeMappingLine()
 *
 * Original Author: Ram Varma 
 * 
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Parsed data type mapping line.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output     Type       Description
 *    line       In               char*      string line
 *
 * Return Value:  None
 *           
 * Local Variables:
 *    Type       Name                        Description
 *     
 * Global Variables Used:
 *    Type       Name             Origin     Description
 *
 * Constant and Macro Substitutions:
 *    Name           Header File             Description
 *    SIZE_OF_LINE   utilities.h             Maximum size of line
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void parseDataTypeMappingLine(char *line)
{
   char str[SIZE_OF_LINE];
   
   memset(str, '\0', SIZE_OF_LINE);
   
   if ( line != NULL )
   {
      char *temp = trimStringEnds(line);

      if(temp != NULL)
      {
        strcpy(str, temp);
        free(temp);
      }

      if ( str[0] != '#' )
      {
         sscanf(str, "%s %s %s %s %s %s", 
	     nwsrfsDataTypeMapping[nwsrfsDataTypeMappingCount].dataType,
             nwsrfsDataTypeMapping[nwsrfsDataTypeMappingCount].nwsrfsDimension,
             nwsrfsDataTypeMapping[nwsrfsDataTypeMappingCount].nwsrfsUnit,
             nwsrfsDataTypeMapping[nwsrfsDataTypeMappingCount].isMissingAllowed,
             nwsrfsDataTypeMapping[nwsrfsDataTypeMappingCount].timeCode,
             nwsrfsDataTypeMapping[nwsrfsDataTypeMappingCount].fewsUnit);   

         nwsrfsDataTypeMappingCount++;
	
      }
   }
} /* parseDataTypeMappingLine() --------------------------------------------- */

/*****************************************************************************
 * Module Name: readDataTypeMappingFile()
 *
 * Original Author: Ram Varma 
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Reads data type mapping file.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output          Type           Description
 *
 * Return Value: None
 *           
 * Local Variables:
 *    Type       Name                                 Description
 *     
 * Global Variables Used:
 *    Type       Name                  Origin         Description
 *
 * Constant and Macro Substitutions:
 *    Name                             Header File    Description
 *    NWSRFS_DATATYPE_MAPPING_FILE                    defined at .profile      
 *    MAX_DATATYPEMAPPINGFILENAME_SIZE utilities.h    maximum data type
 *    SIZE_OF_LINE                     utilities.h    size of line
 *    LINE_MAX_LENGTH                  utiltites.h    size is 2048                      
 *
 * Modification History:
 *    Date           Developer        Action
 *    11/15/08       Cham Pham        Replaced getevn() with
 *                                    getDataTypeFileName()
 *
 *****************************************************************************/

void readDataTypeMappingFile()
{
   FILE *fd = NULL;
   char line[SIZE_OF_LINE];
   
   if ( (fd = fopen(getDataTypeFileName(), "r")) == NULL )
   {
      logMessageWithArgsAndExitOnError(FATAL_LEVEL,
      "Could not open units and dimensions file [%s]", 
      getDataTypeFileName());
      
   }
   else
   {
      memset(line, '\0', SIZE_OF_LINE);
      
      while ( fgets(line, LINE_MAX_LENGTH, fd) != NULL )
      {    
         parseDataTypeMappingLine(line);
      }
   }  

   fclose(fd);

} /* readDataTypeMappingFile() ---------------------------------------------- */

/*****************************************************************************
 * Module Name: getDimensionAndUnitAndTimeCode()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Gets dimension and unit
 *
 * Calling Arguments:
 *
 *    Name       Input/Output    Type       Description
 *    tsType     In              char*      time series data type
 *    units      Out             char*      units
 *    dimension  Out             char*      unit dimension
 *    timeCode   Out             char*      time code
 *
 * Return Value: 
 *           
 * Local Variables: the ts dimentsion
 *
 *    Type       Name                       Description
 *     
 * Global Variables Used:
 *    Type       Name            Origin     Description
 *
 * Constant and Macro Substitutions:
 *    Name            Header File           Description
 *    FATAL_LEVEL     logging.h             fatal level
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void getDimensionAndUnitAndTimeCode(char *tsType, char *units, 
                                    char *dimension, char *timeCode)
{
   int i;
   
   char tempTsType[MAX_DATATYPESTRING_LENGTH];

   strncpy(tempTsType, tsType, MAX_DATATYPESTRING_LENGTH-1);
   
   /* null terminate */
   tempTsType[MAX_DATATYPESTRING_LENGTH-1] = 0;
   
   for ( i = 0; i < nwsrfsDataTypeMappingCount; i++ )
   {   
      if ( !strcmp(nwsrfsDataTypeMapping[i].dataType, tempTsType) )
      {         
         break;
      }
   }
   
   if ( i == nwsrfsDataTypeMappingCount )
   {   
      logMessageWithArgsAndExitOnError(FATAL_LEVEL,
      "Data Type = %s not found while getting dimensions\n", tempTsType);
   }

   strcpy(units, nwsrfsDataTypeMapping[i].nwsrfsUnit);
   strcpy(timeCode, nwsrfsDataTypeMapping[i].timeCode);
   strcpy(dimension, nwsrfsDataTypeMapping[i].nwsrfsDimension);


} /* getDimensionAndUnitAndTimeCode() ------------------------------------- */

/*****************************************************************************
 * Module Name: getdimensionandunitinfortran()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Gets dimension and units in FORTRAN module
 *
 * Calling Arguments:
 *
 *    Name       Input/Output   Type            Description
 *    tsType     In             char*           time series type
 *    dimensions out            char*           dimension
 *    units      out            char*           unit
 *
 * Return Value: None
 *           
 * Local Variables:
 *    Type       Name                           Description
 *     
 * Global Variables Used:
 *    Type       Name           Origin          Description
 *
 * Constant and Macro Substitutions:
 *    Name                       Header File    Description
 *    MAX_DATATYPESTRING_LENGTH  logging.h      maximum data type string length
 *    DEBUG_LEVEL                logging.h      debug level- 4
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void getdimensionandunitinfortran_(char *tsType, char *dimensions, char *units)
{
   int i;

   /* Leave the original string alone, and declare the new one to be one more
    * in length to hold a null terminator (the datatype is limited to 4 
    * characters on the FORTRAN side)
    */
   char tempTsType[MAX_DATATYPESTRING_LENGTH];
   strncpy(tempTsType, tsType, MAX_DATATYPESTRING_LENGTH-1);
   
   /* null terminate */
   tempTsType[MAX_DATATYPESTRING_LENGTH-1] = '\0';

    
   if ( getFewsDebugFlag() > 0 )
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
      "in getdimensionandunitinfortran_:"
      " seraching for unit and dimension using type = %s, "
      "number of data types in map = %d\n",
      tempTsType, nwsrfsDataTypeMappingCount);
   } 

   for ( i = 0; i < nwsrfsDataTypeMappingCount; i ++ )
   {
      if ( getFewsDebugFlag() > 5 )
      { 
         logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
	 "globalTsListPtr[i].type = %s\n", nwsrfsDataTypeMapping[i].dataType); 
      }	 

      if ( !strcmp(nwsrfsDataTypeMapping[i].dataType, 
	   trim(tempTsType,MAX_DIMENSIONSTRING_LENGTH)) )
      {
         break;
      }
   }

   if ( i == nwsrfsDataTypeMappingCount )
   {
      logMessageWithArgsAndExitOnError(FATAL_LEVEL,
      "Datatype = %s not found while searching map.\n",tempTsType );
   }

   strcpy(units, nwsrfsDataTypeMapping[i].nwsrfsUnit);
   strcpy(dimensions, nwsrfsDataTypeMapping[i].nwsrfsDimension);

   /* Need to pad with spaces so the FORTRAN sees it correctly on the return */
   padWithSpaces(dimensions, MAX_DIMENSIONSTRING_LENGTH);
   padWithSpaces(units, MAX_UNITSTRING_LENGTH);

} /* getdimensionandunitinfortran_() --------------------------------------- */


/*****************************************************************************
 * Module Name: getPrevParamsInfo()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 11/03/08
 *
 * Description:
 *    Returns the saved params info.
 *    Will be PARAMS_UNCHANGED if params aren't changed
 *    else will contain the params_saved.txt with path
 *
 * Calling Arguments:
 *
 *    Name       Input/Output        Type       Description
 *
 * Return Value: the param file name
 *           
 * Local Variables:
 *    Type       Name                           Description
 *     
 * Global Variables Used:
 *    Type       Name                Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name       Header File                    Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

char* getPrevParamsInfo()
{
   return modelArgs.prevParamsInfo;
}

/*****************************************************************************
 * Module Name: getParamFileName()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Returns a param file name
 *
 * Calling Arguments:
 *
 *    Name       Input/Output        Type       Description
 *
 * Return Value: the param file name
 *           
 * Local Variables:
 *    Type       Name                           Description
 *     
 * Global Variables Used:
 *    Type       Name                Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name       Header File                    Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

char *getParamFileName()
{
   return modelArgs.paramsFile;

} /* getParamFileName() ---------------------------------------------------- */


/*****************************************************************************
 * Module Name: getTsFileName()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Returns a copy time series file name.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output    Type       Description
 *
 * Return Value: the time series file name 
 *           
 * Local Variables:
 *    Type       Name                       Description
 *     
 * Global Variables Used:
 *    Type       Name            Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name       Header File                Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
char *getTsFileName()
{
   return modelArgs.tsFile;

} /* getTsFileName() ------------------------------------------------------- */

/*****************************************************************************
 * Module Name: getOutputTsFileName()
 *
 * Original Author: CHPS/FEWS Team 
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *   Returns a copy output time series file name.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output        Type       Description
 *
 * Return Value: the output time series file name.
 *           
 * Local Variables:
 *    Type       Name                           Description
 *     
 * Global Variables Used:
 *    Type       Name                 Origin    Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name       Header File                    Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

char *getOutputTsFileName()
{
   return modelArgs.outputTsFile;

} /* getOutputTsFileName() ------------------------------------------------- */


/*****************************************************************************
 * Module Name: getStateFileName()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Returns a copy state (carryover) file name.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output    Type       Description
 *    fileName   In              char*      state file name
 *
 * Return Value: the state file name
 *           
 * Local Variables:
 *    Type       Name                       Description
 *     
 * Global Variables Used:
 *    Type       Name            Origin     Description
 *
 * Constant and Macro Substitutions:
 *    Name       Header File                Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

char *getStateFileName()
{
   return modelArgs.inputStatesFile;
   
} /* getStateFileName() ---------------------------------------------------- */


/*****************************************************************************
 * Module Name: getOutputStateFileName()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Returns a copy output state file name.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output     Type       Description
 *
 * Return Value: the output state file name 
 *           
 * Local Variables:
 *    Type       Name                        Description
 *     
 * Global Variables Used:
 *    Type       Name             Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name       Header File                 Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

char *getOutputStateFileName()
{
   return modelArgs.outputStatesFile;

} /* getOutputStateFileName() ---------------------------------------------- */


/*****************************************************************************
 * Module Name: setDataTypeFileName() and getDataTypeFileName()
 *
 * Original Author: Cham Pham 
 *
 * Module Creation Date: 11/15/08
 *
 * Description:
 *    Returns NWS data type mapping file name
 *
 * Calling Arguments:
 *
 *    Name       Input/Output     Type       Description
 *
 * Return Value: the NWS data type mapping file name 
 *           
 * Local Variables:
 *    Type       Name                        Description
 *     
 * Global Variables Used:
 *    Type       Name             Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name       Header File                 Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
void setDataTypeFileName(char *fileName)
{
   strcpy(datatypeFileName, fileName);
   
   datatypeFileName[strlen(datatypeFileName)] = '\0';

} /* setDataTypeFileName() ------------------------------------------------- */

char *getDataTypeFileName()
{
   return datatypeFileName;

} /* getDataTypeFileName() ------------------------------------------------- */

/*****************************************************************************
 * Module Name: setDataUnitFileName() and getDataUnitFileName()
 *
 * Original Author: Cham Pham 
 *
 * Module Creation Date: 12/02/08
 *
 * Description:
 *    Returns NWS data unit file name
 *
 * Calling Arguments:
 *
 *    Name       Input/Output     Type       Description
 *
 * Return Value: the NWS data unit file name 
 *           
 * Local Variables:
 *    Type       Name                        Description
 *     
 * Global Variables Used:
 *    Type       Name             Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name       Header File                 Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
void setDataUnitFileName(char *fileName)
{
   strcpy(dataunitFileName, fileName);
   
   dataunitFileName[strlen(dataunitFileName)] = '\0';

} /* setDataUnitFileName() ------------------------------------------------- */

char *getDataUnitFileName()
{
   return dataunitFileName;

} /* getDataTypeFileName() ------------------------------------------------- */

void setModFileName(char *fileName)
{
   strcpy(modunitFileName, fileName);
   
   modunitFileName[strlen(modunitFileName)] = '\0';

} 
/*****************************************************************************
 * Module Name: getModFileName()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Returns mod file name
 *
 * Calling Arguments:
 *
 *    Name       Input/Output     Type       Description
 *
 * Return Value: the mod file name 
 *           
 * Local Variables:
 *    Type       Name                        Description
 *     
 * Global Variables Used:
 *    Type       Name             Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name       Header File                 Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

char *getModFileName()
{
   return modunitFileName;

} /* getModFileName() ---------------------------------------------- */

/*****************************************************************************
 * Module Name: getJulianDayForStartOfDriverTSData()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Gets julian date for start of driver time series data.
 *
 * Calling Arguments:
 *
 *    Name      Input/Output        Type       Description
 *
 * Return Value: the julian date
 *           
 * Local Variables:
 *    Type      Name                           Description
 *     
 * Global Variables Used:
 *    Type      Name                Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name      Header File                    Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

int getJulianDayForStartOfDriverTSData()
{
   return modelArgs.julianDayForStartOfDriverTSData;
   
} /* getJulianDayForStartOfDriverTSData() ---------------------------------- */

/*****************************************************************************
 * Module Name: getJulianHourForStartOfDriverTSData()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Get julian hour for start of driver time series data.
 *
 * Calling Arguments:
 *
 *    Name      Input/Output        Type       Description
 *
 * Return Value: the julian hour 
 *           
 * Local Variables:
 *    Type      Name                           Description
 *     
 * Global Variables Used:
 *    Type      Name                Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name      Header File                    Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

int getJulianHourForStartOfDriverTSData()
{
   return modelArgs.julianHourForStartOfDriverTSData;
   
} /* getJulianHourForStartOfDriverTSData() --------------------------------- */

/*****************************************************************************
 * Module Name: getJulianDayForStartOfRun()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Get julian date for start of run.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output        Type       Description
 *
 * Return Value: the julian date start of run 
 *           
 * Local Variables:
 *    Type      Name           Description
 *     
 * Global Variables Used:
 *    Type         Name                     Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name           Header File           Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

int getJulianDayForStartOfRun()
{
   return modelArgs.julianDayForStartOfRun;
   
} /* getJulianDayForStartOfRun() ------------------------------------------- */

/*****************************************************************************
 * Module Name: getJulianHourForStartOfRun()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Get julian hour for start of run.
 *
 * Calling Arguments:
 *
 *    Name      Input/Output        Type       Description
 *
 * Return Value: the julian hour start of run 
 *           
 * Local Variables:
 *    Type      Name                           Description
 *     
 * Global Variables Used:
 *    Type      Name                Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name      Header File                    Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

int getJulianHourForStartOfRun()
{
   return modelArgs.julianHourForStartOfRun;
   
} /* getJulianHourForStartOfRun() ------------------------------------------ */

/*****************************************************************************
 * Module Name: getJulianDayForEndOfRun()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Gets julian date for end of run.
 *
 * Calling Arguments:
 *
 *    Name      Input/Output     Type       Description
 *
 * Return Value: the julian date end of run
 *           
 * Local Variables:
 *    Type      Name                        Description
 *     
 * Global Variables Used:
 *    Type      Name             Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name      Header File                 Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
int getJulianDayForEndOfRun()
{
   return modelArgs.julianDayForEndOfRun;
   
} /* getJulianDayForEndOfRun() --------------------------------------------- */

/*****************************************************************************
 * Module Name: getJulianHourForEndOfRun()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Gets julian hour for end of run.
 *
 * Calling Arguments:
 *
 *    Name      Input/Output     Type       Description
 *
 * Return Value: the julian hour end of run
 *           
 * Local Variables:
 *    Type      Name                        Description
 *     
 * Global Variables Used:
 *    Type      Name             Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name      Header File                 Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

int getJulianHourForEndOfRun()
{
   return modelArgs.julianHourForEndOfRun;
   
} /* getJulianHourForEndOfRun() -------------------------------------------- */


/*****************************************************************************
 * Module Name: getFewsDebugFlag()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 *    Return a copy fews debug flag.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output        Type       Description
 *
 * Return Value: the fews debug flag
 *           
 * Local Variables:
 *    Type       Name                           Description
 *     
 * Global Variables Used:
 *    Type       Name                Origin     Description
 *
 * Constant and Macro Substitutions:
 *    Name           Header File           Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

int getFewsDebugFlag()
{
   return fewsDebugFlag;
   
} /* getFewsDebugFlag() ---------------------------------------------------- */

/*****************************************************************************
 * Module Name: setFewsDebugFlag()
 *
 * Original Author: CHPS/FEWS Team 
 *
 * Module Creation Date:  05/05/08
 *
 * Description:
 *    Makes a copy fews debug flag.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output        Type       Description
 *    flag       In                  int        debug flag
 *
 * Return Value: None
 *           
 * Local Variables: None
 *    Type       Name                           Description
 *     
 * Global Variables Used:
 *    Type       Name                Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name       Header File                    Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void setFewsDebugFlag(int flag)
{
   fewsDebugFlag = flag;

} /* setFewsDebugFlag() ---------------------------------------------------- */

/*****************************************************************************
 * Module Name: readOptions()
 *
 * Original Author: Cham Pham
 *
 * Module Creation Date:  01/05/09
 *
 * Description:
 *    This function reads the command line options for OHD models
 *
 * Calling Arguments:
 *
 *    Name       Input/Output        Type       Description
 *    argc       In                  int
 *    argv       In                  char       
 *
 * Return Value: a boolean indicating success or failure.
 *           
 * Local Variables: None
 *    Type       Name                           Description
 *     
 * Global Variables Used:
 *    Type       Name                Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name       Header File                    Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
int readOptions(int argc, char**argv)
{
   extern char      *optarg; /* used by getopt              */
   extern int       optind;  /* total number of arguments   */
   int              c;       /* to read the program options */
   int              err=0;   /* for error condition         */

   /* NOTE: This method has to use printf statement because it needs to show 
    *       message on console prompt
    */
   while ( (c = getopt(argc, argv, "Vh?")) != EOF )
   {
      switch(c)
      {
         case 'h':
	    printUsage( argv );
	    break;
         case 'V':
            printf("\n\tUsing OHD Model: %s  (Version: %s%s)\n\n",
	  	   argv[0], VERSION_NAME, VERSION_NUMBER);
	    break;
         case '?':
            err = -1;
            break;
      }
   }
  
   if ( optind == argc )
   {
      err = -1;
   }

   if ( err < 0 )
   {
      if ( (argv[optind-1][1] != 'V') && (argv[optind-1][1] != 'h') )
      {
         printf ("\n\tType '%s -h' for help\n\n", argv[0]);
      }
   }
  
   return ( err );

} /* readOptions() --------------------------------------------------------- */

/*****************************************************************************
 * Module Name: printUsage()
 *
 * Original Author: Cham Pham
 *
 * Module Creation Date:  01/10/09
 *
 * Description:
 *    This function just writes out the usage of OHD models
 *
 * Calling Arguments:
 *
 *    Name       Input/Output        Type       Description
 *    argv       In                  char       
 *
 * Return Value: a boolean indicating success or failure.
 *           
 * Local Variables: None
 *    Type       Name                           Description
 *     
 * Global Variables Used:
 *    Type       Name                Origin     Description
 *
 *
 * Constant and Macro Substitutions:
 *    Name       Header File                    Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
void printUsage (char **argv)
{

   /* NOTE: This method has to use printf statement because it needs to show 
    *       message on console 
    */
   if ( strcmp(argv[0],"genStatesRessngl") == 0 )
   {
      printf ( "Usage: %s    <params.txt> <statesI.txt> <diag.txt>\n"
	       "\t\t\t   <nwsrfs_datatype_mapping_file.txt>\n", argv[0] );
   }
   else if ( strcmp(argv[0],"genStatesResj") == 0 )
   {
      printf ( "Usage: %s    <params.txt> <statesI.txt> <diag.txt>\n"
	       "\t\t\t<nwsrfs_datatype_mapping_file.txt>\n"
	       "\t\t\t<nwsrfs_dataunit_file.txt>\n", argv[0] );
   }
   else
   {
      printf ( "Usage: %s    <argumentsFile> \n", argv[0] );
   }
   
   printf ( "       OR\n" );
   printf ( "       %s    <options>\n", argv[0] );
   printf ( "       \t\toptions:\n" );
   printf ( "       \t\t\t-h (for help)\n" );
   printf ( "       \t\t\t-V (for Version number)\n\n" );
   
} /* printUsage() -----------------------------------------------------------*/


void readPropertiesFromFile(int *numOfProperties, 
                            char *propertyFileName, Properties *properties,
                            int *units)
{
   char *propertyLine = NULL;
   char *token = NULL;
   char *value = NULL;
   FILE *propertyFilePtr;
   int  unitFlagExists = FALSE;

   if(properties == NULL)
   {
      exit(-1);
   }

   if ((propertyFilePtr = fopen(propertyFileName, "rb")) == NULL)
   {
      logMessageWithArgsAndExitOnError(FATAL_LEVEL,
                "Property File %s could not be read\n", propertyFileName);
   }

   int propertyCount = 0;

   propertyLine = (char *)malloc(256*sizeof(char));

   memset(propertyLine, 0, sizeof(propertyLine));

   // Read word by word and the format accepted for parsing
   // is key=value. Note: no space before or after '='
   while (fscanf(propertyFilePtr,"%s",propertyLine) != EOF)
   {
      if(strchr(propertyLine,'=') != NULL)
      {
         // if the propertyLine doesn't have '=' in it skip
         if(propertyLine[strlen(propertyLine) - 1] == '=')
         {
            logMessageWithArgsAndExitOnError(FATAL_LEVEL,
              "Property File %s is not in appropriate format", 
               propertyFileName);
         }
            
         token=strtok(propertyLine,"=");
         value = strtok(NULL, "=");

         // if there is space after the key then the
         // next read for fscanf will be '=value' and
         // this will lead to the value being considered as token
         // and hence the "real value" will not be available
         // so we have to skip
         if(token != NULL && value == NULL)
         {
            logMessageWithArgsAndExitOnError(FATAL_LEVEL,
              "Property File %s is not in appropriate format", 
               propertyFileName);
         }

        
	 properties[propertyCount].name = (char *)
                              calloc((strlen(token)+1), sizeof(char));
         properties[propertyCount].value = (char *)
                              calloc((strlen(value)+1), sizeof(char));

         if (strcmp(token,"UNIT") == 0)
         {
            unitFlagExists = TRUE;
            *units = 1;
            if (strcmp(value,"ENGLISH") == 0)
            {
                *units = 0;
            }
         }

         strcpy(properties[propertyCount].name,token);
         strcpy(properties[propertyCount].value,value);

         if(getFewsDebugFlag() > 0)
         {
            logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
                "Token [%s] Value [%s]", properties[propertyCount].name,
                 properties[propertyCount].value);
         }

         propertyCount++;
         memset(propertyLine, 0, sizeof(propertyLine));
      }
   }

   *numOfProperties = propertyCount;
 
   free(propertyLine);
 
   fclose(propertyFilePtr);
}

/*****************************************************************************
 * Module Name: convertFromJulianHourToMMDDYYYYHHZ()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *    Convertes julian hour to YYYYMMDDHHZ format
 *
 * Calling Arguments:
 *
 *    Name         Input/Output  Type       Description
 *    julianHours  In            int        julian hour
 *    dateString   Out           char[]     date string
 *
 * Return Value: None
 *           
 * Local Variables:
 *    Type         Name                     Description
 *
 * Global Variables Used:
 *    Type         Name          Origin     Description
 *
 * Constant and Macro Substitutions:
 *    Name               Header File        Description
 *    DATESTRINGLENGTH   commonInclude.h
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void convertFromJulianHourToMMDDYYYYHHZ(int julianHours, 
                                        char dateString[DATESTRINGLENGTH])
{    
//   int julianDay;
 //  int julianHourOfDay;
  // int month, day, year, hour, daylightSwitch, timeZoneNumber;
   char code[] = "Z  ";

   int julianDay = -1;
   int julianHourOfDay = -1;
   int month, day, year, hour, daylightSwitch, timeZoneNumber;
   month= day= year= hour= daylightSwitch= timeZoneNumber = -1;
   

   julianDay = (julianHours / 24);
   julianHourOfDay = fmod(julianHours, 24);
	
   if ( getFewsDebugFlag() > 5 )
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
      "julian day = %d julian hour of day = %d", julianDay, julianHourOfDay);
   }
   
   /* This mdyh2_ is strange, suddently it requires the julianDay on a 
    * clock where julian day starts at midnight
    */
   /* This logic apply for forecasting mode 12Z */
   if ( StartOfDay > 0 )
   {
      if (julianHourOfDay >= StartOfDay)
      {
         julianDay += 1;
         julianHourOfDay -= StartOfDay;
      }
      else if (julianHourOfDay >  0)
      {
//         julianHourOfDay = 12 + julianHourOfDay;
	 if ((StartOfDay + julianHourOfDay) > 24) 
	 {
             julianDay += 1;
             julianHourOfDay = (StartOfDay - 24) + julianHourOfDay;	 
	 }
	 else 
	 {
	     julianHourOfDay = StartOfDay + julianHourOfDay;
	 }
      }
      else /* julianHourOfDay = 0 */
      {
         julianHourOfDay = StartOfDay;
      }
   }

   mdyh2_(&julianDay, &julianHourOfDay, &month, &day, &year, &hour,
          &timeZoneNumber, &daylightSwitch, code);
   
   sprintf(dateString,"%04d%02d%02d%02d",year,month,day,hour);

} /* convertFromJulianHourToMMDDYYYYHHZ() ---------------------------------- */


/*****************************************************************************
 * Module Name: getversioninfofromfortran_()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/09
 *
 * Description:
 *    used to get version name and number from fortran
 *    it returns values in version_info.h
 *
 * Calling Arguments:
 *
 *    name  (output)                     char[]
 *    number (output)                    char[]
 * Return Value: None
 *           
  
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void getversioninfofromfortran_(char *name, char *number)
{

   strcpy(name, VERSION_NAME);
   strcpy(number, VERSION_NUMBER);

   /* Need to pad with spaces so the FORTRAN sees it correctly on the return */
   padWithSpaces(name, VERSION_NAME_LENGTH+1);
   padWithSpaces(number, VERSION_NUMBER_LENGTH+1);

} /* getversioninfofromfortran_() --------------------------------------- */


/*****************************************************************************
 * Module Name: removeBlankSpacesFromString(char *)
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 6/15/09
 *
 * Description:
 *    Removes all blank spaces from the input string
 *    and returns the string that has no blank spaces in it.
 *
 * Calling Arguments: char *
 *
 * Return Value: char *
 *
 *
 * Modification History:
 *           Date           Developer        Action
 *      6/15/2009    Varalakshmi Rajaram    Initial Implementation
 *
 *****************************************************************************/

char* removeBlankSpacesFromString(char* str)
{
  char *res = NULL;
  if(str == NULL)
   return res;

  int len=0;
  len = strlen(str);
  res = (char*) malloc(sizeof(char)*len);
  int cnt=0,i=0;
  for(cnt=0; cnt< len; cnt++)
   {
     if(str[cnt]!=' ' && str[cnt]!= '\t')
     {
      res[i++]=str[cnt];
     }
      
   }
  res[i]='\0';
  return res;
}

/** returns 0 --- if file is empty
 * returns 1 --- if file is not empty
 * **/

int checkFileEmptyStatus(char* fileName)
{
  off_t st_size;
  struct stat *buf;

  buf = (struct stat*) malloc(sizeof(struct stat));
  
  stat(fileName, buf);
  st_size = (off_t) buf->st_size;

  if(buf != NULL)
    free(buf);

  if(st_size == 0)
  {
    logMessageWithArgsAndExitOnError(WARNING_LEVEL,
           "%s File is empty\n", fileName );
     return 0;
  }

  return 1;

}
