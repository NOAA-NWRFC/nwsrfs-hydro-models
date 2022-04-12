#ifndef _UTILITIES_H
#define _UTILITIES_H
 
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <math.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "logging.h"
#include "timeseries.h"
#include "commonInclude.h"
#include "fortranCommonIncludes.h"
#include "version_info.h"

#define TS_FILE_NAME "tsFileName"
#define PARAM_FILE_NAME "paramFileName"
#define STATE_FILE_NAME "stateFileName"
#define OUTPUT_TS_FILE_NAME "outputTsFileName"
#define OUTPUT_STATE_FILE_NAME "outputStateFileName"
#define DIAG_FILE_NAME "diagFileName"
#define DEBUG_FLAG "debugFlag"
#define JULIAN_DAY_FOR_START_OF_RUN "julianDayForStartOfRun"
#define JULIAN_HOUR_FOR_START_OF_RUN "julianHourForStartOfRun"
#define JULIAN_DAY_FOR_START_OF_DRIVER_TS_DATA "julianDayForStartOfDriverTsData"
#define JULIAN_HOUR_FOR_START_OF_DRIVER_TS_DATA "julianHourForStartOfDriverTsData" 
#define JULIAN_DAY_FOR_END_OF_RUN "julianDayForEndOfRun"
#define JULIAN_HOUR_FOR_END_OF_RUN "julianHourForOfEndOfRun"
#define JULIAN_DAY_FOR_LAST_OBS_DATE "julianDayForLastObsDate"
#define JULIAN_HOUR_FOR_LAST_OBS_DATE "julianHourForLastObsDate"
#define PREV_PARAMS_INFO "prevParamsInfo"
#define LOCAL_HOUR_OFFSET "localHourOffset"
#define DATA_TYPE_FILE_NAME "dataTypeFileName"
#define MOD_FILE_NAME "modFileName"
#define DATA_UNIT_FILE_NAME "dataUnitFileName"
#define BASEFLOW_MODEL_TIME_INTERVAL "baseflowModelTimeInterval"
#define START_OF_DAY "startOfDay"
#define FFG_DURATION_HR "FFGdurationHR"
#define RESSNGL_TECHNIQUE "ressnglTechnique"
#define MAPE_TIMESTEP "mapeTimeStep"
#define NUMBEROFHOURSINDAY 24
#define MAX_DATATYPEMAPPINGFILENAME_SIZE 1024
#define NWSRFS_MAPPING_FILE_SIZE 500
#define SIZE_OF_LINE 2048
#define LINE_MAX_LENGTH 2048
#define FILE_NAME_LENGTH 512
#define TOKEN_NAME_LENGTH 80
#define TOKEN_VALUE_LENGTH 512
#define MAX_PROPERTIES 2000
#define TRUE 1
#define FALSE 0

typedef struct
{
  char *name;
  char *value;
}Properties;

typedef struct ModelArgs
{
   char tsFile[FILE_NAME_LENGTH];
   char paramsFile[FILE_NAME_LENGTH];
   char inputStatesFile[FILE_NAME_LENGTH];
   char outputTsFile[FILE_NAME_LENGTH];
   char outputStatesFile[FILE_NAME_LENGTH];
   char diagFile[FILE_NAME_LENGTH];
   int  printDebugFlag;
   int  julianDayForStartOfRun;
   int  julianHourForStartOfRun;
   int  julianDayForStartOfDriverTSData;
   int  julianHourForStartOfDriverTSData;
   int  julianDayForEndOfRun;
   int  julianHourForEndOfRun;
   int  julianDayForLastObsDate;
   int  julianHourForLastObsDate;
   char prevParamsInfo[FILE_NAME_LENGTH];
   int  localHourOffset;
   char datatypeFile[MAX_DATATYPEMAPPINGFILENAME_SIZE];
   char modFile[FILE_NAME_LENGTH];
   char dataUnitFile[FILE_NAME_LENGTH];
   int  baseflowModelTimeInterval;
   int  startOfDay;
   int  FFGdurationHR;
   int  ressnglTechnique;
   int  mapeTimeStep;
}ModelArgs;

typedef struct NwsrfsDataTypeMapping
{
   char dataType[13];
   char nwsrfsDimension[9];
   char nwsrfsUnit[9];
   char isMissingAllowed[9];
   char timeCode[9];
   char fewsUnit[9];
}NwsrfsDataTypeMapping;

void readPropertiesFromFile(int *numOfProperties, 
                char *propertyFileName, Properties *properties, 
                int *units);

void replaceNewLineCharWithNullChar(char* str);

void readModelArgs();

Properties* readArgs(int* numOfProperties);

void logModelArgs();

int doCox();

char* getArgsFileName();

void setArgsFileName(char *fileName);

void setInformationFromArguments(char *argsFileName);

struct ModelArgs getModelArgs();

void padWithSpaces(char *inputString, int maxLen);

int getTimeInfo(char **argv, int index);

char *getTimeSeriesIdFromPArray(float *p, int startIndex);

char *getTimeSeriesCodeFromPArray(float *p, int startIndex);

int getIntegerFromPArray(float *p, int startIndex);

int getNumberOfElementsInTimeSeries(int resolution);

void numberoftimesteps(int *resolution, int *numberOfTimeSteps);

int calculateJulianDay(int month1, int day1, int year1);

void calculatejulday_(int *month1, int *day1, int *year1, int *hour1, int *julday);

void setModFileName(char *fileName);

void setDataTypeFileName(char *filename);
void setDataUnitFileName(char *filename);
char *getDataTypeFileName();
char *getDataUnitFileName();

void readDataTypeMappingFile();

void getDimensionAndUnitAndTimeCode(char *tsType, char *units,
                                    char *dimension, char *time_code);

void getDimensionAndUnitInFortran_(char *tsType, char *dimensions, char *units);

char *getParamFileName();

char* getPrevParamsInfo();

char *getOutputTsFileName();

char *getOutputStateFileName();

int getJulianDayForStartOfDriverTSData();

int getJulianHourForStartOfDriverTSData();

int getJulianDayForEndOfRun();

int getJulianHourForEndOfRun();

int getFewsDebugFlag();

void setFewsDebugFlag (int flag);

void initializeTimeSeriesTypesList();

void parseDataTypeMappingLine(char *line);

char* trimStringEnds(char* str);

char *trim(char *inputString, int maxLen);

int readOptions(int argc, char**argv);

void printUsage(char **argv);

void convertFromJulianHourToMMDDYYYYHHZ(int julianHours,  char dateString[]);

void getversioninfofromfortran_(char *name, char *number);

char* removeBlankSpacesFromString(char* str);

float getFloatFromPArray(float *pArray, int startIndex);

int checkFileEmptyStatus(char* fileName);

void freeProperties(Properties *properties, int numOfProperties);
#endif
