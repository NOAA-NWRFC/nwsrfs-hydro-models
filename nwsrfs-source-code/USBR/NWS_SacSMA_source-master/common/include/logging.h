#ifndef _LOGGIING_H
#define _LOGGIING_H

#define FATAL_LEVEL 0
#define ERROR_LEVEL 1
#define WARNING_LEVEL 2
#define INFO_LEVEL 3
#define DEBUG_LEVEL 4

#define SUCCESS 0
#define FAILURE 1

#define MAX_DIMENSIONSTRING_LENGTH 5
#define MAX_UNITSTRING_LENGTH 5
#define MAX_DATATYPESTRING_LENGTH 5
#define SIZE_OF_FORMAT_STRING 8

#define MAX_NUM_OF_VALUES_IN_A_LINE 15
#define MAX_VALUE_WIDTH_FOR_DISPLAY 20
#define MAX_VALUE_PRECISION_FOR_DISPLAY 4
#define MAX_LOG_LINES 20
#define MAX_LOG_LENGTH 512

void logMessage(int level, char *message);

void logMessageAndExitOnError(int level, char *message);

void logMessageWithArgsAndExitOnError(int level, char *message, ...);

void logfromfortran_(int *level, char message[][MAX_LOG_LENGTH]);

void logonedimensionarrayfromfortran_(int *level, char *orgUserMessage,
                          int *numOfArraysPassed,  
			  int *numOfValuesRequestForDisplayInALine,
                          char orgFormatString[SIZE_OF_FORMAT_STRING],
                          int *startIndex, int *endIndex, int *incrValue, ...);

char* determineType(char* formatString);

void logArrays(int level, FILE *diagFilePtr,
              char *message,  int numOfArraysPassed,
              int numOfValuesDisplayedInALine, char *valueDisplayFormatString,
              int startIndex, int endIndex, int incrValue, va_list arg_ptr);
	      
	      
void logUserMessageSinceArrayIsNull(char *orgUserMessage);

int determineTotalLengthUsedByValuesInALine(int valueWidth, 
                                            int numOfValuesInALine);

int isLogLineLengthHasExceeded(int length);

int isAllBlank(char* message);

char* getAppropriateUserMessage(char* orgUserMessage, int valueWidth, 
                                int numOfValuesInALine);

int getValueWidth(int valueWidth);

int getNumOfValuesToDisplayInALine(int numOfValuesRequestedInALine);

int getValueWidthToUse(char *type, char *orgFormatString);

char* getAppropriateFormatString(char* type, char *orgFormatString);

int getFirstIndex(char* str, char c);

char* trimString(char* str);

void writeArraysToLogFile(int level,char* type, void **valuesPassed, 
                          char* orgFormatString,
                          char* message, int numOfArraysPassed,
                          int numOfValuesToDisplayInALine, int startIndex, 
			  int endIndex, int incrValue);


int breakCondition(int loopCnt, int incrValue, int endIndex);

void setDiagFileName(char *fileName);

char *getDiagFileName();

void openDiagFile();

void closeDiagFileAndExit(int code);

int findNumberOfLinesInLogMessageFromFortran(char message[][MAX_LOG_LENGTH]);

int getLengthAndNullTerminateLogMessageFromFortran(char *mLine);

void exitOnLevel(int level);

void closeDiagFile();

#endif
