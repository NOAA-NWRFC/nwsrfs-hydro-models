/*.............................................................................
* File: logging.c
* Author(s): CHPS/FEWS Team
* Date Created: 5/5/08
*
* Development group: OHD HSEB
*
* Purpose: various routines to implement logging to diagnostic file
*	   from legacy (fortran, c, and c++) code	 
*                                   
* Module(s): 
*     logMessage
*     logMessageAndExitOnError
*     logMessageWithArgsAndExitOnError
*     logfromfortran_
*     logonedimensionarrayfromfortran_ 
*     determineType (used internally)
*     logArrays (used internally)
*     logUserMessageSinceArrayIsNull (used internally)
*     determineTotalLengthUsedByValuesInALine (used internally)
*     isLogLineLengthHasExceeded (used internally)
*     isAllBlank (used internally)
*     getAppropriateUserMessage (used internally)
*     getValueWidth (used internally)
*     getNumOfValuesToDisplayInALine (used internally)
*     getValueWidthToUse (used internally)
*     getAppropriateFormatString (used internally)
*     getFirstIndex (used internally)
*     trimString (used internally)
*     writeArraysToLogFile (used internally)
*     breakCondition (used internally)
*     setDiagFileName
*     getDiagFileName 
*     openDiagFile 
*     closeDiagFile
*     closeDiagFileAndExit 
*     findNumberOfLinesInLogMessageFromFortran (used internally)
*     getLengthAndNullTerminateLogMessageFromFortran (used internally)
*     exitOnLevel
*                                
* Date Modified:                             
*
*............................................................................*/
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <math.h>

#include "logging.h"

static char diagFileName[512];
FILE *diagFilePtr = NULL;

/******************************************************************************
 * Module Name: logMessage()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 * Used to log single line messages from .c files.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output        Type       Description
 *    level      I                   int        INFO_LEVEL/FATAL_LEVEL/
 *                                              DEBUG_LEVEL/WARNING_LEVEL
 *    message    I                   char*      user message to log
 *
 * Return Value: None
 *           
 * Local Variables:
 *   Type        Name                           Description
 *
 * Global Variables Used:
 *    Type       Name                Origin     Description
 *
 * Constant and Macro Substitutions:
 *  Name         Header File                    Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
void logMessage(int level, char *message)
{
   int fewsLogFlag;

   openDiagFile();

   /* Print blank lines as well	*/
   fprintf(diagFilePtr,"%d|%s\n", level, message);

} /* logMessage() ---------------------------------------------------------- */

/******************************************************************************
 * Module Name: logMessageAndExitOnError()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *    Used to log single line messages from .c files.  If the level is FATAL 
 *    then the process exits.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output        Type       Description
 *    level      I                   int        INFO_LEVEL/FATAL_LEVEL/
 *                                              DEBUG_LEVEL/WARNING_LEVEL
 *    message    I                   char*      user message to log 
 *
 * Return Value: None
 *           
 * Local Variables:
 *   Type        Name                           Description
 *
 * Global Variables Used:
 *    Type       Name                Origin     Description
 *
 * Constant and Macro Substitutions:
 *  Name        Header File                     Description
 *  FATAL_LEVEL logging.h                       indicates 0
 *  FAILURE     logging.h                       indicates 1
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
void logMessageAndExitOnError(int level, char *message)
{
   logMessage(level, message);

   if ( level == FATAL_LEVEL )
   {
      closeDiagFileAndExit(FAILURE);
   }

} /* logMessageAndExitOnError() -------------------------------------------- */


/******************************************************************************
 * Module Name: logMessageWithArgsAndExitOnError()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *  Used to log single line messages along with the args from .c files.
 *  If the level is FATAL then the process exits.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output        Type       Description
 *    level      I                   int        INFO_LEVEL/FATAL_LEVEL/
 *                                              DEBUG_LEVEL/WARNING_LEVEL
 *    message    I                   char*      user message to log 
 *    ...        I                              varaiable arguments
 *
 * Return Value: None
 *           
 * Local Variables:
 *   Type        Name                           Description
 *
 * Global Variables Used:
 *    Type       Name                Origin     Description
 *
 * Constant and Macro Substitutions:
 *  Name           Header File                  Description
 *  MAX_LOG_LENGTH logging.h                    indicates 512
 *  FAILURE     logging.h                       indicates 1
 *  FATAL_LEVEL logging.h                       indicates 0
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
void logMessageWithArgsAndExitOnError(int level, char *message, ...)
{
   va_list ap;

   /* Make sure message length doesn't exceed 512 */
   char tempMessage[MAX_LOG_LENGTH];

   memset(tempMessage, '\0', sizeof(tempMessage));
   sprintf(tempMessage, "%d|%s\n",level,message);
   va_start(ap, message);

   openDiagFile();

   /* Since we don't have a diagnostics file to log to , just exit */
   if ( diagFilePtr == NULL )
   {
      exit(FAILURE);	
   }
   
   vfprintf(diagFilePtr, tempMessage, ap);
   va_end(ap);

   if ( level == FATAL_LEVEL )
   {
      closeDiagFileAndExit(FAILURE);
   }
} /* logMessageWithArgsAndExitOnError() ----------------------------------- */

 
/******************************************************************************
 * Module Name: logfromfortran_()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *  Used to log multiple line messages from .f files.
 *  If the level is FATAL then the process exits.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output   Type       Description
 *    level      I              int        INFO_LEVEL/FATAL_LEVEL/
 *                                         DEBUG_LEVEL/WARNING_LEVEL
 *    message    I              char[][]   array of user messages
 *
 * Return Value: None
 *           
 * Local Variables:
 *   Type        Name                      Description
 *
 * Global Variables Used:
 *    Type       Name           Origin     Description
 *
 * Constant and Macro Substitutions:
 *  Name           Header File             Description
 *  MAX_LOG_LINES  logging.h               indicates 20
 *  MAX_LOG_LENGTH logging.h               indicates 512
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
void logfromfortran_(int *level, char message[][MAX_LOG_LENGTH])
{
   int i = 0;
   int height = 0;
   int length = 0;
   char tempMessage[MAX_LOG_LINES][MAX_LOG_LENGTH];
	
   memset(tempMessage, 0, MAX_LOG_LENGTH*MAX_LOG_LINES);

   if(message != NULL)
   {
   /* Important not to mess up original string */
   for ( i = 0; i < MAX_LOG_LINES; i++ )
   {
      memset(tempMessage[i],'\0', MAX_LOG_LENGTH);
      strncpy(tempMessage[i], message[i],MAX_LOG_LENGTH);
   }
 
   openDiagFile();

   /* Figure out how many lines there are in this one message - height will 
    * come from some header file,as will line length
    */
   height = findNumberOfLinesInLogMessageFromFortran(tempMessage);
   
   for ( i = 0; i < height; i++ )
   {
      length = getLengthAndNullTerminateLogMessageFromFortran(tempMessage[i]);
	
      if ( length > 0 )
      {	
         fprintf(diagFilePtr,"%d|%s\n",*level, tempMessage[i]);
      }
   }	

   memset(message, 0, MAX_LOG_LENGTH*MAX_LOG_LINES);
   }

   exitOnLevel(*level);
   
} /* logfromfortran_() ----------------------------------------------------- */

/******************************************************************************
 * Module Name: logonedimensionarrayfromfortran_()
 *
 * Original Author: Varalakshmi Rajaram 
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 * To log values in an array that is passed from fortran.
 *
 * This method will access the array from the start index and continue until 
 * end index.
 * if incrValue is +ve then incr the array access by +incrValue
 * if incrValue is -ve then decr the array access by -incrValue
 *
 * The printing goes from startindex-1 to endindex-1 since fortran array 
 * starts from 1 and not 0 unlike C.
 *
 * Note: If multiple arrays are passed we assume all arrays start and end at 
 *       same index.
 *       All arrays are of the same data type (int or float) and no 
 *       combination allowed
 *       All arrays will be printed using the formatstring the user sends in
 *       The format string will dictate the data type of the array
 *       ( the f or d in the string is used for this purpose )
 *
 * Calling Arguments:
 *
 *    Name                       Input/Output   Type   Description
 *    level                      I              int    INFO_LEVEL/FATAL_LEVEL/
 *                                                     DEBUG_LEVEL/WARNING_LEVEL
 *    orgUserMessage             I              char*  user message to log
 *    numOfArraysPassed          I              int*   number of arrays to logg
 *    numOfValuesDisplayedInALineI              int*   number of values to be 
 *                                                     displayed in each row, 
 *                                                     if there are 2 arrays 
 *                                                     passed then this number 
 *                                                     denoted how many pairs 
 *                                                     will appear in each line, 
 *                                                     similarly for more that 
 *                                                     2 arrays
 *    orgFormatString            I              char*  C printf format string:
 *                                                     if in fortran we have 
 *                                                     F10.3 then pass %10.3f
 *                                                     if in fortran we have 
 *                                                     I4 then pass %4d
 *    startIndex                 I              int*   start index for printing
 *                                                     the array
 *    endIndex                   I              int*   end index for stopping 
 *                                                     the array print
 *    incrValue                  I              int*   value to be stepped thru 
 *                                                     while printing the array
 *    ...                        I                     multiple arrays are 
 *                                                     accepted
 *
 * Return Value: None
 *           
 * Local Variables:
 *    Type      Name                         Description
 *
 * Global Variables Used:
 *    Type      Name             Origin      Description
 *
 * Constant and Macro Substitutions:
 *  Name        Header File                  Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
void logonedimensionarrayfromfortran_(int *level, char *orgUserMessage,
                                    int *numOfArraysPassed, 
				    int *numOfValuesDisplayedInALine,
                                    char orgFormatString[SIZE_OF_FORMAT_STRING],
                                    int *startIndex, int *endIndex, 
				    int *incrValue, ...)
{
   char *message = NULL;

   /* Open diagnostic file "diag.txt" */
   openDiagFile();   

   va_list arg_ptr;
   va_start(arg_ptr, incrValue);

   logArrays(*level, diagFilePtr, orgUserMessage, *numOfArraysPassed, 
             *numOfValuesDisplayedInALine, orgFormatString,
             *startIndex, *endIndex, *incrValue, arg_ptr);

   memset(orgFormatString, 0, sizeof(orgFormatString));
   memset(orgUserMessage, 0, sizeof(orgUserMessage));

   va_end(arg_ptr);

   exitOnLevel(*level);

}/* logonedimensionarrayfromfortran_() ------------------------------------- */

/******************************************************************************
 * Module Name: determineType()
 *
 * Original Author: Varalakshmi Rajaram 
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *   accepts C format string and returns the type 
 *   ex : accepts the format string like %10.2f and returns the type float
 *
 * Calling Arguments:
 *
 *    Name         Input/Output        Type       Description
 *    formatString I                   char*      c formatstring        
 *
 * Return Value:  the datatype either int or float or nulL
 *           
 * Local Variables:
 *   Type      Name                               Description
 *
 * Global Variables Used:
 *    Type     Name                    Origin     Description
 *
 * Constant and Macro Substitutions:
 *  Name                   Header File            Description
 *  SIZE_OF_FORMAT_STRING  logging.h              indicates 7
 * Modification History:
 *           Date           Developer        Action
 *
 ****************************************************************************/

char* determineType(char* formatString)
{
   char *type = (char*) malloc(sizeof(char) *SIZE_OF_FORMAT_STRING +1);
  
   memset(type, 0, sizeof(type));
  
   if ( strchr(formatString, 'f') != NULL )
   {
      strcpy(type, "float");
   }
   else if ( strchr(formatString, 'd') != NULL )
   {
      strcpy(type, "int");
   }

   return type;
   
} /* determineType() ------------------------------------------------------- */

/******************************************************************************
 * Module Name: logArrays()
 *
 * Original Author: Varalakshmi Rajaram 
 *
 * Module Creation Date: 05/05/08
 *
 * Description:
 * This functions is called within logonedimensionarrayfromfortran_
 * This function determines the data type of the array and calls appropriate
 * function to write the arrays in log file
 *
 * Calling Arguments:
 *
 *    Name                      Input/Output    Type       Description
 *    level                      I              int    INFO_LEVEL/FATAL_LEVEL/
 *                                                     DEBUG_LEVEL/WARNING_LEVEL
 *    diagFilePtr                I              FILE*  pointer to diag.txt file
 *    orgUserMessage             I              char*  user message to log
 *    numOfArraysPassed          I              int*   number of arrays to logg
 *    numOfValuesRequestedInALineI              int*   number of values to be 
 *                                                     displayed in each row, 
 *                                                     if there are 2 arrays 
 *                                                     passed then this number 
 *                                                     denoted how many pairs 
 *                                                     will appear in each line, 
 *                                                     similarly for more that 
 *                                                     2 arrays
 *    orgFormatString            I              char*  C printf format string:
 *                                                     if in fortran we have 
 *                                                     F10.3 then pass %10.3f
 *                                                     if in fortran we have 
 *                                                     I4 then pass %4d
 *    startIndex                 I              int*   start index for printing
 *                                                     the array
 *    endIndex                   I              int*   end index for stopping 
 *                                                     the array print
 *    incrValue                  I              int*   value to be stepped thru 
 *                                                     while printing the array
 *    arg_ptr                    I             va_list var args list
 *
 *
 * Return Value: None
 *           
 * Local Variables:
 *   Type      Name                            Description
 *
 * Global Variables Used:
 *    Type     Name              Origin        Description
 *   float**   	floatValues		      	Double Array Float to store
 *					      	number of arrays passed in
 *   char*	finalValueDisplayFormatString	
 *   int 	valueWidthToUse			size of string
 *   int	numOfValuesToDisplayInALine	number of strings to display in a line
 *   int 	count				counting index array
 *   int   	arrayIsNullFlag			flag to check for a null array
 *
 * Constant and Macro Substitutions:
 *  Name       Header File                     Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 ****************************************************************************/

void logArrays(int level, FILE *diagFilePtr,
              char *orgMessage, int numOfArraysPassed,
              int numOfValuesRequestedInALine, char *orgFormatString,
              int startIndex, int endIndex, int incrValue, va_list arg_ptr)
{
   float **floatValues = NULL;
   int   **intValues = NULL;
   char  **charValues = NULL;

   char  *finalValueDisplayFormatString;
   int   arrayIsNullFlag = 0;

   int   valueWidthToUse, numOfValuesToDisplayInALine;
   int   count;

   char *type = determineType(orgFormatString);

   if (strcmp(type, "float") == 0)
   {
      floatValues = (float**) calloc( numOfArraysPassed, sizeof(float*) );
      
      if ( floatValues == NULL )
      {
         fprintf(diagFilePtr, 
         "%d| Unable to allocate memory for logging float array values\n",
	 WARNING_LEVEL);
         free(type);
         free(floatValues);
         return;   
      }
     
      count = 0;
     
      while ( count < numOfArraysPassed )
      {
         floatValues[count++] = va_arg(arg_ptr, float*);
      }
     
      if ( floatValues[0] == NULL ) 
      {
	 arrayIsNullFlag = 1;
      }
   }
   else if ( strcmp(type, "int") == 0 )
   {
      intValues = (int**) calloc( numOfArraysPassed, sizeof(int*) );
      
      if ( intValues == NULL )
      {
         fprintf(diagFilePtr, 
	 "%d| Unable to allocate memory for logging float array values\n",
	 WARNING_LEVEL);
         free(type);
         return;  
      }
    
      count = 0;
     
      while ( count < numOfArraysPassed )
      {
         intValues[count++] = va_arg(arg_ptr, int*);
      }
    
      if ( intValues[0] == NULL ) 
      {
	 arrayIsNullFlag = 1;
      }
   }
   else
   {
      fprintf(diagFilePtr, 
      "%d| Unrecognized format string [%s] for logging arrays\n", 
      WARNING_LEVEL, orgFormatString);
         free(type);
      return; 
   }

   valueWidthToUse = getValueWidthToUse(type, orgFormatString);

   numOfValuesToDisplayInALine = getNumOfValuesToDisplayInALine(
	                                          numOfValuesRequestedInALine);

   char *message = getAppropriateUserMessage(orgMessage, valueWidthToUse, 
	                                     numOfValuesToDisplayInALine);

   /* Values array is null, if so write the info to log */ 
   if ( arrayIsNullFlag == 1 ) 
   {
      logUserMessageSinceArrayIsNull(message);
      free(type);

      if(message != NULL)
         free(message);

      if(floatValues != NULL)
         free(floatValues);

      return;
   }

   char *formatString = getAppropriateFormatString(type, orgFormatString);

   if ( strcmp(type, "float") == 0 )
   {
      writeArraysToLogFile(level, type, (void**)floatValues, formatString, 
 	                   message, numOfArraysPassed, 
			   numOfValuesToDisplayInALine, 
			   startIndex, endIndex, incrValue);
      if(floatValues != NULL)
         free(floatValues);
   }
   else if ( strcmp(type, "int") == 0 )
   {
      writeArraysToLogFile(level, type, (void**)intValues, formatString, 
	                   message, numOfArraysPassed, 
			   numOfValuesToDisplayInALine,
                           startIndex, endIndex, incrValue);
   }
   
   if(message != NULL)
     free(message);

    if(formatString != NULL)
      free(formatString);

    free(type);

} /* logArrays() ----------------------------------------------------------- */

/******************************************************************************
 * Module Name: logUserMessageSinceArrayIsNull() 
 *
 * Original Author: Varalakshmi Rajaram
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 * This function is called by logArrays to log just the user message if the 
 * arrays to log that are passed from fortran are null.
 * Calling Arguments:
 *
 *    Name            Input/Output    Type     Description
 *    orgUserMessage  I               char*    user message to log
 *
 * Return Value: None
 *           
 * Local Variables:
 *   Type      Name                            Description
 *
 * Global Variables Used:
 *    Type     Name                   Origin   Description
 *
 * Constant and Macro Substitutions:
 *  Name       Header File                     Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void logUserMessageSinceArrayIsNull(char *orgUserMessage)
{
   char *str = "Attemp to print an array that is null";
   char finalStr[MAX_LOG_LENGTH];
   int  sizeToTrim;

   /* 5 is a number chosen to accomodate level, pipe and -- */
   if ( (strlen(orgUserMessage) + strlen(str) + 5) > MAX_LOG_LENGTH )
   {
      sizeToTrim =  strlen(orgUserMessage)- strlen(str) - 5;
      strncpy(finalStr, orgUserMessage, sizeToTrim);
      finalStr[sizeToTrim] = '\0';
   }
   else
   {
      strcpy(finalStr, orgUserMessage);
   }

   fprintf(diagFilePtr,"%d|%s --%s\n",WARNING_LEVEL, orgUserMessage, 
	               "Attemp to print an array that is null");

} /* logUserMessageSinceArrayIsNull() -------------------------------------- */


/*****************************************************************************
 * Module Name: determineTotalLengthUsedByValuesInALine()
 *
 * Original Author: Varalakshmi Rajaram
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *  This function is called by getAppropriateUserMessage to find the total 
 *  length used by just the values in the line
 *
 * Calling Arguments:
 *
 *    Name                Input/Output   Type    Description
 *    valueWidth          In             int     Total width for printing 
 *						 the value
 *    numOfValuesInALine  In             int     Number of values to be
 *                                               logged in a line
 *
 * Return Value: message length
 *   ( numOfValuesInALine * valueWidth )
 *           
 * Local Variables:
 *   Type      Name                              Description
 *
 * Global Variables Used:
 *    Type     Name        Origin                Description
 *
 * Constant and Macro Substitutions:
 *  Name       Header File                       Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
int determineTotalLengthUsedByValuesInALine(int valueWidth, 
                                            int numOfValuesInALine)
{
   int result = numOfValuesInALine * valueWidth ;

   return result;
   
} /* determineTotalLengthUsedByValuesInALine() ----------------------------- */


/******************************************************************************
 * Module Name: isLogLineLengthHasExceeded()
 *
 * Original Author: Varalakshmi Rajaram
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *  This function is called by getAppropriateUserMessage to determine whether
 *  the length of the user message along with the length occupied by the values
 *  is exceeding a log line's length (MAX_LOG_LENGTH).
 *
 * Calling Arguments:
 *
 *    Name       Input/Output        Type      Description
 *    length     I                   int       line length  
 *
 * Return Value: 
 *   1 - if length is exceeded
 *   0 - if length is not exceeded
 *           
 * Local Variables:
 *   Type        Name                          Description
 *
 * Global Variables Used:
 *    Type       Name            Origin        Description
 *
 * Constant and Macro Substitutions:
 *  Name        Header File                    Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

int isLogLineLengthHasExceeded(int length)
{
   if ( length > MAX_LOG_LENGTH )
   {
      return 1;
   }
   else
   {
      return 0;
   }

} /* isLogLineLengthHasExceeded() ------------------------------------------ */


/******************************************************************************
 * Module Name: isAllBlank()
 *
 * Original Author: Varalakshmi Rajaram
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *  This function is called by getAppropriateUserMessage to determine whether
 *  message sent from fortran is an blank message
 *
 * Calling Arguments:
 *
 *    Name       Input/Output        Type       Description
 *    message    I                   char*      user message to log
 *
 * Return Value: 
 *           0 if message is not blank
 *           1 if message is blank
 *
 * Local Variables:
 *   Type      Name                             Description
 *
 * Global Variables Used:
 *    Type     Name                  Origin     Description
 *
 * Constant and Macro Substitutions:
 *  Name       Header File                      Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 ****************************************************************************/
int isAllBlank(char* message)
{
   int loopCnt;
   int result = 1;
 
   if ( message != NULL )
   {
      loopCnt = 0;
      for ( loopCnt = strlen(message) -1 ; loopCnt >=0 ; loopCnt++ )
      {
         if ( message[loopCnt] != ' ' && message[loopCnt] != 0 )
         {
            result = 0;
            break;
         }
      }
   }
   
   return result;
   
} /* isAllBlank() ---------------------------------------------------------- */

/******************************************************************************
 * Module Name: getAppropriateUserMessage()
 *
 * Original Author: Varalakshmi Rajaram
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *  This function is called by logArrays to trim the user message if needed.
 *  The decision to trim and how much of the message is to be trimmed is 
 *  determined by the value width and number of values to be logged in a line.
 *
 * Calling Arguments:
 *
 *    Name                Input/Output   Type    Description
 *    orgUserMessage      I              char*   User message to log
 *    valueWidth          I              int     Total width used to log 
 *                                               the value
 *    numOfValuesInALine  In             int     Number of values to be
 *                                               logged in a line
 * 
 * Return Value: the message string 
 *           
 * Local Variables:
 *   Type      Name                              Description
 *
 * Global Variables Used:
 *    Type     Name                      Origin  Description
 *
 * Constant and Macro Substitutions:
 *  Name       Header File                       Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
char* getAppropriateUserMessage(char* orgUserMessage, int valueWidth, 
                                int numOfValuesInALine)
{
   char *returnMessage = NULL;
   int  numOfValuesDisplayed,
        numOfSpacesBetweenValues,
        lengthOfLineWithValuesAndSpacesBetweenThem;
   int  i, sizeToTrim;

   /* Check if orgUserMessage is null - set it to empty string */
   if ( orgUserMessage == NULL || isAllBlank(orgUserMessage) )
   {
      returnMessage = (char*) calloc(2, sizeof(char));
      returnMessage[0] = ' ';
      returnMessage[1] = '\0';
   }
   else /* check if orgUserMessage has many characters, if so trim it*/
   {
      i = 0;
      for ( i = strlen(orgUserMessage) -1 ; i >= 0 ; i++ )
      {
         if ( (orgUserMessage[i] != ' ') && (orgUserMessage[i] != 0) )
         {
            if ( i == strlen(orgUserMessage) )
	    {
               orgUserMessage[i] = 0;
	    }
            else
	    {
               orgUserMessage[i+1] = 0;
	    }
         }
      }
     
      numOfValuesDisplayed = determineTotalLengthUsedByValuesInALine( 
	                              valueWidth, numOfValuesInALine);

      numOfSpacesBetweenValues = numOfValuesInALine;

      lengthOfLineWithValuesAndSpacesBetweenThem = numOfValuesDisplayed + 
	                                           numOfSpacesBetweenValues;

     if (isLogLineLengthHasExceeded(lengthOfLineWithValuesAndSpacesBetweenThem +
	                            strlen(orgUserMessage)))
     {
       /* 4 is a random number chosen to accomodate the level and pipe 
	* delimiter
        */	
        sizeToTrim =  strlen(orgUserMessage) - 
	                 (lengthOfLineWithValuesAndSpacesBetweenThem + 4); 

        returnMessage = (char*) calloc( MAX_LOG_LENGTH, sizeof(char));

        memset(returnMessage, 0, sizeof(returnMessage));
        strncpy(returnMessage, orgUserMessage, sizeToTrim);
        returnMessage[sizeToTrim] = '\0';

        fprintf(diagFilePtr,"%d|%s \n", WARNING_LEVEL, 
	       "User Message is trimmed since it exceeded the character limit");
     }
     else
     {
        returnMessage = (char*) calloc(strlen(orgUserMessage) , sizeof(char));
        memset(returnMessage, 0, sizeof(returnMessage));
        strcpy(returnMessage, orgUserMessage);
     }
   }

   return returnMessage;

} /* getAppropriateUserMessage() ------------------------------------------- */


/******************************************************************************
 * Module Name: getValueWidth()
 *
 * Original Author: Varalakshmi Rajaram
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *  This function is called by getValueWidthToUse. If the passed in valueWidth
 *  is exceeding a default limit then the default value will be used.
 *
 * Calling Arguments:
 *
 *    Name        Input/Output        Type       Description
 *    valueWidth  I                   int        Total width to use to log value
 *
 * Return Value: 
 *   defaul value width (MAX_VALUE_WIDTH_FOR_DISPLAY) if the input is exceeding
 *   else the same input is returned as output
 *           
 * Local Variables:
 *   Type         Name                           Description
 *
 * Global Variables Used:
 *   Type         Name                Origin     Description
 *
 * Constant and Macro Substitutions:
 *   Name                         Header File    Description
 *   MAX_VALUE_WIDTH_FOR_DISPLAY  logging.h
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

int getValueWidth(int valueWidth)
{
   int returnWidth = valueWidth;

   if ( valueWidth < 0 || valueWidth > MAX_VALUE_WIDTH_FOR_DISPLAY )
   {
      returnWidth = MAX_VALUE_WIDTH_FOR_DISPLAY;
   }
   return valueWidth;

} /* getValueWidth() ------------------------------------------------------- */


/******************************************************************************
 * Module Name: getNumOfValuesToDisplayInALine()
 *
 * Original Author: Varalakshmi Rajaram
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *  This function is called by logArrays to find the number of values to 
 *  display in a line. The input is compared with a preset default.
 *
 * Calling Arguments:
 *
 *    Name                        Input/Output Type  Description
 *    numOfValuesRequestedInALine I            int*  number of values to be 
 *                                                   displayed in each row, 
 *                                                   if there are 2 arrays 
 *                                                   passed then this number 
 *                                                   denoted how many pairs 
 *                                                   will appear in each line,
 *                                                   similarly for more that 
 *                                                   2 arrays
 *
 * Return Value: 
 *   default (MAX_NUM_OF_VALUES_IN_A_LINE) if input exceeds this value
 *   if not return the input itself
 *
 * Local Variables:
 *   Type      Name                                Description
 *
 * Global Variables Used:
 *   Type      Name            Origin              Description
 *
 * Constant and Macro Substitutions:
 *   Name                         Header File      Description
 *   MAX_NUM_OF_VALUES_IN_A_LINE  logging.h        indicates 15
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
int getNumOfValuesToDisplayInALine(int numOfValuesRequestedInALine)
{
   int returnValue = numOfValuesRequestedInALine;

   if ( numOfValuesRequestedInALine < 0 || 
        numOfValuesRequestedInALine > MAX_NUM_OF_VALUES_IN_A_LINE )
   {
      returnValue = MAX_NUM_OF_VALUES_IN_A_LINE;
   }
   return returnValue;
   
} /* getNumOfValuesToDisplayInALine() -------------------------------------- */


/*****************************************************************************
 * Module Name: getValueWidthToUse()
 *
 * Original Author: Varalakshmi Rajaram
 *
 * Module Creation Date:
 *
 * Description:
 *  This function is called by logArrays to determine the value width to use
 *  for logging the array values passed from fortran.
 *
 * Calling Arguments:
 *
 *    Name             Input/Output  Type     Description
 *    type             I             char*    int or float
 *    orgFormatString  I             char*    format string passed from fortran
 *
 * Return Value: the width 
 *           
 * Local Variables:
 *   Type      Name                           Description
 *
 * Global Variables Used:
 *   Type     Name            Origin          Description
 *
 * Constant and Macro Substitutions:
 *   Name                         Header File Description
 *   MAX_VALUE_WIDTH_FOR_DISPLAY  logging.h   indicates 12
 *   SIZE_OF_FORMAT_STRING        logging.h   indicates 7
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

int getValueWidthToUse(char *type, char *orgFormatString)
{
   int valueWidthToUse = MAX_VALUE_WIDTH_FOR_DISPLAY;
   char *tempFormatString = (char*) calloc (SIZE_OF_FORMAT_STRING +1, sizeof(char)); 
  
   strcpy(tempFormatString, orgFormatString+1); /* exclude the letter % */

   if ( strcmp(type, "float") == 0 )
   {
      char *splitStr = (char*) strtok(tempFormatString, ".");
      
      if ( splitStr != NULL )
      {
         valueWidthToUse = getValueWidth(atoi(splitStr));
      }
   }
   else if ( strcmp(type, "int") == 0 )
   {
      char *splitStr = (char*) strtok(tempFormatString, "d");
      
      if ( splitStr != NULL )
      {
         valueWidthToUse = getValueWidth(atoi(splitStr));
      }
   }
 
   if(tempFormatString !=NULL)
     free(tempFormatString);

   return valueWidthToUse;

} /* getValueWidthToUse() -------------------------------------------------- */

/******************************************************************************
 * Module Name: getAppropriateFormatString()
 *
 * Original Author: Varalakshmi Rajaram
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *  This function is called by logArray to get the format string to use to
 *  log the values in a line, based on the original format string passed by
 *  the user.
 * Calling Arguments:
 *
 *    Name             Input/Output  Type     Description
 *    type             I             char*    int or float
 *    orgFormatString  I             char*    format string passed by user
 *
 * Return Value: the format string 
 *           
 * Local Variables:
 *   Type      Name                           Description
 *
 * Global Variables Used:
 *    Type     Name                  Origin   Description
 *
 * Constant and Macro Substitutions:
 *  Name                    Header File       Description
 *  SIZE_OF_FORMAT_STRING   logging.h
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

char* getAppropriateFormatString(char* type, char *orgFormatString)
{
   int valuePrecision, indexOfPeriod;
   int indexOfFormatCharF, indexOfStr, indexOfTrimmedFormatString;
   char str[2];
   char *trimmedFormatString = NULL;
   
   if(orgFormatString != NULL)
   {
       trimmedFormatString = trimString(orgFormatString);

   }

   memset(str, '\0', 2);

   char *returnFormatString = (char*) calloc((SIZE_OF_FORMAT_STRING +1), 
	                                     sizeof(char));

   if(type == NULL)
   {
      sprintf(returnFormatString,"%s ",trimmedFormatString);
      return returnFormatString;
   }

   if ( strcmp(type, "float") == 0 )
   {
     indexOfPeriod = getFirstIndex(trimmedFormatString, '.');

     if ( indexOfPeriod != -1 )
     {
        indexOfFormatCharF = getFirstIndex(trimmedFormatString, 'f');
        indexOfTrimmedFormatString  = indexOfPeriod + 1;
       
        for (indexOfStr=0; indexOfStr<strlen(trimmedFormatString); indexOfStr++)
        {
           if ( trimmedFormatString[indexOfTrimmedFormatString] != 'f' )
	   {
           str[indexOfStr] = trimmedFormatString[indexOfTrimmedFormatString++];
	   }
        }
       
        valuePrecision = atoi(str);
	
        if ( valuePrecision == 0 )
        {
	   sprintf(returnFormatString,"%s. ",trimmedFormatString);
        }
        else
        {
	   sprintf(returnFormatString,"%s ",trimmedFormatString);
        }
     }
   }
   else
   {
      sprintf(returnFormatString,"%s ",trimmedFormatString);
   }

   if(trimmedFormatString != NULL)
      free(trimmedFormatString);

   return returnFormatString;

} /* getAppropriateFormatString() ------------------------------------------ */

/*****************************************************************************
 * Module Name: getFirstIndex()
 *
 * Original Author: Varalakshmi Rajaram
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *   This function determines the index of the first occurence of the 
 *   the character in a string.
 * Calling Arguments:
 *
 *    Name       Input/Output        Type       Description
 *    c          I                   char       character to search
 *    str        I                   char*      string 
 *
 * Return Value: the first index of character c in string str.
 *           
 * Local Variables:
 *   Type      Name                             Description
 *
 * Global Variables Used:
 *   Type     Name            Origin            Description
 *
 * Constant and Macro Substitutions:
 *   Name     Header File                       Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

int getFirstIndex(char* str, char c)
{
   int count;
   int index = -1;
    
   if ( str != NULL )
   {
      count = 0;
      for ( count = 0; count < strlen(str) ; count++ )
      {
         if ( str[count] == c ) 
         {
            index = count;
            break;
         }
      }
   }
   
   return index;

} /* getFirstIndex() ------------------------------------------------------- */


/******************************************************************************
 * Module Name: trimString()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *  Removed all spaces in the string.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output        Type       Description
 *    str        I                   char*      string
 *
 * Return Value: the string without any spaces
 *           
 * Local Variables:
 *   Type      Name                             Description
 *
 * Global Variables Used:
 *    Type     Name            Origin           Description
 *
 * Constant and Macro Substitutions:
 *  Name    Header File                         Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

char* trimString(char* str)
{
   int  indexOfStr;
   int  indexOfTempStr;
   char *tempStr = NULL;

   if ( str != NULL )
   {
      tempStr = (char*) calloc((strlen(str)+1) , sizeof(char));
      indexOfTempStr = 0;

      for ( indexOfStr = 0;  indexOfStr<strlen(str); indexOfStr++ )
      {
         if ( str[indexOfStr] != ' ' )
	 {
            tempStr[indexOfTempStr++] = str[indexOfStr];
	 }
      }
      tempStr[indexOfTempStr]='\0';
   }
   
   return tempStr;

} /* trimString() --------------------------------------------------------- */
 
/*****************************************************************************
 * Module Name: writeArraysToLogFile()
 *
 * Original Author: Varalakshmi Rajaram
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *   This function is called by logArrays, to do the actual writing of
 *   message and array values to the log file.
 *
 * Calling Arguments:
 *
 *    Name                       Input/Output  Type   Description
 *
 *    level                      I             int    INFO_LEVEL/FATAL_LEVEL/
 *                                                    DEBUG_LEVEL/WARNING_LEVEL
 *    type                       I             char*  int or float
 *    formatString               I             char*  format string to print 
 *                                                    values in log file
 *    message                    I             char*  message to log
 *    numOfArraysPassed          I             int*   number of arrays to log
 *    numOfValuesToDisplayInALineI             int*   number of values to be 
 *                                                    displayed in each row
 *    startIndex                 I             int*   start index for printing
 *                                                    the array
 *    endIndex                   I             int*   end index for stopping 
 *                                                    the array print
 *    incrValue                  I             int*   value to be stepped thru 
 *                                                    while printing the array
 * Return Value: None
 *           
 * Local Variables:
 *   Type      Name                         Description
 *
 * Global Variables Used:
 *   Type     Name            Origin        Description
 *
 * Constant and Macro Substitutions:
 *   Name     Header File                   Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void writeArraysToLogFile(int level, char* type, void **valuesPassed,  
                          char* formatString, char* message, 
                          int numOfArraysPassed,
                          int numOfValuesToDisplayInALine, int startIndex, 
			  int endIndex, int incrValue)
{ 
   int  rowCount, loopCnt;
   int  index;
   char tempStr[1024];
   char str[MAX_VALUE_WIDTH_FOR_DISPLAY];
   char lineToDisplay [MAX_LOG_LENGTH];
   float **floatValues = NULL;
   int  **intValues = NULL;

   if ( strcmp(type, "float") == 0 )
   {
      floatValues = (float**) valuesPassed;
   }
   else if ( strcmp(type, "int") == 0 )
   {
      intValues = (int**) valuesPassed;
   }
   
   if ( incrValue == 0 ) 
   {
      incrValue = 1;  /* if user specifies 0 then assume 1 */
   }
   
   loopCnt = startIndex-1;

   while ( 1 )
   {
      if ( breakCondition(loopCnt, incrValue, endIndex-1) )
      {
         break;
      }
      
      memset(lineToDisplay, 0, sizeof(lineToDisplay));

      sprintf(lineToDisplay, "%d|%s ", level, message);

      for ( rowCount = 1; rowCount <= numOfValuesToDisplayInALine; rowCount++ )
      {
         memset(tempStr, 0, sizeof(tempStr));

         if ( breakCondition(loopCnt, incrValue, endIndex-1) )
	 {
            break;
	 }
	 
         index = 0;

         while ( index < numOfArraysPassed )
         {  
         
	    memset(str, 0, sizeof(str));
       
            /* fprintf(diagFilePtr, " [%d][%d]", index, (loopCnt)); */

            if ( strcmp(type,"float") == 0 )
	    {
               sprintf( str, formatString, *(floatValues[index] + (loopCnt)));
	    }
            else  /* its an int */
	    {
               sprintf( str, formatString, *(intValues[index] + (loopCnt)));
	    }

            if ( index == 0 ) 
	    {
               strcpy(tempStr, str);  
	    }
            else 
	    {
               strcat(tempStr, str);
	    }

            index++;
         } 

         loopCnt += incrValue;
         strcat(lineToDisplay, tempStr);
      }

      if ( rowCount == 1 )  
      {
         fprintf(diagFilePtr,"\n%s\n", lineToDisplay);
      }
      else
      {
         fprintf(diagFilePtr,"%s\n", lineToDisplay);
      }
   } 
   
} /* writeArraysToLogFile() ------------------------------------------------ */


/******************************************************************************
 * Module Name: breakCondition()
 *
 * Original Author: Varalakshmi Rajaram
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *  This function is called by writeArraysToLogFile, to determine the break
 *  condition to print the arrays in the loop.
 *
 * Calling Arguments:
 *
 *    Name        Input/Output    Type       Description
 *    loopCnt      I              int        loop count
 *    incrValue    I              int        increment value
 *    endIndex     I              int        end index value
 *
 * Return Value: the result flag 1 or 0
 *           
 * Local Variables:
 *   Type      Name                          Description
 *
 * Global Variables Used:
 *   Type     Name            Origin         Description
 *
 * Constant and Macro Substitutions:
 *   Name     Header File                    Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

int breakCondition(int loopCnt, int incrValue, int endIndex)
{
   int result = 0;

   /* in +ve steps loop, if endindex is reached bail out  or
    * in -ve steps loop, if endIndex is reached bail out
    */
   if ( ((incrValue > 0) && loopCnt > endIndex) ||
        ((incrValue < 0) && loopCnt < endIndex) )
   {
      result = 1;
   }
 
   return result;

} /* breakCondition() ------------------------------------------------------ */


/******************************************************************************
 * Module Name: setDiagFileName()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *   Sets the diagnostics file name
 *
 * Calling Arguments:
 *
 *    Name       Input/Output        Type    Description
 *    filename   I                   char*   file name
 *
 * Return Value: None
 *           
 * Local Variables:
 *   Type      Name                          Description
 *
 * Global Variables Used:
 *    Type     Name            Origin        Description
 *    char[]    diagFileName                    
 *
 * Constant and Macro Substitutions:
 *    Name    Header File                    Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void setDiagFileName(char *fileName)
{   
   strcpy(diagFileName, fileName);
   
   diagFileName[strlen(diagFileName)] = '\0';

} /* setDiagFileName() ----------------------------------------------------- */

/******************************************************************************
 * Module Name: getDiagFileName()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *   Returns the diagnostics file name
 *
 * Calling Arguments:
 *
 *    Name     Input/Output      Type     Description
 *
 * Return Value: the diagnostics file name 
 *           
 * Local Variables:
 *   Type      Name                       Description
 *
 * Global Variables Used:
 *   Type     		 Name              Origin   Description
 * static char[]	 diagFileName		    Diagnostic file name
 * Constant and Macro Substitutions:
 *   Name     Header File                 Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 ****************************************************************************/

char *getDiagFileName()
{
   return diagFileName;

} /* getDiagFileName() ----------------------------------------------------- */


/*****************************************************************************
 * Module Name: openDiagFile()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *   Opens diagnostics file.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output        Type    Description
 *
 * Return Value: None 
 *           
 * Local Variables:
 *   Type      Name                          Description
 *
 * Global Variables Used:
 *    Type     		Name            Origin        Description
 *  
 * FILE*	 diagFilePtr		    	  diagnostic file pointer
 * static char[] diagFileName		    	  diagnostic file name
 * Constant and Macro Substitutions:
 *  Name       Header File                   Description
 *  FAILURE    logging.h
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
void openDiagFile()
{
   if ( diagFilePtr == NULL )
   {
      diagFilePtr = fopen(getDiagFileName(), "a");
      
      /* Since we don't have a diagnostics file to log to , just exit */
      if ( diagFilePtr == NULL )
      {
         fprintf(stderr, "Couldn't open diagnostics file:[%s]\n", getDiagFileName());
         exit(FAILURE);	
      }
   }
} /* openDiagFile() -------------------------------------------------------- */

/******************************************************************************
 * Module Name: closeDiagFileAndExit()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *  Close diagnostics file
 *
 * Calling Arguments:
 *
 *    Name       Input/Output     Type       Description
 *    code       I                int         
 *
 * Return Value: None
 *           
 * Local Variables:
 *   Type      Name                          Description
 *
 * Global Variables Used:
 *    Type             Name            Origin        Description
 * static char[]     diagFilePtr		     diagnostic file
 * Constant and Macro Substitutions:
 *  Name        Header File                  Description
 *  FAILURE     logging.h
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void closeDiagFileAndExit(int code)
{
   if ( diagFilePtr != NULL )
   {
      if ( fclose(diagFilePtr) == EOF )
      {
         exit(FAILURE);	
      }
      else
      {
         exit(code);
      }
   }
   else
   {
      exit(code);
   }
} /* closeDiagFileAndExit() ------------------------------------------------ */


/******************************************************************************
 * Module Name: findNumberOfLinesInLogMessageFromFortran()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *  This function is called by logfromfortran_ to determine the number of line
 *  in the two dimensional character array passed from fortran.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output    Type      Description
 *    message    I               char[][]  multiple line user message 
 *
 * Return Value: the number of line 
 *           
 * Local Variables:
 *   Type      Name                        Description
 *
 * Global Variables Used:
 *    Type     Name            Origin      Description
 *
 * Constant and Macro Substitutions:
 *  Name             Header File           Description
 *  MAX_LOG_LINES    logging.h             indicates 20
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

int findNumberOfLinesInLogMessageFromFortran(char message[][MAX_LOG_LENGTH])
{
   int i = 0;
   int j = MAX_LOG_LINES;
   int length = 0;
   int height = 0;

   for ( j = MAX_LOG_LINES - 1; j >= 0; j-- )
   {
      length = getLengthAndNullTerminateLogMessageFromFortran(message[j]);

      if ( length > 0 )
      {
         height = j + 1;
         break;
      }
   }
   
   return height;

} /* findNumberOfLinesInLogMessageFromFortran() ---------------------------- */
  
/*****************************************************************************
 * Module Name: getLengthAndNullTerminateLogMessageFromFortran()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *  This function is called by findNumberOfLinesInLogMessageFromFortran to 
 *  determine whether a line is empty, if not return the length of the line.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output        Type    Description
 *    mLine      I                   char* 
 *
 * Return Value: 
 *    the length of line if line is not empty 
 *    0 if line is empty
 *
 * Local Variables:
 *   Type      Name                          Description
 *
 * Global Variables Used:
 *    Type     Name            Origin        Description
 *
 * Constant and Macro Substitutions:
 *  Name             Header File             Description
 *  MAX_LOG_LENGTH   logging.h
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

int getLengthAndNullTerminateLogMessageFromFortran(char *mLine)
{
   int i;
   int found = 0;

   /* More efficient to just check here rather than figure it out
    * going through the whole line
    */
   if ( strlen(mLine) == 0 )
   {
      return 0;
   }

   /* In c the length of message should be 513 */
   for ( i = MAX_LOG_LENGTH-1; i >= 0; i-- )
   {
      if ( (mLine[i] != ' ') && (mLine[i] != 0) )
      {
         mLine[i+1] = 0;
         found = 1;
         break;
      }
   }
 
   /* Empty line */
   if ( found == 0 )
   {
      mLine[0] = 0;
      i = 0;
   }

   return i;
   
} /* getLengthAndNullTerminateLogMessageFromFortran() ---------------------- */

/*****************************************************************************
 * Module Name: exitOnLevel()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *   If the level is FATAL_LEVEL the diagnostics file is closed and the process 
 *   is exited.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output    Type    Description
 *    level      I               int        
 *
 * Return Value: None
 *           
 * Local Variables:
 *    Type       Name                    Description
 *
 * Global Variables Used:
 *    Type       Name            Origin  Description
 *
 * Constant and Macro Substitutions:
 *    Name           Header File         Description
 *    FATAL_LEVEL    logging.h           indicates 0
 *    FAILURE        logging.h           indicates 1
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void exitOnLevel(int level)
{
   if ( level == FATAL_LEVEL )
   {
      closeDiagFileAndExit(FAILURE);
   }
   
} /* exitOnLevel() --------------------------------------------------------- */

void closeDiagFile()
{
   if(diagFilePtr != NULL)
      fclose( diagFilePtr );
}
