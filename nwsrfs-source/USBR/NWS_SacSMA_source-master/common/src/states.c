/*.............................................................................
* File: states.c
* Author(s): CHPS/FEWS Team
* Date Created: 5/5/08

* Development group: OHD HSEB
*
* Purpose: various routines to read and write state data for
*	   legacy (fortran, c, and c++) models	 
*                                   
* Module(s): 
*     readStatesFromFile
*     populateIntStateValue
*     populateFloatStateValue
*     populateStringStateValue
*     populateArrayOfStringStateValues 
*     populateArrayOfFloatStateValues
*     populateArrayOfFloatStateValuesPair
*     populateArrayOfIntStateValues  
*     writeIntStateToFile 
*     writeFloatStateToFile 
*     writeStringStateToFile 
*     writeArrayOfIntStatesToFile
*     writeArrayOfFloatStatesToFile 
*     writeArrayOfFloatStatesPairsToFile
*                                
* Date Modified:                             
*
*............................................................................*/
/******************************************************************************
 * Module Name: readStatesFromFile()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *    Read state (carryover) from file. 
 *
 * Calling Arguments:
 *
 *    Name          Input/Output     Type      Description
 *    nStates       Out              int*      Number of state    
 *    
 *    stateUnits    Out              int*      state units   
 *
 * Return Value: None
 *           
 * Local Variables:
 *   Type           Name                       Description
 *
 * Global Variables Used:
 *   Type           		Name             	    Description
 *   static ModelState*    	states                     object ModelState structure  
 *   static int*   		numSates                   count number of states
 *
 * Constant and Macro Substitutions:
 *   Name           Header File                Description
 *   MAX_PROPERTIES states.h                   Maximun number of states
 *   DEBUG_LEVEL    logging.h
 *   FATAL_LEVEL    logging.h
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <math.h>
#include <string.h>

#include "states.h"
#include "logging.h"
#include "utilities.h"
#include "commonInclude.h"


static Properties *states = NULL; 
static int numStates = 0;
 
void readStatesFromFile(int *nStates, int *stateUnits)
{
   numStates = 0;

//   Properties* properties = (Properties *)
 //                         malloc(sizeof(Properties)*MAX_PROPERTIES);

   if ( checkFileEmptyStatus(getStateFileName()) == 0)
   {
       logMessageWithArgsAndExitOnError(FATAL_LEVEL,
	            "\n STATE File [%s] is empty"  , getStateFileName());
   } 

   states = (Properties *) calloc(MAX_PROPERTIES, sizeof(Properties));
   readPropertiesFromFile(nStates, 
                            getStateFileName(), states,
                             stateUnits);

   
   //memcpy(states, properties, sizeof(Properties)*MAX_PROPERTIES);

   int i;
   if(getFewsDebugFlag() > 0)
   {
     for(i=1; i < *nStates; i++)
     {
       logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
       "\n STATE PROPERTY [%s] value [%f]", states[i].name, 
       atof(states[i].value));
     }
   }

   *nStates = *nStates - 1;// Exclude the line for UNIT token
   numStates = *nStates;
   if(getFewsDebugFlag() > 0)
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL,"numStates= %d\n",
                                             numStates);
   }

}
/******************************************************************************
 * Module Name: populateIntStateValue()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *    Populate the integer state value.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output        Type      Description
 *    key        In                  char*     key name
 *    cArray     In                  float*    conatins carryover values
 *    index      In                  int       index 
 *                                             
 *
 * Return Value: the state value in integer
 *           
 * Local Variables:
 *   Type        Name                          Description
 *
 * Global Variables Used:
 *   Type        Name                Origin    Description
 *
 * Constant and Macro Substitutions:
 *   
 *   Name              Header File            Description
 * NON_EXISTENT_STATE  states.h		      missing carryover (-999)
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
int populateIntStateValue(char *key, float *cArray, int index)
{
   int i, value;
   value = NON_EXISTENT_STATE;
   for (i = 0; i < numStates; i++)
   {
      if (!strcmp(states[i].name, key))
      {
         value = atoi(states[i].value);

         if ( getFewsDebugFlag() > 0 )
	 {
            logMessageWithArgsAndExitOnError(DEBUG_LEVEL,"key=%s, value = %d\n",
	                                     key, value);
	 }
         /* The index is the FORTRAN index, we'll subtract 1 here */
         cArray[index-1] = (float)value;
	 return value;
      }
   }	
   
   logMessageWithArgsAndExitOnError(FATAL_LEVEL,
	     "state with key=%s could not be found\n",key);
   
   /* Just for the compiler - even though we're existing above */
   return value;
}

/******************************************************************************
 * Module Name: populateFloatStateValue()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *    Populates the state (carryover) value in floating point. 
 *
 * Calling Arguments:
 *
 *    Name       Input/Output    Type      Description
 *    key        In              char*     key of carryover
 *    cArray     In              float*    contains carryover values
 *    index      In              int       index
 *
 * Return Value: the state value in floating point 
 *           
 * Local Variables:
 *   Type        Name                      Description
 *
 * Global Variables Used:
 *   Type        Name            Origin    Description
 *   int         numStates                 number of states
 *
 * Constant and Macro Substitutions:
 *   Name                Header File       Description
 *   NON_EXISTENT_STATE  states.h          missing carryover (-999)
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
float populateFloatStateValue(char *key, float *cArray, int index)
{
   int i;
   float value = NON_EXISTENT_STATE;

   for (i = 0; i < numStates + 1; i++)
   {
      if (!strcmp(states[i].name, key))
      {
         value = atof(states[i].value);

         if ( getFewsDebugFlag() > 0 )
	 {
            logMessageWithArgsAndExitOnError(DEBUG_LEVEL,"key=%s, value = %f\n",
	                                     key, value);
	 }
         cArray[index-1] = (float)value;
         return value;
      }
   }
   
   logMessageWithArgsAndExitOnError(FATAL_LEVEL,
	     "state with key=%s could not be found\n",key);

   /* Just for the compiler - even though we're existing above */
   return value;
}

/******************************************************************************
 * Module Name: populateStringStateValue()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *    Populates the state value in string. 
 *
 * Calling Arguments:
 *
 *    Name       Input/Output   Type      Description
 *    key        In             char*     key carryover
 *    cArray     In             float*    contains carryover values
 *    index      In             int       index to populate
 *    length     In             int       length of string to populate
 *
 * Return Value: the state value in string 
 *           
 * Local Variables:
 *   Type        Name                     Description
 *
 * Global Variables Used:
 *   Type        Name           Origin    Description
 *   int         numStates                number of states
 *
 * Constant and Macro Substitutions:
 *   Name       Header File               Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

char* populateStringStateValue(char *key, float *cArray, int index, int length)
{
   int i;
   char *value = NULL;

   for (i = 0; i < numStates + 1; i++)
   {
      if (!strcmp(states[i].name, key))
      {
         value = states[i].value;
         strncpy((char *)(&cArray[index-1]), value, 
	         (length > strlen(value) ? strlen(value) : length));
      
         if ( getFewsDebugFlag() > 0 )
	 {
            logMessageWithArgsAndExitOnError(DEBUG_LEVEL,"key=%s, value = %s\n",
	                                     key, value);
	 }
         return value;
      }
   } 
   
   logMessageWithArgsAndExitOnError(FATAL_LEVEL,
	     "state with key=%s could not be found\n",key);
   
   /* Just for the compiler - even though we're existing above */
   return value;

} /* populateStringStateValue() -------------------------------------------- */

/******************************************************************************
 * Module Name: populateArrayOfStringStateValues()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *    Populates array of string state values.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output    Type      Description
 *    key        In              char*     key carryover
 *    numElementsInArray In      int       number element in carryover array
 *    cArray     In              float*    contains carryover values
 *    index      In              int       index 
 *
 * Return Value: the array state values in string format 
 *           
 * Local Variables:
 *   Type        Name                      Description
 *
 * Global Variables Used:
 *   Type        Name            Origin    Description
 *
 * Constant and Macro Substitutions:
 *   Name           Header File               Description
 *  STATE_KEY_SIZE  states.h		      maximum is 50 
 *  FATAL_LEVEL     logging.h                 logging level - Fatal error. Program will exit
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

char **populateArrayOfStringStateValues(char *key, int numElementsInArray, 
                                        float *cArray, int index)
{
   /* Instead of mallocing, realloc every time - array with at most 20 
    * elements
    */
   int i;
   char **arrayList = (char **) malloc(sizeof(char *)*numElementsInArray);
   int numElements = 0;
   int curKeyIndex = 0;
   char curKey[STATE_KEY_SIZE];
   char curIndexInAscii[4];

   /* We're looking for each one of the array keys separately so as not to 
    * assume a sorted order in the file.  It's the slowest way to do a sort 
    * essentially, but there are few elements in states, performance is not 
    * important here
    */
   sprintf(curKey,"%s#%d", key, curKeyIndex);

   for (i = 0; i < numStates + 1; i++)
   {
      if (!strcmp(states[i].name, curKey))
      {
         arrayList[numElements] = (char *)malloc(strlen(states[i].value)+1);
         strcpy(arrayList[numElements], states[i].value);

         /* Look for the next incremental key, but have to reset i now */
         sprintf(curIndexInAscii, "#%d", ++curKeyIndex);
	
         /* Reset the key and append next index */
         sprintf(curKey,"%s%s",key, curIndexInAscii);
         numElements++;
	
         /* Restart */
         i = 0;
      }
   }

   if (numElementsInArray != numElements)
   {
      logMessageWithArgsAndExitOnError(FATAL_LEVEL,
	        "ERROR reading state array: %d values with key= %s were not found.\n", numElementsInArray, key);
   }

   if (numElements == 0)
   {
      return NULL;
   }
   else
   {
      return arrayList;
   }
} /* populateArrayOfStringStateValues() -------------------------------------*/

/******************************************************************************
 * Module Name: populateArrayOfFloatStateValues()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *    Populates array of floating point of state values.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output   Type      Description
 *    key        In             char*     key carryover
 *    numElementsInArray In     int       number element in carryover array
 *    cArray     In             float*    contains carryover values
 *    index      In             int       index
 *
 * Return Value: the array state values in floating point 
 *           
 * Local Variables:
 *   Type        Name                     Description
 *
 * Global Variables Used:
 *   Type        Name           Origin    Description
 *   int         numStates
 *   
 * Constant and Macro Substitutions:
 *   Name           Header File              Description
 *  STATE_KEY_SIZE  states.h		      maximum is 50 
 *  FATAL_LEVEL     logging.h                 logging level - Fatal error. Program will exit
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
void populateArrayOfFloatStateValues(char *key, int numElementsInArray, 
                                       float *cArray, int index)
{
   /* Instead of mallocing realloc every time */
   float *arrayList = (float *) calloc(numElementsInArray, sizeof(float));
   int i;
   int numElements = 0;
   int curKeyIndex = 0;
   char curKey[STATE_KEY_SIZE];
   char curIndexInAscii[4];

   /* We're looking for each one of the array keys separately so as not to 
    * assume a sorted order in the file.  It's the slowest way to do a sort 
    * essentially, but there are few elements in states, performance is not 
    * important here
    */
   sprintf(curKey,"%s#%d", key, curKeyIndex);

   for (i  = 0; i < numStates + 1 ; i++)
   {
      if (!strcmp(states[i].name, curKey))
      {

         arrayList[numElements] = atof(states[i].value);

         /* Look for the next incremental key, but have to reset i now */
         sprintf(curIndexInAscii, "#%d", ++curKeyIndex);

         /* Reset the key and append next index */
         sprintf(curKey,"%s%s",key, curIndexInAscii);
         numElements++;

         if(curKeyIndex == numElementsInArray)
            break;

         /* Restart looking for the next key - instead of smartly sorting :-) */
         i = 0;
      }
   }
	
   if (numElementsInArray  != numElements )
   {
      logMessageWithArgsAndExitOnError(FATAL_LEVEL,
	        "ERROR reading state array: %d values with key= %s were not found.\n", numElementsInArray, key);
   }

   for (i = 0; i < numElements; i++)
   {
      cArray[index-1+i] = arrayList[i];
   }
   
   if(arrayList != NULL)
   	free(arrayList);

} /* populateArrayOfFloatStateValues() ------------------------------------- */

/******************************************************************************
 * Module Name: populateArrayOfFloatStateValuesPair()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *    Populates array of floating point of state value pair
 *
 * Calling Arguments:
 *
 *    Name       Input/Output    Type      Description
 *    key1       In              char*     key 1 carryover
 *    key2       In              char*     key 2 carryover
 *    numElementsInArray In      int       number element in carryover array
 *    cArray     In              float*    contains carryover values
 *    index      In              int       index
 *
 * Return Value: the array state values pair in floating point 
 *           
 * Local Variables:
 *   Type        Name                      Description
 *
 * Global Variables Used:
 *   Type        Name            Origin    Description
 *
 * Constant and Macro Substitutions:
 *   Name           Header File               Description
 *  STATE_KEY_SIZE  states.h		      maximum is 50 
 *  FATAL_LEVEL     logging.h                 logging level - Fatal error. Program will exit
 *  DEBUG_LEVEL     logging.h                 logging level - Debugging purpose
 *  INFO_LEVEL     logging.h                  logging level - information only
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void populateArrayOfFloatStateValuesPair(char *key1,char *key2, 
                                           int numElementsInArray, 
					   float *cArray, int index)
{
   /* Instead of mallocing realloc every time */
   float *arrayList = (float *) calloc(numElementsInArray*4, sizeof(float));
   int  i, j;
   int  numElements = 0;
   int  curKeyIndex1 = 0;
   int  curKeyIndex2 = 0;
   char curKey1[STATE_KEY_SIZE];
   char curKey2[STATE_KEY_SIZE];
   char curIndexInAscii1[4];
   char curIndexInAscii2[4];

   /* We're looking for each one of the array keys separately so as not to 
    * assume a sorted order in the file.  It's the slowest way to do a sort 
    * essentially, but there are few elements in states, performance is not 
    * important here
    */
   sprintf(curKey1,"%s#%d",key1, curKeyIndex1);
   sprintf(curKey2,"%s#%d",key2, curKeyIndex2);

   for (i = 0; i < numStates + 1; i++)
   {
      if ( getFewsDebugFlag() > 0 )
      {
         logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
	 "states[i].name, curKey1 %s %s\n",states[i].name, curKey1);
         logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
	 "states[i].name, curKey1, value %s %s %f\n",states[i].name, curKey1, atof(states[i].value));
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
	 "TEST1 in common curkey1 length", strlen(curKey1));
      }
      
      if (!strcmp(states[i].name, curKey1))
      {
         if ( getFewsDebugFlag() > 0 )
	 {
	    logMessageWithArgsAndExitOnError(DEBUG_LEVEL, 
            "TEST 1 in common numElements %d", numElements);
	 }
         arrayList[numElements] = atof(states[i].value);

         /* Look for the next incremental key, but have to reset i now */
         sprintf(curIndexInAscii1, "#%d", ++curKeyIndex1);

         /* Reset the key and append next index */
         sprintf(curKey1,"%s%s",key1, curIndexInAscii1);       
         numElements++;

         /* Restart looking for the next key - instead of smartly sorting :-) */
         i = 0;

         /* Seccond value of the pair need to be found. It could not be next to 
	  * the fist value.  For this reason we use similar loop to find fisrt 
	  * one. But we stop loop after found it.	
	  */
         for (j  = 0; j < numStates + 1; j++) 
         {
            if ( getFewsDebugFlag() > 0 )
	    {
	       logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
	       "states[j].name, curKey2 %s %s\n",states[j].name,curKey2);
	    }
	    
            if (!strcmp(states[j].name, curKey2))
            {
               arrayList[numElements] = atof(states[j].value);
	
               /* Look for the next incremental key */
               sprintf(curIndexInAscii2, "#%d", ++curKeyIndex2);
	
               /* Reset the key and append next index */
               sprintf(curKey2,"%s%s",key2, curIndexInAscii2);
               numElements++;	
               break;			
            }
         }			
      }
   }
   
   if (numElementsInArray*2 != numElements)
   {
      logMessageWithArgsAndExitOnError(FATAL_LEVEL,
	        "ERROR reading state array: %d values with keys = %s and %s were not found.\n", numElementsInArray, key1,key2);
   }
		
   for (i = 0; i < numElements*2; i++)
   {
      cArray[index-1+i] = arrayList[i];
   }

   if ( arrayList != NULL )
      free (arrayList);
      

/*   if (numElements == 0)
   {
     return NULL;
   }
   else
   {
     return arrayList;
   }
*/
} /* populateArrayOfFloatStateValuesPair() --------------------------------- */

/******************************************************************************
 * Module Name: populateArrayOfIntStateValues()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *    Populates array of integer state values.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output   Type      Description
 *    key        In             char*     key carryover
 *    numElementsInArray In     int       number element in array
 *    cArray     In             float*    contains carrayover values
 *    index      In             int       index
 *
 * Return Value: the array of state value in integer 
 *           
 * Local Variables:
 *   Type        Name                     Description
 *
 * Global Variables Used:
 *   Type            Name           Origin    Description
 
 *
 * Constant and Macro Substitutions:
 *   Name            Header File              Description
 *  STATE_KEY_SIZE  states.h		      maximum is 50 
 *  FATAL_LEVEL     logging.h                 logging level - Fatal error. Program will exit
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
int *populateArrayOfIntStateValues(char *key, int numElementsInArray, 
                                   float *cArray, int index)
{
   /* Instead of mallocing realloc every time */
   int *arrayList = (int *) calloc(numElementsInArray, sizeof(int));
   int i;
   int numElements = 0;
   int curKeyIndex = 0;
   char curKey[STATE_KEY_SIZE];
   char curIndexInAscii[4];

   /* We're looking for each one of the array keys separately so as not to 
    * assume a sorted order in the file.  It's the slowest way to do a sort 
    * essentially, but there are few elements in states, performance is not 
    * important here
    */
   sprintf(curIndexInAscii, "#%d", curKeyIndex);
   sprintf(curKey,"%s%s", key, curIndexInAscii); 

   for (i  = 0; i < numStates + 1; i++)
   {
      if (!strcmp(states[i].name, curKey))
      {
         arrayList[numElements] = atoi(states[i].value);

         /* Look for the next incremental key, but have to reset i now */
         sprintf(curIndexInAscii, "#%d", ++curKeyIndex);

         /* Reset the key and append next index */
         sprintf(curKey,"%s%s", key, curIndexInAscii);
         numElements++;
         i = 0;
     }
   }

   if (numElementsInArray != numElements)
   {
      logMessageWithArgsAndExitOnError(FATAL_LEVEL,
	        "ERROR reading state array: %d values with key= %s were not found.\n", numElementsInArray, key);
   }
   
   for (i = 0; i < numElements; i++)
   {
      cArray[index-1+i] = (float)(arrayList[i]);
   }
   
   if (numElements == 0)
   {
      return NULL;
   }
   else
   {
      return arrayList;
   }
} /* populateArrayOfIntStateValues() --------------------------------------- */

/******************************************************************************
 * Module Name: writeIntStateToFile()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *    Writes integer state value to file.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output     Type      Description
 *    filePtr    In               FILE*     file descriptor
 *    key        In               char*     key carryover
 *    cArray     In               float*    contains carryover values
 *    index      In               int       index
 *
 * Return Value: None 
 *           
 * Local Variables:
 *   Type        Name                       Description
 *
 * Global Variables Used:
 *   Type        Name             Origin    Description
 *
 * Constant and Macro Substitutions:
 *   Name        Header File                Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/
void writeIntStateToFile(FILE *filePtr, char *key, float *cArray, int index)
{
   fprintf(filePtr, "%s=%d\n", key, (int)(cArray[index-1]));

} /* writeIntStateToFile() ------------------------------------------------- */

/******************************************************************************
 * Module Name: writeFloatStateToFile()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *    Writes float state value to file.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output     Type      Description
 *    filePtr    In               FILE*     file descriptor 
 *    key        In               char*     key carryover
 *    cArray     In               float*    contains carryover value
 *    index      In               int       index
 *    
 * Return Value: None
 *           
 * Local Variables:
 *   Type        Name                       Description
 *
 * Global Variables Used:
 *   Type        Name             Origin    Description
 *
 * Constant and Macro Substitutions:
 *   Name        Header File                Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void writeFloatStateToFile(FILE *filePtr, char *key, float *cArray, int index)
{
   fprintf(filePtr, "%s=%f\n", key, cArray[index-1]);

} /* writeFloatStateToFile() ----------------------------------------------- */

/******************************************************************************
 * Module Name: writeStringStateToFile()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *    Writes string state value to file.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output     Type      Description
 *    filePtr    In               FILE*     file descriptor
 *    key        In               char*     key carryover
 *    cArray     In               float*    contains carryover values
 *    index      In               int       index
 *    
 * Return Value: None
 *           
 * Local Variables:
 *   Type        Name                       Description
 *
 * Global Variables Used:
 *   Type        Name             Origin    Description
 *
 * Constant and Macro Substitutions:
 *   Name        Header File                Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void writeStringStateToFile(FILE *filePtr, char *key, char *cArray)
{
   fprintf(filePtr, "%s=%s\n", key, cArray);

} /* writeStringStateToFile() ---------------------------------------------- */

/******************************************************************************
 * Module Name: writeArrayOfIntStatesToFile()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *    Writes array of integer state to file.
 *
 * Calling Arguments:
 *
 *    Name        Input/Output    Type      Description
 *    filePtr     In              FILE*     file descriptor
 *    key         In              char*     key carryover
 *    cArray      In              float*    contains carryover values
 *    index       In              int       index
 *    arrayLength In              int       array length
 *
 * Return Value: None
 *           
 * Local Variables:
 *   Type         Name                      Description
 *
 * Global Variables Used:
 *   Type         Name            Origin    Description
 *
 * Constant and Macro Substitutions:
 *   Name        Header File                Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void writeArrayOfIntStatesToFile(FILE *filePtr, char *key, float *cArray, 
                                 int index, int arrayLength)
{
   int i;

   /* Assume array indices <=99 */
   char newKey[strlen(key)+4];
   
   for (i = 0; i < arrayLength; i++)
   {
      sprintf(newKey,"%s#%d",key,i);
      fprintf(filePtr, "%s=%d\n", newKey, (int)(cArray[index-1+i]));
   }
} /* writeArrayOfIntStatesToFile() ----------------------------------------- */

/******************************************************************************
 * Module Name: writeArrayOfFloatStatesToFile()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *    Used to log single line messages from .c files.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output        Type      Description
 *    filePtr    In                  FILE*     file descriptor
 *    key        In                  char*     key carryover
 *    cArray     In                  float*    contains carryover values
 *    index      In                  int       index
 *    arrayLength In                 int       array length
 *
 * Return Value: None 
 *           
 * Local Variables:
 *   Type        Name                          Description
 *
 * Global Variables Used:
 *   Type        Name                Origin    Description
 *
 * Constant and Macro Substitutions:
 *   Name        Header File                   Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void writeArrayOfFloatStatesToFile(FILE *filePtr, char *key, float *cArray, 
                                   int index, int arrayLength)
{
   int i;
   
   /* Assume array indices <=99 */
   char newKey[strlen(key)+4];
   
   for (i = 0; i < arrayLength; i++)
   {
      sprintf(newKey,"%s#%d",key,i);
      fprintf(filePtr, "%s=%f\n", newKey, cArray[index-1+i]);
   }

} /* writeArrayOfFloatStatesToFile() --------------------------------------- */

/******************************************************************************
 * Module Name: writeArrayOfFloatStatesPairsToFile()
 *
 * Original Author: CHPS/FEWS Team
 *
 * Module Creation Date: 5/5/08
 *
 * Description:
 *    Writes arry of floating point state values to file.
 *
 * Calling Arguments:
 *
 *    Name       Input/Output   Type      Description
 *    filePtr    In             FILE*     file descriptor
 *    key1       In             char*     key 1 carryover
 *    key2       In             char*     key 2 carryover
 *    cArray     In             float*    contains carryover array values
 *    index      In             int       index
 *    arrayLength In            int       array length
 *
 * Return Value: None 
 *           
 * Local Variables:
 *   Type        Name                     Description
 *
 * Global Variables Used:
 *   Type        Name           Origin    Description
 *
 * Constant and Macro Substitutions:
 *   Name        Header File              Description
 *
 * Modification History:
 *           Date           Developer        Action
 *
 *****************************************************************************/

void writeArrayOfFloatStatesPairsToFile(FILE *filePtr, char *key1, char *key2, 
                                        float *cArray, int index, 
					int arrayLength)
{
   int i;
   int j = 0;
   
   /* Assume array indices <=99 */
   char newKey1[strlen(key1)+4];
   char newKey2[strlen(key2)+4];

   for (i = 0; i < arrayLength; i++)
   {		
      sprintf(newKey1,"%s#%d",key1,i);
      sprintf(newKey2,"%s#%d",key2,i);
      fprintf(filePtr, "%s=%f\n", newKey1, cArray[index-1+j]);		
      fprintf(filePtr, "%s=%f\n", newKey2, cArray[index-1+(j+1)]);
      j = j + 2;		
   }

} /* writeArrayOfFloatStatesPairsToFile() ---------------------------------- */

void freeStates()
{
   int count = 0;
   if ( states != NULL )
   {
    if ( numStates > 0 )
    {
       for ( count = 0; count < numStates +1; count++ )
       {
          if ( states[count].name != NULL )
	     free( states[count].name);

          if ( states[count].value != NULL )
	     free( states[count].value );
       }
       
       free( states );
    }
    states = NULL;
   }
}


