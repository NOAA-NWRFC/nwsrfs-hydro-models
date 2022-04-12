#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "logging.h"
#include "utilities.h"
#include "parse_mod_file.h"

#define ID_LENGTH 30
//#define MAX_NUM_MODS 30000

static MODS *allMods = NULL;
MODS *moreMods;

static int modCount = 0;
static int num_identifiers = -1;
FILE *fd = NULL;

int isModFileEmpty(char *fileName)
{
   off_t st_size;
   struct stat *buf;
   
   buf = (struct stat*) calloc(1, sizeof(struct stat));
   stat(fileName, buf);
   st_size = (off_t) buf->st_size;
   
   if(buf != NULL)
     free(buf);

   if(st_size == 0)
   {
      
      if ( getFewsDebugFlag() > 0 )
      {
         logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
         "Mod file: %s is empty\n", fileName );
      }
   }
   
   return (int) st_size;
}

//huge draw back with the following code line.
//we are assuming that, there will be less than
//100 entries in the "mods.txt" file. If there
//is a possibility of more, it should be changed
//to a pointer and malloc'ed and free'd.
//CP static MODS allMods[MAX_NUM_MODS];

int parseModsFile(char *fileName, int *modFileSize)
{
   char *str = NULL;
   char line[SIZE_OF_LINE];

   int i;
   int isNum = 1;

   float effectiveDate = 0.;
   int idCount = 0;

   char leftSideOfEquals[SIZE_OF_LINE];
   char rightSideOfEquals[SIZE_OF_LINE];
   char eachToken[SIZE_OF_LINE];
   
   //int *modFileSize;

   *modFileSize = isModFileEmpty(fileName);
   if(*modFileSize == 0)
   {
      if ( getFewsDebugFlag() > 0 )
      {
         logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
         "Empty mod file: %s will not be parsed\n", fileName);
         
      }
      return modCount;
   }
   
   str = (char*) malloc(sizeof(char)* SIZE_OF_LINE);
   
   logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
   "Start parsing mod file :%s\n", fileName);
   

   if((fd = fopen(fileName, "r")) == NULL)
   {
      logMessageWithArgsAndExitOnError(FATAL_LEVEL,
      "Mod file:%s not found\n", fileName);
   }
   else
   {     

      memset(line, '\0', SIZE_OF_LINE);
      
      while(fgets(line, SIZE_OF_LINE, fd) != NULL)
      {	 

	 isNum = 1;
	 strtok(line, "\n");
	 memset(str, '\0', SIZE_OF_LINE);
       
         char *temp = trimStringEnds(line);

         if(temp != NULL)
         {
	    strcpy(str, temp);
            free(temp);
         }

	 if(str != NULL && str[0] != '#')
	 {
            i = 0;
	    while(str[i] != '\0')
	    {
	       if(str[i] == ' ' || str[i] == '.')
	       {
	          i++;
		  continue;
	       }
	       if(isdigit(str[i]))
	       {
	          i++;
	          continue;
	       }
	       else
	       {
	          isNum = 0;
	          break;
	       }
	    }
	    if(isNum)
	    {
	       //log reading of number of identifiers
	       //for this set of mods
	       sscanf(str, "%d %f", &num_identifiers, &effectiveDate);
	       //isNum = 1;
	       idCount = 0;
	    }

	    if(strstr(str, "=") != NULL)
	    {
               moreMods = (MODS*) realloc(allMods, (modCount+1) * sizeof(MODS)); 

               if ( moreMods != NULL )
	       {
		  allMods = moreMods;
	       }
	       else
	       {
                  logMessageWithArgsAndExitOnError(FATAL_LEVEL,
		  "ERROR - (re)allocating memory for allMods\n");
	       }
	       //log reading a non number line or a mods
	       //entry
	       memset(leftSideOfEquals, '\0', SIZE_OF_LINE);
               memset(rightSideOfEquals, '\0', SIZE_OF_LINE);
	       memset(eachToken, '\0', SIZE_OF_LINE);
	       
	       //extracting the value
	       strcpy(leftSideOfEquals, str);
	       strtok(leftSideOfEquals, "=");
	       strcpy(rightSideOfEquals, strtok(NULL, "="));
	       
	       //copying the mod name
	       allMods[modCount].value = atof(rightSideOfEquals);
	       
	       //extracting the mod name
	       //strtok(leftSideOfEquals, "-");
	       strcpy(eachToken, strtok(leftSideOfEquals, "_"));
	       
	       //copying the mod name
	       allMods[modCount].modName = (char*) calloc(SIZE_OF_LINE, sizeof(char));

	       strcpy(allMods[modCount].modName, eachToken);

               if(num_identifiers > 0)
	       {
	          idCount = 0;		  
		  allMods[modCount].identifiers = (char**) calloc(num_identifiers, sizeof(char*));
		  
		  while(idCount < num_identifiers)
		  {
		     //extracting all identifiers and copying 
		     strcpy(eachToken, strtok(NULL, "_"));
		     allMods[modCount].identifiers[idCount] = (char*) calloc(SIZE_OF_LINE, sizeof(char));
		     strcpy(allMods[modCount].identifiers[idCount], eachToken);
		     
		     idCount++;
		  }
	       }
	       //extracting and copying the date part
	       strcpy(eachToken, strtok(NULL, "_"));
	       allMods[modCount].date = atof(eachToken);
	      
	       //storing number of identifiers for each row
	       //which makes it easier for data retrieval
	       allMods[modCount].numIdentifiers = num_identifiers;
	       allMods[modCount].effectiveDate = effectiveDate;
	       
               //increase mod count
               modCount++;
	       
	    }
	 }
      }
   }
   
   if(str != NULL)
     free(str);

   if ( modCount <= 0 )
   {
       *modFileSize=0;
       logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
       "IGNORE MODS - Because number of mods is %d ....\n", modCount );
   }

   return modCount;
}

void close_mod_file()
{
   if ( modCount > 0 )
      freeMods();

   if ( fd != NULL )
   {
      fclose( fd );
      fd = NULL;
   }

}


/*void printVals()
{
   int i = 0;
   
   while(1)
   {
      if(allMods[i].modName == NULL)
      {
         break;
      }
      else
      {
         printf("modName = %s\n", allMods[i].modName);
         //printf("identifier = %s\n", allMods[i].identifiers[0]);
         printf("date = %f\n", allMods[i].date);
         printf("value = %lf\n", allMods[i].value);
         printf("numIdentifiers = %d\n", allMods[i].numIdentifiers);
	 printf("effectiveDate = %f\n", allMods[i].effectiveDate);
         printf("---------------\n");
      }

      i++;
   }
}*/

//This function will malloc numResults and modResults.The calling function 
//should ensure that numResults is malloc'ed and free'd after use. The function should
//NOT attempt to free modResults because this will free the global pointer.
/*void getEntries(int *numResults, MODS modResults[MAX_NUM_MODS], const char *modName)
{ 
    int i = 0, j = 0;
    int resultCount = 0;

    for(i=0;i<modCount;i++)
    {
       if(modName == NULL || numResults == NULL)
       {
          //log message - Some of the arguements are NULL
	  
	  return;
       }
       if(strcmp(allMods[i].modName, modName) == 0)
       {
           modResults[resultCount].modName = (char*) malloc(sizeof(char)*SIZE_OF_LINE);
	   strcpy(modResults[resultCount].modName, allMods[i].modName);
	   if(allMods[i].numIdentifiers > 0)
	   {
              modResults[resultCount].identifiers = (char**) malloc(sizeof(char*)*allMods[i].numIdentifiers);
	      for(j=0;j<allMods[i].numIdentifiers;j++)
	      {
	         modResults[resultCount].identifiers[j] = (char*) malloc(sizeof(char)*SIZE_OF_LINE);
		 strcpy(modResults[resultCount].identifiers[j], allMods[i].identifiers[j]);
	      }
	   }
	   modResults[resultCount].date = allMods[i].date;
	   modResults[resultCount].value = allMods[i].value;
	   modResults[resultCount].numIdentifiers = allMods[i].numIdentifiers;

	   resultCount++;
       }
    }

    if(resultCount == 0)
    {
       //log - no results found
    }
    *numResults = resultCount;

    return;
}*

//This function will malloc numResults and modResults.The calling function 
//should ensure that numResults is malloc'ed and free'd after use. The function should
//NOT attempt to free modResults because this will free the global pointer.
//This function returns all the rows with the number of identifiers equal to numIds
//This is used in getValue() function.
/*void getEntriesWithFixedIdentifiers(int *numResults, MODS modResults[MAX_NUM_MODS], int numIds)
{
    int i = 0, j = 0;
    int resultCount = 0;

    for(i=0;i<modCount;i++)
    {
       if(numResults == NULL)
       {
          //log message - Some of the arguements are NULL
	  
	  return;
       }
       if(allMods[i].numIdentifiers == numIds)
       {
           modResults[resultCount].modName = (char*) malloc(sizeof(char)*SIZE_OF_LINE);
	   strcpy(modResults[resultCount].modName, allMods[i].modName);
	   if(numIds > 0)
	   {
              modResults[resultCount].identifiers = (char**) malloc(sizeof(char*)*numIds);
	      for(j=0;j<numIds;j++)
	      {
	         modResults[resultCount].identifiers[j] = (char*) malloc(sizeof(char)*SIZE_OF_LINE);
		 strcpy(modResults[resultCount].identifiers[j], allMods[i].identifiers[j]);
	      }
	   }
	   modResults[resultCount].date = allMods[i].date;
	   modResults[resultCount].value = allMods[i].value;
	   modResults[resultCount].numIdentifiers = allMods[i].numIdentifiers;

	   resultCount++;
       }
    }

    if(resultCount == 0)
    {
       //log - no results found
    }
    *numResults = resultCount;

    return;
}*/

/*void printResultsFromGetEntries()
{
   int i;
   int *num = NULL;
   MODS mds[MAX_NUM_MODS];
   
   num = (int*) malloc(sizeof(int));
   getEntries(num, mds, "SSARREG");
   
   for(i=0;i<*num;i++)
   {
         printf("modName = %s\n", mds[i].modName);
         //printf("identifier = %s\n", mds[i].identifiers[0]);
         printf("date = %f\n", mds[i].date);
         printf("value = %lf\n", mds[i].value);
         printf("numIdentifiers = %d\n", mds[i].numIdentifiers);
         printf("---------------\n");
   }
   
   if(num != NULL)
   {
      free(num);
      num = NULL;
   }
   
   return;
}*/

//This function returns an integer with a
//value '3' if the identifier is upstream
//only, '1' if downstream only, '2' if both
//or '0' if neither.
void is_upstream_or_both_or_neither_(int *idType)
{
   int i;
   int up = 0;
   int dn = 0;
   
   //idType = (int*) malloc(sizeof(int));
   if(idType == NULL)
   {
      //Log error
   }
   
   for(i=0;i<modCount;i++)
   {
      if(allMods[i].numIdentifiers > 0)
      {
         if(!strcmp(allMods[i].identifiers[0], "US"))
	 {
	    up = 1;
	 }
	 else if(!strcmp(allMods[i].identifiers[0], "DS"))
	 {
	    dn = 1;
	 }
      }
      else
      {
         //Log error
      }
   }
   
   if(up == 1)
   {
      if(dn == 1)
      {
         *idType = 2;
      }
      else
      {
         //Log error
      }
   }
   else if(dn == 1)
   {
      *idType = 1;
   }
   else
   {
      //Log error
   }

}

int getNumIdentifiers()
{
   int numIds = 0;
   
   if(allMods[0].modName != NULL)
   {
      numIds = allMods[0].numIdentifiers;
   }

   return numIds;
}

//This function takes variable number of arguements in a given order.
//The order is as follows:
//(1) A malloc'ed pointer to a float which is the value found. The calling
//function should free it after use.
//(2) Number of variable arguements being passed after this arguement
//Arguement list of strings with
//(3) Mod Name
//(4) Date as a string
//(5)...The identifiers in order...
//This function returns a value of the matching row. It returns -999.0
//if there is no match.
/*void getValue(float *val, int n_args, ...)
{
   register int i;
   int j;
   va_list ap;
   
   //int numIdents = numIds;
   char modName[SIZE_OF_LINE];
   float date = 0.;
   int numIds = getNumIdentifiers();
   char identifiers[numIds][SIZE_OF_LINE];
   
   int *num = NULL;
   //MODS mds[MAX_NUM_MODS];
   int idsDontMatch = 0;
   
   *val = -999.0;

   for(i = 0; i < numIds; i++) 
   {
      memset(identifiers[i], '\0', SIZE_OF_LINE);
   }

   va_start(ap, n_args);   
   for(i = 0; i < n_args; i++) 
   {
      if(i == 0)
      {
	 memset(modName, '\0', SIZE_OF_LINE);
	 strcpy(modName, va_arg(ap, char*));
      }
      else if(i == 1)
      {
	 date = atol(va_arg(ap, char*));
      }
      else
      {
         strcpy(identifiers[i-numIds-1], va_arg(ap, char*));
      }
   }
   va_end(ap);
   
   //---------------------------------------
   //could separate top and bottom parts of these
   //comment lines into separate functions
   //---------------------------------------
   
   num = (int*) malloc(sizeof(int));
   
   *num = modCount;
   //mds = allMods;
   //getEntriesWithFixedIdentifiers(num, mds, numIds);
   
   for(i = 0; i < *num; i++)
   {
      if(strcmp(allMods[i].modName, modName) != 0)
      {
         //log - no match found
	 continue;
      }
      if(allMods[i].date != date)
      {
	 //log - no match found
	 continue;
      }
      for(j=0;j<numIds;j++)
      {
	 if(strcmp(allMods[i].identifiers[j], identifiers[j]) != 0)
	 {
	    //log - no match found
	    idsDontMatch = 1;
	    break;
	 }
      }
      
      if(idsDontMatch == 1)
      {
         idsDontMatch = 0;
         continue;
      }

      *val = allMods[i].value;
      break;
   }
   
   //free block
   if(num != NULL)
   {
      free(num);
      num = NULL;
   }
}*/

//This function takes variable number of arguements in a given order.
//The order is as follows:
//(1) A NULL integer pointer to be assigned the number of dates that matched
//(2) a float dates empty array
//(3) Number of variable arguements being passed after this arguement
//Arguement list of strings with
//(4) Mod Name
//(5)...The identifiers in order...these arguments need not be passed
//and can be left empty.
//This function assigns the first argument the number of dates that matched the
//arguments passed.
void get_num_dates_and_dates_(int *numDates, float dates[], int *n_args, ...)
{
   register int i;
   int j;
   va_list ap;
   
   char modName[SIZE_OF_LINE];
   char modNameCopy[SIZE_OF_LINE];
   float date = 0.;
   int numIds = getNumIdentifiers();
   char identifiers[numIds][SIZE_OF_LINE];
   int dateCount = 0;
   
   //int *num = NULL;
   int num;

   int idCnt = 0;
   char catStr[SIZE_OF_LINE];

   for(i = 0; i < numIds; i++) 
   {
      memset(identifiers[i], '\0', SIZE_OF_LINE);
   }

   va_start(ap, *n_args);   
   for(i = 0; i < *n_args; i++) 
   {
      if(i == 0)
      {
	 memset(modName, '\0', SIZE_OF_LINE);
	 strcpy(modName, va_arg(ap, char*));
	 modName[ID_LENGTH]='\0';
         strcpy(modNameCopy, modName);	
         
         char *temp = trimStringEnds(modNameCopy);
        
         if(temp != NULL)
         {
           strcpy(modName, temp);
           free(temp);
         }
      }
      else
      {
         strcpy(identifiers[i-1], va_arg(ap, char*));
	 idCnt++;
      }
   }
   va_end(ap);
   
   //---------------------------------------
   //could separate top and bottom parts of these
   //comment lines into separate functions
   //---------------------------------------
   
   //num = (int*) malloc(sizeof(int));   
   //*num = modCount;
   num = modCount;

   //for(i = 0; i < *num; i++)
   for(i = 0; i < num; i++)
   {
      if(strcmp(allMods[i].modName, modName) != 0)
      {
         //log - no match found
	 continue;
      }
      
      memset(catStr, '\0', SIZE_OF_LINE);
      for(j=0;j<numIds;j++)
      {
	 strcat(catStr, allMods[i].identifiers[j]);
      }
      for(j=0;j<idCnt;j++)
      {
	 if(strstr(catStr, identifiers[j]) != NULL)
	 {
	       //log - no match found
	       break;
	 }
      }
      if(j == idCnt)
      {
	 if ( numIds == 0 && idCnt == 0)
	 {
	    date = allMods[i].date;
	    dates[dateCount] = date;
            dateCount++;
	 }
	 continue;
      }

      date = allMods[i].date;
      
      dates[dateCount] = date;
      
      dateCount++;
   }
   
   *numDates = dateCount;   
      
   /*free block
   if(num != NULL)
   {
      free(num);
      num = NULL;
   } */
}

void getModCount(int *cnt)
{
   *cnt = modCount;
}

void get_effective_date_(float *effDate)
{
   if(allMods[0].modName == NULL)
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
      "No entries in the allMods global structure; "
      "either this function was called before the mods file was "
      "parsed or mods file is empty...returning -0.f\n");
      
      *effDate = -0.f;
   }
   else
   {
      *effDate = allMods[0].effectiveDate;
   }
}

//This function takes variable number of arguements in a given order.
//The order is as follows:
//(1) A non NULL integer pointer to be assigned the number of matches
//(2) Number of variable arguements being passed after this arguement
//Arguement list of strings with
//(3) Mod Name - this could be NULL an indication to ignore this field
//while searching for matches
//(4) The date - this could be -1L an indication to ignore this field
//while searching for matches
//(5)...The identifiers in order...these arguments need not be passed
//and can be left empty.
void get_num_matches_(int *numMatches, int *n_args, ...)
{
   register int i;
   int j;
   va_list ap;
   
   char modName[SIZE_OF_LINE];
   char modNameCopy[SIZE_OF_LINE];
   float date = -1.0;
   float value = -999.0;
   int numIds = getNumIdentifiers();
   char identifiers[numIds][SIZE_OF_LINE];
   
   int idCnt = 0;
   char catStr[SIZE_OF_LINE];
   
   //int *num = NULL;
   int num;
   //MODS mds[MAX_NUM_MODS];

   for(i = 0; i < numIds; i++) 
   {
      memset(identifiers[i], '\0', SIZE_OF_LINE);
   }
   
   va_start(ap, n_args);      
   for(i = 0; i < *n_args; i++) 
   {
      if(i == 0)
      {
	 memset(modName, '\0', SIZE_OF_LINE);
	 strcpy(modName, va_arg(ap, char*));
	 modName[ID_LENGTH]='\0';
         strcpy(modNameCopy, modName);	

         char *temp = trimStringEnds(modNameCopy);

         if(temp != NULL)
         {
           strcpy(modName, temp);
           free(temp);
         }
      }
      else if(i == 1)
      {
	 date = atol(va_arg(ap, char*));
      }
      else
      {
         strcpy(identifiers[i-numIds], va_arg(ap, char*));
	 idCnt++;
      }
   }
   va_end(ap);
   
   //---------------------------------------
   //could separate top and bottom parts of these
   //comment lines into separate functions
   //---------------------------------------
   
   //num = (int*) malloc(sizeof(int));
   //*num = modCount;
   num = modCount;

   //numMatches = (int*) malloc(sizeof(int));
   *numMatches = 0;
   
   //for(i = 0; i < *num; i++)
   for(i = 0; i < num; i++)
   {
      if(modName != NULL)
      {
	 if(strcmp(allMods[i].modName, modName) != 0)
	 {
            //log - no match found
	    continue;
	 }
      }
      if(date > 0)
      {
	 if(allMods[i].date != date)
	 {
	    //log - no match found
	    continue;
	 }
      }

      memset(catStr, '\0', SIZE_OF_LINE);
      for(j=0;j<numIds;j++)
      {
	 strcat(catStr, allMods[i].identifiers[j]);
      }
      for(j=0;j<idCnt;j++)
      {
	 if(strstr(catStr, identifiers[j]) != NULL)
	 {
	       //log - no match found
	       break;
	 }
      }
      if(j == idCnt)
      {
	 continue;
      }

      *numMatches = *numMatches + 1;
   }
      
   /*free block
   if(num != NULL)
   {
      free(num);
      num = NULL;
   } */
}

//This function takes variable number of arguements in a given order.
//The order is as follows:
//(1) A malloc'ed integer pointer to be assigned the number of matches
//(2) an empty float array to be filled with results. The calling program
//must ensure the sizeof the array using getModCount() function.
//(3) Number of variable arguements being passed after this arguement
//Arguement list of strings with
//(4) Mod Name - this could be NULL an indication to ignore this field
//while searching for matches
//(5) The date - this could be -1.0 an indication to ignore this field
//while searching for matches
//(6)...The identifiers in order...these arguments need not be passed
//and can be left empty.
void get_num_values_and_values_(int *numValues, float values[], int *n_args, ...)
{
   register int i;
   int j;
   va_list ap;
   
   //int numIdents = numIds;
   char modName[SIZE_OF_LINE];
   char modNameCopy[SIZE_OF_LINE];
   float date = 0.;
   float value = -999.0;
   int numIds = getNumIdentifiers();
   char identifiers[numIds][SIZE_OF_LINE];
   
   //int *num = NULL;
   int num;
   //MODS mds[MAX_NUM_MODS];
   
      
   int idCnt = 0;
   char catStr[SIZE_OF_LINE];

   for(i = 0; i < numIds; i++) 
   {
      memset(identifiers[i], '\0', SIZE_OF_LINE);
   }

   va_start(ap, *n_args);   
   for(i = 0; i < *n_args; i++) 
   {
      if(i == 0)
      {
	 memset(modName, '\0', SIZE_OF_LINE);
	 strcpy(modName, va_arg(ap, char*));
	 modName[ID_LENGTH]='\0';
         strcpy(modNameCopy, modName);	

         char *temp = trimStringEnds(modNameCopy);
 
         if(temp != NULL)
         {
            strcpy(modName, temp);
            free(temp);
         }
      }
      else if(i == 1)
      {
	 date = atol(va_arg(ap, char*));
      }
      else
      {
         strcpy(identifiers[i-numIds], va_arg(ap, char*));
	 idCnt++;
      }
   }
   va_end(ap);
   
   //---------------------------------------
   //could separate top and bottom parts of these
   //comment lines into separate functions
   //---------------------------------------
   
   //num = (int*) malloc(sizeof(int));
   //*num = modCount;
   num = modCount;
   
   *numValues = 0;
   
   //for(i = 0; i < *num; i++)
   for(i = 0; i < num; i++)
   {
      if(modName != NULL)
      {
	 if(strcmp(allMods[i].modName, modName) != 0)
	 {
            //log - no match found
	    continue;
	 }
      }
      if(date > 0)
      {
	 if(allMods[i].date != date)
	 {
	    //log - no match found
	    continue;
	 }
      }
      memset(catStr, '\0', SIZE_OF_LINE);
      for(j=0;j<numIds;j++)
      {
	 strcat(catStr, allMods[i].identifiers[j]);
      }
      for(j=0;j<idCnt;j++)
      {
	 if(strstr(catStr, identifiers[j]) != NULL)
	 {
	       //log - no match found
	       break;
	 }
      }
      if(j == idCnt)
      {
         if ( numIds == 0 && idCnt == 0 )
	 {
	    values[*numValues] = allMods[i].value;
	    *numValues = *numValues+1;
	 }
	 continue;
      }

      values[*numValues] = allMods[i].value;
      *numValues = *numValues+1;
   }
      
   /*free block
   if(num != NULL)
   {
      free(num);
      num = NULL;
   } */
}

//This function takes variable number of arguements in a given order.
//The order is as follows:
//(1) A malloc'ed integer pointer to be assigned the number of matches
//(2) an empty int array to be filled with results. The calling program
//must ensure the sizeof the array using getModCount() function.
//(3) Number of variable arguements being passed after this arguement
//Arguement list of strings with
//(4) The date - this could be -1L an indication to ignore this field
//while searching for matches
//(5)only the first piece of the identifier that indicates "US" or "DS"
// the modName need not be passed and for this function it is assumed as
//"SSARREG"
void get_codes_from_ssarresv_(int *numIdents, int idents[], int *n_args, ...)
{
   register int i;
   int j;
   va_list ap;
   
   //int numIdents = numIds;
   char modName[SIZE_OF_LINE];
   float date = 0.;
   float value = -999.0;
   int numIds = getNumIdentifiers();
   char identifiers[numIds][SIZE_OF_LINE];
   
   //int *num = NULL;
   int num;
   //MODS mds[MAX_NUM_MODS];
   
   int idCnt = 0;
   char catStr[SIZE_OF_LINE];

   for(i = 0; i < numIds; i++) 
   {
      memset(identifiers[i], '\0', SIZE_OF_LINE);
   }
   
   memset(modName, '\0', SIZE_OF_LINE);
   strcpy(modName, "SSARREG");

   va_start(ap, *n_args);   
   for(i = 0; i < *n_args; i++) 
   {
      if(i == 0)
      {
	 date = atof(va_arg(ap, char*));
      }
      else
      {
         strcpy(identifiers[i-numIds+1], va_arg(ap, char*));
	 idCnt++;
      }
   }
   va_end(ap);
   
   //---------------------------------------
   //could separate top and bottom parts of these
   //comment lines into separate functions
   //---------------------------------------
   
   //num = (int*) malloc(sizeof(int));
   //*num = modCount;
   num = modCount;
   
   *numIdents = 0;
   
   //for(i = 0; i < *num; i++)
   for(i = 0; i < num; i++)
   {
      if(modName != NULL)
      {
	 if(strcmp(allMods[i].modName, modName) != 0)
	 {
            //log - no match found
	    continue;
	 }
      }
      if(date > 0)
      {
	 if(allMods[i].date != date)
	 {
	    //log - no match found
	    continue;
	 }
      }
 
      memset(catStr, '\0', SIZE_OF_LINE);

      for(j=0;j<numIds;j++)
      {
	 strcat(catStr, allMods[i].identifiers[j]);
      }

      for(j=0;idCnt;j++)
      {
	 if(strstr(catStr, identifiers[j]) != NULL)
	 {
	       //log - no match found
	       break;
	 }
      }
      if(j == idCnt)
      {
	 continue;
      }

      if(numIds > 1)
      {
         idents[*numIdents] = getCharAtEnd(allMods[i].identifiers[1]);
         *numIdents = *numIdents+1;
      }
   }

}

int getCharAtEnd(char *str)
{
   int ch = -1;
   char chr[2];
   
   if(str == NULL)
   {
      logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
      "In getCharAtEnd(): string is null...returning -1");
      
      return ch;
   }
   
   chr[0] = str[strlen(str)-1];
   chr[1] = '\0';
   ch = atoi(chr);
   
   return ch;
}
/****************************************************************************
 This function takes variable number of arguements in a given order.
 The order is as follows:
(1) A NULL integer pointer to be assigned the number of dates that matched
(2) A float dates empty array
(3) An empty float array to be filled with results
(4) Number index of keyword in order(1-API, 2-SMI, 3-SMID, 4-BFSC, 5-BFI,
    6-FI, 7-FEI and 8-UNKNOWN
(5) Number of variable arguements being passed after this arguement
 Arguement list of strings with
(6) Mod Name
(7)...The identifiers in order...these arguments need not be passed
and can be left empty.
This function assigns the first argument the number of dates that matched the
arguments passed.

 Note: This function is only used by API-CONT model.
 
*****************************************************************************/
void get_dates_values_apicont_(int *numMatch, float dates[], 
                               float values[], 
                               int idco[], int *n_args, ...)
{
   int i, j;
   va_list ap;
   
   int  matchCount = 0;
   int  idCnt = 0;
   int  num = modCount;
   int  numIds = getNumIdentifiers();
   char identifiers[numIds][SIZE_OF_LINE];
   char modName[SIZE_OF_LINE];
   char modNameCopy[SIZE_OF_LINE];
   char catStr[SIZE_OF_LINE];  
   char id[num][SIZE_OF_LINE];

   memset(id, '\0',sizeof(id));

   for ( i = 0; i < numIds; i++ ) 
   {
      memset(identifiers[i], '\0', SIZE_OF_LINE);
   }

   va_start(ap, *n_args);  
    
   for ( i = 0; i < *n_args; i++ ) 
   {
      if ( i == 0 )
      {
	 memset(modName, '\0', SIZE_OF_LINE);
	 strcpy(modName, va_arg(ap, char*));
	 modName[ID_LENGTH]='\0';
         strcpy(modNameCopy, modName);	

         char *temp = trimStringEnds(modNameCopy);

         if(temp != NULL)
         {
           strcpy(modName, temp);
           free(temp);
         }
      }
      else
      {
         strcpy(identifiers[i-1], va_arg(ap, char*));

	 idCnt++;
      }
   }
   
   va_end(ap);
     
   for ( i = 0; i < num; i++ )
   {
      if ( strcmp(allMods[i].modName, modName) != 0 )
      {
         /*log - no match found */
	 continue;
      }
      
      memset(catStr, '\0', SIZE_OF_LINE);
      
      for ( j = 0; j < numIds; j++ )
      {
	 strcat(catStr, allMods[i].identifiers[j]);
      }
      
      for ( j = 0; j < idCnt; j++ )
      {
         if ( strcmp(catStr, identifiers[j]) != 0 )
	 {
	       /*log - no match found*/
	       break;
	 }
      }
      
      /* store date(s) */
      dates[matchCount] = allMods[i].date;
      
      /* store value(s) */
      values[matchCount] = allMods[i].value;

      /* store identifier(s) */
      strncpy(id[matchCount], catStr,strlen(catStr));
       
      /* increase number matching */
      matchCount++;
      
   }/*i loop*/
   
   for ( i = 0; i < matchCount; i++ )
   {
      if ( !strcmp(id[i],"API") ) idco[i] = 1;
      else if ( !strcmp(id[i],"SMI") ) idco[i] = 2;
      else if ( !strcmp(id[i],"SMID") ) idco[i] = 3;
      else if ( !strcmp(id[i],"BFSC") ) idco[i] = 4;
      else if ( !strcmp(id[i],"BFI") ) idco[i] = 5;
      else if ( !strcmp(id[i],"FI") ) idco[i] = 6;
      else if ( !strcmp(id[i],"FEI") ) idco[i] = 7;
      else if ( !strcmp(id[i],"UNKNOWN") ) idco[i] = 8;
   }
   
   *numMatch = matchCount;   

} /* get_dates_values_apicont_() ========================================== */


void freeMods()
{
   int count = 0, idCount ;

   if ( allMods != NULL )
   {
      for ( count = 0; count < modCount; count++ )
      {
         if ( allMods[count].modName != NULL )
	 {
	    free( allMods[count].modName );
	 }

	 if ( allMods[count].identifiers != NULL )
	 {
            if ( num_identifiers > 0 )
	    {
               for ( idCount = 0; idCount < num_identifiers; idCount++ )
	       {
	          free( allMods[count].identifiers[idCount] );
	       }
               free( allMods[count].identifiers );
	    }
	 }
      }
      free( allMods );
    }
}
