/*****************************************************************************************
* FILENAME:             retrieveFFGConfig.c
* GENERAL INFORMATION:  retrieve configuration data fields in argments.txt and
*                       assign them to ffg_config structure
* PROGRAMMER:           Jingtao Deng
* CREATION DATE:        May, 2010
* ORGANIZATIONL         OHD11/HSEB
* MACHINE:              Linux
* MODIFICATION HISTORY:
******************************************************************************************/
#include "ffgGribProduct.h"
#include "ffgConfig.h"

void retrieveFFGConfig(char *fname)	       
{
   int status;
   Properties* args_properties = NULL; 
   int numOfProperties;


   if ( getFewsDebugFlag() > DEBUG_LEVEL_FLAG )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
                                        "retrieveFFGConfig() - Start to retrieve configuration data...\n");
   }
   
   /* initialize ffg_config structure 
   ffg_config = (ffgConfigStr *)malloc(sizeof(ffgConfigStr));
   */
   /* malloc properties 
   args_properties = (Properties *) calloc(MAX_PROPERTIES, sizeof(Properties));*/
   

   /* retrieve the content from ffg_configuration.txt file */
   setArgsFileName(fname);
   args_properties = readArgs(&numOfProperties);   
         
   /* define the ffg_config structure from the data defined in arguments.txt file */
			     			        
   define_ffgConfig_struct(numOfProperties, args_properties);
   
   /* free memory*/
   
   if (args_properties != NULL)
   {
      free(args_properties);
      args_properties = NULL;   
   }
     
return;
}				  


/***************************************************************************
void define_ffgConfig_struct()
***************************************************************************/
void define_ffgConfig_struct(int           numOfProperties,
                             Properties    *properties)			     
			     
{
   int i;
     
   for (i=0; i<numOfProperties; i++)
   {
      if (!strcmp(properties[i].name, PROCESSID))
      {
         ffg_config->processId = atoi(properties[i].value);
      }
      else if (!strcmp(properties[i].name, SITEID))
      {
         strcpy(ffg_config->siteId, properties[i].value);
      }	
      else if (!strcmp(properties[i].name, WMOID))
      {
         strcpy(ffg_config->wmoId, properties[i].value);
      }	 
      else if (!strcmp(properties[i].name, COMPUTATIONYEAR))
      {
         ffg_config->computationYear = atoi(properties[i].value);
      }	 
      else if (!strcmp(properties[i].name, COMPUTATIONMON))
      {
         ffg_config->computationMon = atoi(properties[i].value);
      }	
      else if (!strcmp(properties[i].name, COMPUTATIONDAY))
      {
         ffg_config->computationDay = atoi(properties[i].value);
      }	  
      else if (!strcmp(properties[i].name, COMPUTATIONHRMIN))
      {
         ffg_config->computationHrMin = atoi(properties[i].value);
      }	 
      else if (!strcmp(properties[i].name, FORECASTTIMEUNIT))
      {
         ffg_config->forecastTimeUnit = atoi(properties[i].value);
      }	
      else if (!strcmp(properties[i].name, TIMERANGEINDICATOR ))
      {
         ffg_config->timeRangeIndicator = atoi(properties[i].value);
      }	
      else if (!strcmp(properties[i].name, SUBCENTERID ))
      {
         ffg_config->subCenterId = atoi(properties[i].value);
      }	
      else if (!strcmp(properties[i].name, DECIMALSCALEFACTOR ))
      {
         ffg_config->decimalScaleFactor = atoi(properties[i].value);
      }	
      else if (!strcmp(properties[i].name, PROJECTIONTYPE ))
      {
         ffg_config->projectionType = atoi(properties[i].value);
      }	
      else if (!strcmp(properties[i].name, LATOFFIRSTGRIDPOINT ))
      {
         ffg_config->latOfFirstGridPoint = atoi(properties[i].value);
      }	
      else if (!strcmp(properties[i].name, LONOFFIRSTGRIDPOINT ))
      {
         ffg_config->lonOfFirstGridPoint=atoi(properties[i].value);
      }
      else if (!strcmp(properties[i].name, LONORIGIN ))
      {
         ffg_config->lonOrigin = atoi(properties[i].value);
      }	
      else if (!strcmp(properties[i].name, XCELLSIZE ))
      {
         ffg_config->xCellSize =atoi(properties[i].value);
      }	
      else if (!strcmp(properties[i].name, YCELLSIZE ))
      {
         ffg_config->yCellSize = atoi(properties[i].value);
      }	
      else if (!strcmp(properties[i].name, INPUTNETCDFFILE ))
      {
         strcpy(ffg_config->inputNetcdfFile, properties[i].value);
      }
      else if (!strcmp(properties[i].name, OUTPUTGRIB1FILEPREF ))
      {
         strcpy(ffg_config->outputGrib1FilePref, properties[i].value);
      }
      else if (!strcmp(properties[i].name, DIAGFILENAME))
      {
         strcpy(ffg_config->diagFileName, properties[i].value);
      }
      else if (!strcmp(properties[i].name, DEBUGFLAG ))
      {
         ffg_config->debugFlag = atoi(properties[i].value);
      } 
   }
  
  return;
}			     
