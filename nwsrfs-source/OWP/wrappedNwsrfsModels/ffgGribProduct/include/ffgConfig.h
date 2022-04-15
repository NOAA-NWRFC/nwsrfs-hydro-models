#ifndef _FFGCONFIG_H
#define _FFGCONFIG_H
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/stat.h>
#include <limits.h>
#include <malloc.h>

/* define the constants */
#define INPUTNETCDFFILE   "inputNetcdfFile"
#define OUTPUTGRIB1FILEPREF  "outputGrib1FilePref"
#define DIAGFILENAME        "diagFileName"
#define DEBUGFLAG   "debugFlag"
#define SITEID     "siteId"
#define WMOID      "wmoId"
#define PROCESSID  "processId"
#define COMPUTATIONYEAR   "computationYear"
#define COMPUTATIONMON    "computationMon"
#define COMPUTATIONDAY    "computationDay"
#define COMPUTATIONHRMIN  "computationHrMin"
#define FORECASTTIMEUNIT  "forecastTimeUnit"
#define TIMERANGEINDICATOR "timeRangeIndicator"
#define SUBCENTERID        "subCenterId"
#define DECIMALSCALEFACTOR "decimalScaleFactor"
#define PROJECTIONTYPE     "projectionType"
#define LATOFFIRSTGRIDPOINT "latOfFirstGridPoint"
#define LONOFFIRSTGRIDPOINT "lonOfFirstGridPoint"
#define LONORIGIN           "lonOrigin"
#define XCELLSIZE           "xCellSize"
#define YCELLSIZE           "yCellSize"

int open_ffgConfig_file(char *fname,
                        FILE *ffgConfig_file_ptr);
						     			     
void define_ffgConfig_struct(int           numOfProperties,
                             Properties    *properties);		
			     
			            
#endif
