#ifndef _FFGGRIBPRODUCT_H
#define _FFGGRIBPRODUCT_H
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

#include "netcdf.h"
#include "packgrib.h"
#include "unpackgrib.h"
#include "cmapf.h"
#include "utilities.h"
 

/* define the constants */
#define GRIB_META_DATA_LEN  43
#define MISSING_VAL  -999
#define COPYSIZE  6200000
#define MAX_GRIB_FILE_NUM 5
#define MAX_FILE_LEN 1024
#define SITE_ID_LEN   4
#define WMO_ID_LEN    6
#define WMO_HEADER_LEN  18
#define DEBUG_LEVEL_FLAG    3

/* define the error-related constants */
#define MALERR    -10	    /* memory allocation error */
#define SUBERR    -4        /* subroutine return error */
#define OPENERR   -2        /* this is an open file error */
#define WRITERR   -5        /*writing to grib file error or grib file length is 0*/

static char *dur_suffix[] = {"1","3","6","12","24"};

/* structure of ffgDataStr */
struct ffgDataStr {                  /* dimension */
  int    time_size;
  int    x_size;
  int    y_size;
  double *timeArray;
  float  *xArray;
  float  *yArray;
  float  *ffgArray;
  int    latOfFirstGridPoint;
  int    lonOfFirstGridPoint;
  int    lonOrigin;
  int    xCellSize;
  int    yCellSize;
};
struct ffgDataStr ffgData;

/* define structure that includes the contents in ffg_configuration.txt file */

typedef struct {
       char inputNetcdfFile[FILE_NAME_LENGTH];
       char outputGrib1FilePref[FILE_NAME_LENGTH];
       char diagFileName[FILE_NAME_LENGTH];
       int  debugFlag;
       char siteId[SITE_ID_LEN+1];
       char wmoId[WMO_ID_LEN+1];
       int  processId;      
       int  computationYear;
       int  computationMon;
       int  computationDay;
       int  computationHrMin;
       int  forecastTimeUnit;
       int  timeRangeIndicator;
       int  subCenterId;
       int  decimalScaleFactor;
       int  projectionType;
       int  latOfFirstGridPoint;
       int  lonOfFirstGridPoint;
       int  lonOrigin;
       int  xCellSize;
       int  yCellSize;
}ffgConfigStr;

ffgConfigStr *ffg_config;

/* function prototype */
void decodeFFG ( char *fname );

void define_grib_metadata(int *grib_lbl,                          
			  int duration_cnt );			  			  
			  
void output_grib(char *grib1FN, 
                size_t *output_buffer,
		size_t length);

int write_grib_tofile(FILE *grib_file_ptr,
                      size_t *output_buffer,
		      size_t  length);			      
		       
void retrieveFFGConfig(char *fname);

void dump_test_grib(char *grib1FN);


#endif
