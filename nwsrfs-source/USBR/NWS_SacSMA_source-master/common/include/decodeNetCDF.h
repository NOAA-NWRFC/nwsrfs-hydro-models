#ifndef _DECODEFFGNETCDF_H
#define _DECODEFFGNETCDF_H

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
#include <netcdf.h>

#include "utilities.h"


#define MAX_NC_DIMS NC_MAX_DIMS

/* Declare global variables */
int _ncid;
int _ndims;                    /* number of dimensions */
int _nvars;                    /* number of variables */
int _ngatts;                   /* number of attributes */
int _unlimdim;

struct ncdim{                  /* dimension */
   char  name[NC_MAX_NAME];
   size_t  size;
};
struct ncdim dims[MAX_NC_DIMS];

struct ncvar{                  /* variable */
   char name[NC_MAX_NAME];
   nc_type type;
   int ndims;
   int dims[NC_MAX_VAR_DIMS];
   int natts;
   int has_fillval;
   double fillval;
};
struct ncvar var;


void openAndInquiryNETCDF( char *filename );

void closeNetCDF();

void getDimension( );

void getNetcdfData( char strname[], int *sizeArr, size_t **anyValues );

void getAttributeValue( char *varName, char *attName, int *attLen,
                        size_t **vals );

#endif
