#ifndef _SSARRESV_H_
#define _SSARRESV_H_


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>
#include <ctype.h>
#include <malloc.h>

#include "fortranincludes.h"
#include "logging.h"
#include "timeseries.h"
#include "utilities.h"
#include "states.h"
#include "parse_mod_file.h"

#define TRUE                    1
#define FALSE                   0
#define MAXC 			3000
#define MAXP 			10000


#define NUM_ARGUMENTS_TO_PROGRAM   2
#define NWSRFS_ID_LENGTH	8
#define NWSRFS_TYPE_LENGTH 	4
#define MAXINPUT_TS		10
#define MAXOUTPUT_TS		20

#define SINGLERESERVOIR		5
#define TWORESERVOIR		9
#define STATIONBACKWATER        3
#define ENGLISH			0
#define METRIC			1
#define TS_INPUT	0
#define TS_OUTPUT	1

#define DS_RES_INST_IN_STRING "DOWNSTREAM_RESERVOIR_INST_INFLOW"
#define DS_RES_INST_OUT_STRING "DOWNSTREAM_RESERVOIR_INST_OUTLOW"
#define DS_RES_ELEVATION_STRING "DOWNSTREAM_RESERVOIR_ELEVATION"
#define DS_RES_STORAGE_STRING "DOWNSTREAM_RESERVOIR_STORAGE"
#define DS_RES_NOT_USED_STRING "DOWNSTREAM_RESERVOIR_NOT_USED"

#define US_RES_OUTFLOW_STRING "UPSTREAM_RESERVOIR_OUTLOW"
#define US_RES_ELEVATION_STRING "UPSTREAM_RESERVOIR_ELEVATION"
#define US_RES_STORAGE_STRING "UPSTREAM_RESERVOIR_STORAGE"
#define TRIB_FLOW_INTO_DS_RES_STRING "TRIB_FLOW_INTO_DOWNSTREAM_RESERVOIR"

void store_currentid_type_and_timestep( char *dataId, char *dataType, 
                                        int dataTimeStep, char currId[], 
					char currType[], int *timeStep);

void ssarresv_write_states_output(FILE * filenamePtr, float *pArray, 
                                                      float *cArray);

void readStates(float *pArray, float *cArray);

void ssarresv_populate_states(float cArray[], int numberOfCarryoverValues,
                        int reservoirTypeIndicator);
/* routines from common directory */			
extern char *getModFileName();
extern int parseModsFile(char *fileName, int *modFileSize);

#endif

