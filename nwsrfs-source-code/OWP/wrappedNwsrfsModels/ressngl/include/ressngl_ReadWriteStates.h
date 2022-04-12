#ifndef _RESSNGL_READWRITESTATES_
#define _RESSNGL_READWRITESTATES_

#include <ctype.h>
#include <math.h>
#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "fortranCommonIncludes.h"
#include "logging.h"
#include "states.h"
#include "timeseries.h"
#include "utilities.h"

#define GENERAL_CARRYOVER 6
#define SETQ      102
#define SETH      103
#define FILLSPILL 105
#define SPILLWAY  106
#define STPOOLQ   108
#define INDSRCHGE 111
#define FLASHBDS  112
#define POWERGEN  113 
#define RULEADJ   151
#define ADJUST    154
#define BACKFLOW  155

typedef struct { 
   int icode; 
   int ilevel;
   int iloc; 
   int nco;  
} CarryOver_t;

/* General states */
static char generalCOKey[][80] = { "INSTANTANEOUS_INFLOW",
                                   "MEAN_DISCHARGE",
                                   "INSTANTANEOUS_DISCHARGE",
                                   "POOL_ELEVATION_ONE_PERIOD_BACK",
                                   "POOL_ELEVATION",
                                   "STORAGE_CONTENTS"
                                 };

/* Specific states - Schema and utilities */
static char specificCOKey[][80] = { "SETQ", "SETH", "FILLSPILL",
                                    "SPILLWAY", "STPOOLQ", "INDSRCHGE",
                                    "FLASHBDS", "POWERGEN", "RULEADJ",
                                    "ADJUST", "BACKFLOW"
                                  };

/* Golobal variable(s) used by ressngl_ReadWriteStates.c file */
int COIDX = 0; 

#endif
