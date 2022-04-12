/********************************
  Header file for rsnwelev.c
*********************************/
#ifndef _RSNWELEV_42
#define _RSNWELEV_42

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <math.h>
#include <string.h>

#include "logging.h"
#include "utilities.h"
#include "timeseries.h"
#include "states.h"
#include "fortranincludes.h"

int outputCount;

void getRequiredInputTimeSeries(float *parray, float **airTempTsData, float **rnswElevTsData);

#endif
