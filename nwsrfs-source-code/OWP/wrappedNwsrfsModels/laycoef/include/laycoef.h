/************************
 Header file for laycoef
*************************/

#ifndef _LAYCOEF_11
#define _LAYCOEF_11


#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <math.h>
#include <time.h>
#include <string.h>

#include "logging.h"
#include "utilities.h"
#include "timeseries.h"
#include "states.h"
#include "fortranincludes.h"

int outputCount;

void readStates(float* pArray, float *cArray);
void writeStates(float* pArray, float *cArray);
void getRequiredInputTimeSeries(float *pArray,
                                float **qIn, int driverResolution);
#endif
