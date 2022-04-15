/************************
 Header file for sarroute
*************************/

#ifndef _SARROUTE_44
#define _SARROUTE_44


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

void readStates(float* pArray, float *cArray);
void writeStates(float* pArray, float *cArray);
void getRequiredInputTimeSeries(float *pArray,
                                float **qInStTsData, float **qInEnTsData,
                                int driverResolution);
#endif
