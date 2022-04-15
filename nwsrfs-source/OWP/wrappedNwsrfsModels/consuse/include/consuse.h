/********************
Header file for consuse
*************************/

#ifndef _CONSUSE_57
#define _CONSUSE_57

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

void getRequiredInputTimeSeries(float *pArray, float **sqmeTsData, float **matTsData, float** peTsData);

#endif
