#ifndef _MUSKROUT_H_
#define _MUSKROUT_H_

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <ctype.h>
#include <malloc.h>
#include "logging.h"
#include "timeseries.h"
#include "utilities.h"
#include "states.h"

#define POMAX     1000
#define COMAX     3

/*fortrain prototypes */
void pin9_( float *po, float *co, char *filename );
void ex9_( float *po, float *co, float *qin, float *qout );
void cox9_( float *poPrevious, float *coPrevious, 
              float *poCurrent, float *coCurrent );
/* c prototypes */
void readStates( float* pArray, float *cArray );
void writeStates( float* pArray, float *cArray );

#endif
