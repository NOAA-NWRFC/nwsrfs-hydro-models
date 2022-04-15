#ifndef _GLACIER_H_
#define _GLACIER_H_

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <ctype.h>
#include <string.h>

#include "logging.h"
#include "utilities.h"
#include "timeseries.h"
#include "states.h"

#define POMAX     1000 //PO stores 16 fields
#define COMAX     3    //carryover has only 2 values

/*fortrain prototypes */
void pin56_( float *po, float *co, char *filename );
void ex56_( float *po, float *co, float *rminp, float *gout, float *afiout);
void cox56_( float *poPrevious, float *coPrevious, 
              float *poCurrent, float *coCurrent );
/* c prototypes */
void readStates( float *pArray, float *cArray );
void writeStates( float *pArray, float *cArray );

#endif
