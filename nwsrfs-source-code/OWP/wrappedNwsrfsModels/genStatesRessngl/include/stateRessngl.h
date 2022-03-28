#ifndef _STATERESSNGL_
#define _STATERESSNGL_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <stdarg.h>
#include <math.h>
#include <ctype.h>
#include <malloc.h>

#include "fortranCommonIncludes.h"
#include "logging.h"
#include "utilities.h"
#include "states.h"

#define NUMARGUMENTSTOPROGRAM  5
#define MAXC  3000
#define MAXD  20000
#define MAXP  50000

/* for ressngl/src/model_setup dir */
extern void pin26_( float *po, float *co, float *work, int *nwork );

extern void modelsetupinit_( char filename[] );

/* for ressngl/src/utils dir */
extern void writeStateRessngl( float POarray[], float COarray[],
                               char* outputStatesFileName );
#endif
