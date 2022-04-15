#ifndef _SACSMAHT_
#define _SACSMAHT_

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

#define PSIZE 100
#define CSIZE 100

#define NUMIN 6    /* required inputs TS: raim for soil only
		      options: sasc, mat, swe, and snsg, mape */

#define DTYPESIZE 5

/* FORTRAN subroutines */
void pin1_( char *parampath, float *po);

void etinit1_( float *po, float *raim, float *sasc, float *mat, float *swe, 
               float *snsg, float *mape, int *datetime, int *periodts );

void ex1_( float *po, float *co, float *tciVals, float *fgixvals, 
           float *frzdvals, float *uztwc, float *uzfwc, float *lztwc,
	   float *lzfsc, float *lzfpc, float *adimc, float *ssm, float *sst );

/* C functions */
void getRequiredInputTS( float pArray[], int *count );

void writeOutputTS( float pArray[], float *tci, float *fgix, float *frzd,
                    float *uztwc, float *uzfwc, float *lztwc, float *lzfsc,
		    float *lzfpc, float *adimc, float *ssm, float *sst );

void readStatesSacsmaHt( float poArray[], float coArray[] );

void writeStatesSacsmaHt( float poArray[], float coArray[], 
                          char *outputStatesFileName );

#endif
