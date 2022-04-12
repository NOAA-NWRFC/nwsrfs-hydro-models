#ifndef _APICONT_
#define _APICONT_

#include <ctype.h>
#include <math.h>
#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <stdarg.h>

#include "fortranCommonIncludes.h"
#include "logging.h"
#include "states.h"
#include "timeseries.h"
#include "utilities.h"
#include "parse_mod_file.h"

#define NUM_INPUT   5    /* Require: raim, mape, mat; Option: sasc, and swe */
/*#define NUM_OUTPUT 11     Option: infw, suro, gwro, aiai, apts, fgix, apic, 
			    aeis, ati, feix, and psro */
#define PSIZE 10000
#define CSIZE 100
#define TRUE  1
#define FALSE 0

#define FIVE_VALTS  5
#define NUMARGUMENTSTOPROGRAM  2

/* FORTRAN subroutines */
void ffg_( float *po, float *co, float *precip, float *airtemp, int *idt, 
           float *roff );

void pin24_( float *po, float *co, char *parampath );

void mod24_( float *po, int *mod_file_size, int *numMod );

void ex24_( float *po, float *co, float *pts, float *rts, float *pets, 
            float *scts, float *wets, float *tats, float *rsts, float *rgts,
	    float *aits, float *apits, float *fits, float *apicts, 
	    float *aeits, float *atits, float *feits, float *frsts );

void cox24_( float *poold, float *cold, float *pnew, float *cnew );

/* C functions */
void readTsFFG( float pArray[], float *pts, float **tats );

void readInputTS( float pArray[], float **pts, float **pets, float **scts,
                 float **wets, float **tats , int);

void writeOutputTS( float pArray[], float *rts, float *rsts, float *rgts,
                    float *aits, float *apits, float *fits, float *apicts,
		    float *aeits, float *atits, float *feits, float *frsts,
                    int res1, int res3, int res4, int count1, int count3, 
		    int count4 );

void readStatesApiCont( float poArray[], float coArray[] );

void writeStatesApiCont( float coArray[], char *outputStatesFileName );

void computeDailyMAPE( TimeSeries *inputTS , int inputMAPETimeStep );
/* routines from common code */
extern char *getModFileName();
extern int parseModsFile(char *fileName, int *modFileSize);

#endif
