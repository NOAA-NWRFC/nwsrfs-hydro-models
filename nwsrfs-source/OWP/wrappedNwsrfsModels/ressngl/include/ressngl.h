#ifndef _RESSNGL_26_
#define _RESSNGL_26_

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
#include "parse_mod_file.h"

#define TRUE  1
#define FALSE 0
#define MAXC  3000
#define MAXP  10000

#define TSINPUT          0
#define TSOUTPUT         1
#define NWSRFS_ID_LEN    8 
#define NWSRFS_TYPE_LEN  4 
#define MAXINPUT_TS      15  /* general 3; spec(s/u) 12 */
#define MAXOUTPUT_TS     21  /* general 3; spec(s/u) 18 */
#define MXTSIO           (MAXINPUT_TS + MAXOUTPUT_TS)
#define NUMARGUMENTSTOPROGRAM  2

/* Used by ressngl.c */
void getCurr_IdTypeTStep( char *dataId, char *dataType, int dataTimeStep,
                          char currId[], char currType[], int *timeStep );

/* for model_setup dir - FORTRAN language */
void pin26_( float *po, float *co, float *work, int *nwork ); 

void cox26_( float *pold, float *cold, float *pnew, float *cnew );

void wksp26_( float po[], int *nwksp );

void modelsetupinit_( char filename[] );

/* for model_executable dir - FORTRAN language */
void modelexecinit_( int *modelTimeStep, int *mod_file_size,
                     float *vm126, int *numMods, int *regTech );

void ex26_( float *po, float *co, float *d, int *idpt, float *w, float *vm126, 
            int *numHrYrArray );

/* for utils dir that used by ressngl.c file - C language */
void readStateRessngl( float POarray[], float COarray[] );

void writeStateRessngl( float POarray[], float COarray[],
                        char* outputStatesFileName );

/* for utils dir - FORTRAN language */
void create_data_arrays_( float po[], char dataids[][8], char datatype[][4],
                          int datatimesteps[], int datainorout[],
                          int *totio );

/* routines from common directory */
extern char *getModFileName();
extern int parseModsFile(char *fileName, int *modFileSize);
#endif
