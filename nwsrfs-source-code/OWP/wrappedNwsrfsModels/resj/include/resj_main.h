#ifndef _RESJ_MAIN_
#define _RESJ_MAIN_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <stdarg.h>
#include <math.h>
#include <ctype.h>
#include <malloc.h>

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif /* C++ */

#include "fortranCommonIncludes.h"
#include "logging.h"
#include "timeseries.h"
#include "utilities.h"
#include "states.h"
#include "resj_states.h"

#define TSINPUT          0
#define TSOUTPUT         1
#define NWSRFS_ID_LEN    9 
#define NWSRFS_TYPE_LEN  5 
#define MAX_TS_IO        30     /* (18 in + 10 out)    */
#define NUMARGUMENTSTOPROGRAM  2
#define MAXCARRAY         3000
#define MAXPARRAY         50000

static char errorStr[] = "0          **ERROR**";
static char warnStr[] = "0**WARNING**";
extern int FewsDebugFlag;

/* routines from common directory */			
extern char *getFileName(char **, int );
extern void setDiagFileName(char *);

/* routines from resj.c file */

/*
void getTS_information ( float PO[], int numTimeSeries,
                         char dataIds[MAX_TS_IO][NWSRFS_ID_LEN],
			 char dataTypes[MAX_TS_IO][NWSRFS_TYPE_LEN],
			 int  timeSteps[MAX_TS_IO], int tsIO[MAX_TS_IO] );
*/
void getTS_information ( float PO[], int numTimeSeries,
                         char** dataIds,
                         char** dataTypes,
                         int* timeSteps, int* tsIO);
void get_currId_currType( char *dataId, char *dataType,     
                          char currId[], char currType[] );

char** allocateTwoDemensionalArrayOfCharType(char** array, int rows, int cols,
      int firstTimeFlag);


/* routine from resj_system directory */
extern void input_parameters58( char* fname, float* PO, float* CO );
extern void carryovertransfer58( float* POprev, float* COprev, float* COcurr, 
                                 float* POcurr, char *prevParamsF, char* currParamsF);

extern void execute58( char* fname, float* CO, float* PO, float* D,
            int* d_index, int* co_da, int* co_hr, int* num_co, int* st_da,
            int* st_hr, int* end_da, int* end_hr, int* ifillc, int* iusec);
                /*AVCP int* ibug, int* ipr, int* iodebug, int* ierr ); */

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif /* C++ */

#endif
