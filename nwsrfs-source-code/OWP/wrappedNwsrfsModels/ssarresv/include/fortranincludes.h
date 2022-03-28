#ifndef _FORTRANINCLUDES_
#define _FORTRANINCLUDES_
/* fortran function prototypes */
void model_setup_init_(char path[]);
void model_execution_init_(int *model_time_step,
                           int *mod_file_size , int *numMods);
void pin51_(float PO[],float CO[],float WK[], int *);
void create_data_arrays_(float po[], char dataIds[][8],
                         char dataTypes[][4],
			 int datatimesteps[]);
void create_data_index_(float po[], int *numtimesteps,
                        int dataindex[],int datainorout[]);
void ex51_(float po[],float co[],float D[],int idpt[],float wk[]);
void wksp51_(float po[], int *nwksp);
void cox51_( float *pold, float *cold, float *pnew, float *cnew);
#endif
