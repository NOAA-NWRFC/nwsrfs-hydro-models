/******************************************************************************
   Filename: stateRessngl_main.c

   Description:
   ============
      The main function for creating the state for RES-SNGL operation (#26).
      1. Read the arguments passed in from the java
      2. Set up any common blocks to pass in FORTRAN input file.
      3. Read the parameter file using the PIN26  routine
      4. Create input states "statesI.txt" file 
       
   Inputs: 
   
   Outputs:

   Change History
   ==============
   DATE          VERSION    PROGRAMMERS        NOTES
   ----------    -------    -----------------  ----------------------
   09/15/08      1          Cham Pham          Initial implementation

******************************************************************************/
#include "stateRessngl.h"

int main( int argc, char **argv )
{
   float Params[MAXP], WorkSpace[MAXD];
   float CarryOverArray[MAXC];
   int   MD = (int)MAXD;
     
   char *StateFileName = (char *)calloc(256, sizeof(char));
   char *ParamFilename = (char *)calloc(256, sizeof(char));

   if ( readOptions(argc, argv) < 0 )
   {
      exit( 0 );
   }

   /* Initialize p and c arrays */
   memset( Params, 0, sizeof(Params) );
   memset( CarryOverArray, 0, sizeof(CarryOverArray) );
   memset( WorkSpace, 0 , sizeof(WorkSpace) );

   /* Get Params file name */
   strncpy( ParamFilename, argv[1], strlen(argv[1]) ); 
   ParamFilename[strlen(ParamFilename) + 1] = '\0';
   
   /* Get State filename */
   strncpy( StateFileName, argv[2], strlen(argv[2]) );
   StateFileName[strlen(StateFileName) + 1] = '\0';

   /* Get Diagnostic file name */
   setDiagFileName( argv[3] );
   getDiagFileName( );

   /* Read Data Type Mapping File */
   setDataTypeFileName( argv[4] );
   readDataTypeMappingFile( );

   /* Set up any common blocks to pass in fortran input text file and
    * model_setup
    */
   modelsetupinit_( ParamFilename );
	  
   /* Read the parameter file using the pin routine */
   pin26_( Params, CarryOverArray, WorkSpace, &MD );

   /* Create input states (carryover) statesI.txt file */
   writeStateRessngl( Params, CarryOverArray, StateFileName );

   /* Free memory */
   free( StateFileName );
   free( ParamFilename );

   logMessage(DEBUG_LEVEL, "Exit genStatesRessngl program" ); 

   return ( SUCCESS );

}/* main() ----------------------------------------------------------------- */
