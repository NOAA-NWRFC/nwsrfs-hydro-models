/******************************************************************************
   Filename: gen_states_main.c

   Description:
   ============
     generates input states file (statesI.txt ) 
   Inputs: 
   
   Outputs:

   Change History
   ==============
   DATE          VERSION    PROGRAMMERS        NOTES
   ----------    -------    -----------------  ----------------------
   10/14/08                 Ai Vo / Cham Pham  initial implementation
******************************************************************************/

#include "gen_states_main.h"


int main ( int argc, char **argv )
{
    int i, j;
    int numTimeSeries, modelTimeStep, numCOArrayElem;

    ModelArgs *modelArgs;
    int MaxCArray = 3000;
    int MaxPArray = 50000;
    float POarray[MaxPArray], COarray[MaxCArray];
    
    char *statesFileName = (char *)calloc(256, sizeof(char));
    char *paramsFileName = (char *)calloc(256, sizeof(char));
    /* 
     * Check for appropriate input arguments 
     */
    if ( readOptions(argc, argv) < 0 )
    {
       exit(0);
    }

    /* Initialize p and c arrays */
    memset( POarray, 0, sizeof(POarray) );
    memset( COarray, 0, sizeof(COarray) );

    /* create Diagnostic file name */
    setDiagFileName( argv[3] );    
    getDiagFileName();
    
    /* Get Params file name from input arguments*/
    strncpy( paramsFileName, argv[1], strlen(argv[1]) ); 
    paramsFileName[strlen(paramsFileName) + 1] = '\0';
    
    /* Get State filename */
    strncpy( statesFileName, argv[2], strlen(argv[2]) );
    statesFileName[strlen(statesFileName) + 1] = '\0';
   
    /* Read Data Type Mapping File */
    setDataTypeFileName( argv[4] );
    readDataTypeMappingFile( );

    /* Get Data Unit file */
    setDataUnitFileName( argv[5] );
        
    /* Read parameters file - */    
    
    input_parameters58( paramsFileName , POarray, COarray );    
  
    /* Writes carryover information into statesI.txt file */
    resj_writeStates( POarray, COarray, statesFileName );
    
    
    if ( statesFileName != NULL )
       free( statesFileName );

    if ( paramsFileName != NULL )
       free( paramsFileName );

    logMessage(DEBUG_LEVEL, "Exit genStatesResj program" ); 

    closeDiagFile();

    return SUCCESS;
    
} /* end main() ------------------------------------------------------------ */

