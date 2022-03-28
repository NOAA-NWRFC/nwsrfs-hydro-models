/******************************************************************************
   Filename: sacsmaht.c

   Description:
   ============
      This main function for the SACSMA-HT (Sacramento Soil Moisture Accounting
      with Heat Transfer) operation.
      - Read the arguments passed in from the java
      - Load the input time series frist.
      - Read the current parameter file (params.txt) using the PIN1 routine
      - Get required input time series (ts.txt) file and pass to Fortran module
      - Read input states (statesI.txt)
      - Execute the SACSMA-HT operation.
      - Write the TimeSeries results into output.txt file
      - Write state(s) information into StatesO.txt file      

   Inputs:

   Outputs:

   Change History
   ==============
   DATE          VERSION    PROGRAMMERS        NOTES
   ----------    -------    -----------------  ----------------------
   02/18/09      1          Cham Pham          Initial implementation

******************************************************************************/
#include <time.h>
#include "sacsmaht.h"

int main ( int argc, char **argv )
{
   clock_t start, end;
   double cpu_time_used;

   const int MAX_SOILLAYER = 10; 

   int i, numTimeSteps, numSoilLayer;
   float pArray[PSIZE], cArray[CSIZE];
   float *runoffCInflow = NULL,
	 *frzGIndex = NULL,
	 *frzDepth = NULL,
	 *uztwc = NULL,
	 *uzfwc = NULL,
	 *lztwc = NULL,
	 *lzfsc = NULL,
	 *lzfpc = NULL,
	 *adimc = NULL;
   float *soilMoisture = NULL,
         *soilTemp = NULL;

   TimeSeries *tsListPtr;
   
   start = clock();
   
   /* Check options sent to program */
   if ( readOptions(argc, argv) < 0 )
   {
      exit(0);
   }

   /* Get infomation from arguments (arguments.txt) */
   setInformationFromArguments( argv[1] );

   /* Load the time series first */
   tsListPtr = readAllInputTimeSeries( );

   /* Read params file */
   pin1_( getParamFileName(), pArray );

   /* Get require input TS */
   getRequiredInputTS( pArray, &numTimeSteps );

   /* Read states from statesI.txt */
   readStatesSacsmaHt( pArray, cArray );

   /* Allocate memory spaces for outputs time series */
   runoffCInflow = malloc( sizeof(float) * numTimeSteps );
   frzGIndex = malloc( sizeof(float) * numTimeSteps );
   frzDepth = malloc( sizeof(float) * numTimeSteps );
   uztwc = malloc( sizeof(float) * numTimeSteps );
   uzfwc = malloc( sizeof(float) * numTimeSteps );
   lztwc = malloc( sizeof(float) * numTimeSteps );
   lzfsc = malloc( sizeof(float) * numTimeSteps );
   lzfpc = malloc( sizeof(float) * numTimeSteps );
   adimc = malloc( sizeof(float) * numTimeSteps );
   soilMoisture = (float*)malloc( numTimeSteps*MAX_SOILLAYER*sizeof(float) );
   soilTemp = (float*)malloc( numTimeSteps*MAX_SOILLAYER*sizeof(float) );

   /* Execute SACSMA-HT operation */
   ex1_( pArray, cArray, runoffCInflow, frzGIndex, frzDepth,
         uztwc, uzfwc, lztwc, lzfsc, lzfpc, adimc, 
	 soilMoisture, soilTemp );

   /* Write the output time series to file */
   writeOutputTS( pArray, runoffCInflow, frzGIndex, frzDepth,
	          uztwc, uzfwc, lztwc, lzfsc, lzfpc, adimc,
	          soilMoisture, soilTemp );

   /* Write states to statesO.txt file */
   writeStatesSacsmaHt( pArray, cArray, getOutputStateFileName() );

   /* Free memories */
   free( runoffCInflow );
   free( frzGIndex );
   free( frzDepth );
   free( uztwc );
   free( uzfwc );
   free( lztwc );
   free( lzfsc );
   free( lzfpc );
   free( adimc );
   free( soilMoisture );
   free( soilTemp );

   logMessage(DEBUG_LEVEL, "Exit SACSMAHT program" );

   end = clock();
   cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
   logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
   "===> TOTAL RUN TIME FORTRAN MODEL: %.2f (seconds)", cpu_time_used );


   return ( SUCCESS );
   
}/* main() ----------------------------------------------------------------- */
