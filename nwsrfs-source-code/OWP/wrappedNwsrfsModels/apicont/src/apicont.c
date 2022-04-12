/******************************************************************************
   Filename: apicont.c

   Description:
   ============
      This main function for the API-CONT operation (Continuous Incremental API 
      operation #24) or FFG operation
      - Read the arguments passed in from the java
      - Load the input time series frist.
      - Read the current parameter file (params.txt) using the PIN24 routine
      - Check if do not perform carryover transfer, the actual states are read
        from statesI.txt file.
      - Get required input time series (ts.txt) file and pass to Fortran module
      - Read input states (statesI.txt)
      - Performs the AEICQN, AIADJ, APICBASF and APICCO mods
      - Execute the API-CONT operation.
      - Write the TimeSeries results into output.txt file
      - Write state(s) information into StatesO.txt file      

   Inputs:

   Outputs:

   Change History
   ==============
   DATE          VERSION    PROGRAMMERS        NOTES
   ----------    -------    -----------------  ----------------------
   05/04/09      1          Cham Pham          Initial implementation
   12/22/09      2          Cham Pham          Added FFG operation
   07/22/10      3          Cham/Varalakshmi   Fixed memory leak
******************************************************************************/
#include <time.h>
#include "apicont.h"

int main ( int argc, char **argv )
{
   clock_t start, end;
   double cpu_time_used;

   int   i;

   /* Initialize 5-input timeseries variables */
   float **pts = (float **)malloc(sizeof(float *)); 
   *pts = NULL;
   float **pets = (float **)malloc(sizeof(float *));
   *pets = NULL;
   float **scts = (float **)malloc(sizeof(float *));	
   *scts = NULL;
   float **wets = (float **)malloc(sizeof(float *)); 
   *wets = NULL;
   float **tats = (float **)malloc(sizeof(float *));
   *tats = NULL;

   /* Initialize 11-output timeseries variables */
   int count1=0,
       count2=0,
       count3=0,
       count4=0;
   int res1=0,
       res2=0,
       res3=0,
       res4=0;
   float *rts = NULL,
	 *rsts = NULL,
	 *rgts = NULL,
	 *aits = NULL,
	 *apits = NULL,
	 *fits = NULL,
	 *apicts = NULL,
	 *aeits = NULL,
	 *atits = NULL,
	 *feits = NULL,
	 *frsts = NULL;
   
   /* Variables for parameter and state(s) */
   float pArrayCurr[PSIZE], cArrayCurr[CSIZE];
   float pArrayPrev[PSIZE], cArrayPrev[CSIZE];
   int   doCOX = FALSE;
   
   /* Variables for Mod */
   int numMods = 0;
   int modFileSize = -1;
   /* mape data user set timestep from property*/
   int _mapeTimeStep = 24;
   

   /* Get the start time to run model */
   start = clock();
   
   /* Check options sent to program */
   if ( readOptions(argc, argv) < 0 )
   {
      exit(0);
   }
  
   /* Get infomation from arguments (arguments.txt) */
   setInformationFromArguments( argv[1] );
   
   struct ModelArgs modelArgs = getModelArgs();
   
   
   logMessage( DEBUG_LEVEL, "Start API-CONT model ..." ); 
   
   _mapeTimeStep = modelArgs.mapeTimeStep;
   if(modelArgs.mapeTimeStep < 0 || modelArgs.mapeTimeStep > 24 )
   {
     
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL, "API-CONT - "
         "invalid Property set in argument: 'mapeTimeStep' = [%d] hour", _mapeTimeStep );     
	 
   }   
   _mapeTimeStep = modelArgs.mapeTimeStep;
   
   if(modelArgs.mapeTimeStep == 0)
   {
      _mapeTimeStep = 24;
   }
   
   
   
   /* Read params file ----------------------------------------------------*/
   pin24_( pArrayCurr, cArrayCurr, getParamFileName() );

   /* Determine if carryover transfer is to be performed ------------------*/
   if ( strcmp(getPrevParamsInfo(), "PARAMS_UNCHANGED") != 0 )
   {
      /* Set doCOX variale to TRUE if do carryover transfer. */
      doCOX = TRUE;

      /* Read the previous/saved states parameter file (params_previous.txt)
       * using the PIN24 routine
       */
      
      pin24_( pArrayPrev, cArrayPrev, getPrevParamsInfo() );

      /* Initialize the COprevious array since the actual states are read
       * from statesI.txt unlike the pin routine filling the values from
       * the params file
       */
      memset( cArrayPrev, 0., sizeof(cArrayPrev) );

      /* Read the previous/saved states (carryover) from statesI.txt file */
      readStatesApiCont( pArrayPrev, cArrayPrev );

      /* Peform carryover transfer by executing COX24 routine */
      cox24_( pArrayPrev, cArrayPrev, pArrayCurr, cArrayCurr );
      
   }

   /* If do NOT perform Carryover transfer, we need to read the actual states
    * from statesI.txt.
    */
   if ( doCOX == FALSE )
   {
      /* Initialize the COcurrent array since the actual states are read from
       * (statesI.txt) unlike the pin routine filling the values from the
       * params file 
       */
      memset( cArrayCurr, 0., sizeof(cArrayCurr) );

      /* Read the current states (carryover) from statesI.txt file */
      readStatesApiCont( pArrayCurr, cArrayCurr );
      
   }
   

   /* Load the time series first ------------------------------------------*/
   readAllInputTimeSeries( );

   /*======================================================================*/
   /* RUN FFG operation (Flash Flood Guidance)                             */
   /*======================================================================*/
   /*struct ModelArgs modelArgs = getModelArgs();*/
   int _ffgIDT =  modelArgs.FFGdurationHR;

   if ( _ffgIDT > 0 )
   {

      /* total runoff (INFW) */
      float roff = 0.0;

      /* preciptation and air temperature input TS */
      float **airtemp = (float **)malloc(sizeof(float *));
      *airtemp = NULL;
    
      float precip;

      /* reset time interval (in hr) of precip and runof time series. */
      pArrayCurr[6] = _ffgIDT;

      logMessageWithArgsAndExitOnError( DEBUG_LEVEL, "FFG: API-CONT - "
      "with [%d] hour duration", _ffgIDT );

      /* Read input time series */
      readTsFFG( pArrayCurr, &precip, airtemp );

      /* Execute FFG operation */
      ffg_( pArrayCurr, cArrayCurr, &precip, *tats, &_ffgIDT, &roff );

      /* Write the output time series to file ----------------------------*/
      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
         "Write runoff value to file");
      }

      /* Open the output TS file */
      FILE *outputTsFilePtr = fopen( getOutputTsFileName(), "w+" );

      if ( outputTsFilePtr == NULL )
      {
         logMessageWithArgsAndExitOnError( FATAL_LEVEL,
         "ERROR: Could not open %s\n", getOutputTsFileName() );
      }

      /* INFW time series  */
      char *Id = getTimeSeriesIdFromPArray( pArrayCurr, 11 );
      char *dataType = getTimeSeriesCodeFromPArray( pArrayCurr, 13 );

      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
	 "1-id = %s datatype = %s", Id, dataType );
      }

      if ( Id != NULL && !strcmp(dataType, "INFW") )
      {
	 writeOneTimeSeries( outputTsFilePtr, Id, dataType, _ffgIDT,
	                     &roff, 1 );
      }

      /* Close output time series outputs.txt file */
      fclose( outputTsFilePtr );

      free( airtemp );
   
      freeTimeSeries();

      /* Compute CPU time for model run */
      end = clock();
      cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;

      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "===> TOTAL RUN TIME FFG FORTRAN MODEL: %.2f (seconds)", 
      cpu_time_used );

      closeDiagFile();

      return ( SUCCESS );

   }   
   /*========= DONE FFG ===================================================*/
   
   /* Get require input TS ------------------------------------------------*/
   readInputTS( pArrayCurr, pts, pets, scts, wets, tats, _mapeTimeStep );

   /* Allocate memory spaces for outputs time series ----------------------*/
   
   /* Time interval of precipitation and runoff ts ********/
   res1 = (int)pArrayCurr[6]; 
   if ( res1 > 0 )
   {
      count1 = getNumberOfElementsInTimeSeries( res1 );
      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
         "infw, suro, psro, gwro - res1: %d count1: %d", res1, count1 );
      }
   }
   /* total runoff (INFW) */
   rts  = (float *)calloc( count1+1, sizeof(float) );

   /* storm runoff (SURO) */
   if ( (int)pArrayCurr[18] > 0 )
   {
      rsts = (float *)calloc( count1+1, sizeof(float) );
   }
   
   /* percent surface runoff (PSRO) */
   if ( (int)pArrayCurr[30] > 0 )
   {
      frsts = (float *)calloc( count1+1, sizeof(float) );
   }
   
   /* ground water runoff (GWRO) */
   if ( (int)pArrayCurr[19] > 0 )
   {
      rgts = (float *)calloc( count1+1, sizeof(float) );
   }

   /* Time interval of AIAI, APIS, APIC, AEIS and ATI ******/
   /* antencedent index (AIAI) */
   if ( (int)pArrayCurr[20]  > 0 )
   {
      res2 = (int)pArrayCurr[(int)(pArrayCurr[20]+3)-1];
      count2 = getNumberOfElementsInTimeSeries( res2 );
      
      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
         "aiai - res2: %d count2: %d", res2, count2 );
      }
      aits = (float *)calloc( count2+1, sizeof(float) );
   }
   
   /* (APIS) */
   if ( (int)pArrayCurr[21]  > 0 )
   {
      res2 = (int)pArrayCurr[(int)(pArrayCurr[21]+3)-1]; 
      count2 = getNumberOfElementsInTimeSeries( res2 );
      
      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
         "apis - res2: %d count2: %d", res2, count2 );
      }
      apits = (float *)calloc( count2+1, sizeof(float) );
   }
   
   /* API content (APIC) */
   if ( (int)pArrayCurr[28] > 0 )
   {
      res2 = (int)pArrayCurr[(int)(pArrayCurr[28]+3)-1]; 
      count2 = getNumberOfElementsInTimeSeries( res2 );
      
      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
         "apic - res2: %d count2: %d", res2, count2 );
      }
      apicts = (float *)calloc( (count2+1) * FIVE_VALTS, sizeof(float) );
   }

   /* AEI and atencedent temp index (ATI) */
   if ( (int)pArrayCurr[29] > 0 )
   {
      res2 = (int)pArrayCurr[(int)(pArrayCurr[29]+3)-1]; 
      count2 = getNumberOfElementsInTimeSeries( res2 );
      
      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
         "aei, ati - res2: %d count2: %d", res2, count2 );
      }
      aeits = (float *)calloc( count2+1, sizeof(float) );
      atits = (float *)calloc( count2+1, sizeof(float) );
   }

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
      "===> res2: %d count2: %d", res2, count2 );
   }
   
   /* Check if frozen ground is  considered, then get time interval of
    * FI and FEI                                           
    */
   if ( (int)pArrayCurr[23] > 0 )
   {
      /* frost index (FGIX) */
      res3= (int)pArrayCurr[(int)(pArrayCurr[23]+3)-1]; 
      if ( res3 > 0 )
      {
         count3 = getNumberOfElementsInTimeSeries( res3 );  

	 if ( getFewsDebugFlag() > 3 )
	 {
            logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
            "figx - res3: %d count3: %d", res3, count3 );
	 }
         fits = (float *)calloc( count3+1, sizeof(float) );
      }
   
      /* frost efficienly index (FEIX) */
      res4= (int)pArrayCurr[(int)(pArrayCurr[23]+7)-1]; 
      if ( res4 > 0 )
      {
         count4 = getNumberOfElementsInTimeSeries( res4 );  
         
	 if ( getFewsDebugFlag() > 3 )
	 {
            logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
	    "feix - res4: %d count4: %d", res4, count4 );
	 }
         feits = (float *)calloc( count4+1, sizeof(float) );
      }
   }

   /* Performs the AEICQN, AIADJ, APICBASF and APICCO Mods */
   numMods = parseModsFile( getModFileName(), &modFileSize );

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "Including mods from file = %s\n", getModFileName() );
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "Mods file size is = %d\n", modFileSize );
   }

   if ( modFileSize <= 0 || numMods <= 0 )
      numMods = 1; //need to set array size for using in fortran module

   mod24_( pArrayCurr, &modFileSize, &numMods );

   close_mod_file();
   
   /* Execute API-CONT operation ------------------------------------------*/
   ex24_( pArrayCurr, cArrayCurr, *pts, rts, *pets, *scts, *wets, *tats, 
	  rsts, rgts, aits, apits, fits, apicts, aeits, atits, feits, frsts );

   /* Write the output time series to file --------------------------------*/
   writeOutputTS( pArrayCurr, rts, rsts, rgts, aits, apits, fits, apicts, 
	          aeits, atits, feits, frsts, res1, res3, res4,
		  count1, count3, count4 );
		    
   /* Free memories allocated */
   free ( pts );
   free ( pets );
   free ( scts );
   free ( wets );
   free ( tats );
   free ( apicts );

   if ( rts != NULL ) 
      free ( rts );

   if ( rsts != NULL )
      free ( rsts );

   if ( rgts != NULL )
      free ( rgts );

   if ( aits != NULL )
      free ( aits );

   if ( apits != NULL )
      free ( apits );

   if ( aeits != NULL )
      free ( aeits );

   if ( atits != NULL )
      free ( atits );
   
   if ( fits != NULL )
      free ( fits );
   
   if ( feits != NULL )
      free ( feits );

   if ( frsts != NULL )
      free ( frsts );

   freeTimeSeries();

   /* Write states to statesO.txt file ------------------------------------*/
   writeStatesApiCont( cArrayCurr, getOutputStateFileName() );

   freeStates();

   logMessage(DEBUG_LEVEL, "Exit API-CONT model\n" );

   /* Compute CPU time for model run */
   end = clock();
   cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;

   logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
   "===> TOTAL RUN TIME FORTRAN MODEL: %.2f (seconds)", cpu_time_used );
   
   closeDiagFile();

   return ( SUCCESS );
   
}/* main() ----------------------------------------------------------------- */
