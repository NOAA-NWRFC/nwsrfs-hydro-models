/******************************************************************************
     Filename:  writeOutputTsAPICont.c

     Description:
     ============
        This function write out the output time series data to output.txt file 	
     
     Input: 
     -----
     float  pArray[]   - contains output ts information
     float  *rts       - contains INFW ts
     float  *rsts      -          SURO ts
     float  *rgts      -          GWRO ts
     float  *aits      -          AIAI ts
     float  *apits     -          APIS ts
     float  *fits      -          FGIX ts
     float  *apicts    -          APIC ts (API, AI, SMI, BFSC and BFI)
     float  *aeits     -          AEIS ts
     float  *atits     -          ATI  ts
     float  *feits     -          FEIX ts
     float  *frsts     -          PSRO ts
     int    res1       - time interval for precipitation and runoff ts
                         (INFW, SURO, GWRO, and PSRO)
     int    count1     - number time steps for INFW, SURO, GWRO, and PSRO 
     int    res3       - time interval for frost index (FGIX)
     int    count3     - number time steps for FGIX
     int    res4       - time interval for frost efficiency index (FEIX)
     int    count4     - number time steps for FEIX

     Output:
     ------
     - Write all output time series to output.txt file
     
     Change History
     ==============

     DATE          VERSION    PROGRAMMERS           NOTES
     ----------    -------    -----------------     ----------------------
     05/08/09      1          Cham Pham             Initial implementation
                          
******************************************************************************/
#include "apicont.h"

static char apicDT[][5] = { "API", "AI", "SMI", "BFSC", "BFI" };

void writeOutputTS( float pArray[], float *rts, float *rsts, float *rgts,
                    float *aits, float *apits, float *fits, float *apicts,
                    float *aeits, float *atits, float *feits, float *frsts,
                    int res1, int res3, int res4,
                    int count1, int count3, int count4 ) 
{
   int  i, arrayIndx;
   int  res2 = 0;         /* time interval for AIAI, APIS, APIC, AEIS, ATI*/
   int  count2 = 0;
   char *Id = NULL,
       	*dataType = NULL;
   FILE *outputTsFilePtr = NULL;
    
   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "Begin writeOutputTS() ...");
   }

   /* Open the output TS file */
   outputTsFilePtr = fopen( getOutputTsFileName(), "w+" );

   if ( outputTsFilePtr == NULL )
   {
      logMessageWithArgsAndExitOnError( FATAL_LEVEL,
      "ERROR: Could not open %s\n", getOutputTsFileName() );
   }
   
   /* INFW time series  */
   Id = getTimeSeriesIdFromPArray( pArray, 11 );
   dataType = getTimeSeriesCodeFromPArray( pArray, 13 );
   logMessageWithArgsAndExitOnError( DEBUG_LEVEL, "1-id = %s datatype = %s", 
	                                          Id, dataType );
   
   if ( Id != NULL && !strcmp(dataType, "INFW") )
   {
      writeOneTimeSeries( outputTsFilePtr, Id, dataType, res1, rts,
                          count1 );	 
   }

   /* SURO time series */
   arrayIndx = (int)pArray[18];
   if ( arrayIndx > 0 )
   {
      Id = getTimeSeriesIdFromPArray( pArray,  arrayIndx );
      dataType = getTimeSeriesCodeFromPArray( pArray, arrayIndx+2 );

      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
	 "2-id = %s datatype = %s", Id, dataType );
      }

      if ( !strcmp(dataType, "SURO") )
      {
         writeOneTimeSeries( outputTsFilePtr, Id, dataType, res1, rsts, 
	                     count1 );
      }
   }

   /* GWRO time series */
   arrayIndx = (int)pArray[19];
   if ( arrayIndx > 0 )
   {
      Id = getTimeSeriesIdFromPArray( pArray, arrayIndx );
      dataType = getTimeSeriesCodeFromPArray( pArray, arrayIndx+2 );

      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
	 "3-id = %s datatype = %s", Id, dataType );
      }
      
      if ( !strcmp(dataType, "GWRO") )
      {
         writeOneTimeSeries( outputTsFilePtr, Id, dataType, res1, rgts,
	                     count1 );
      }
   }
      
   /* AIAI time series */
   arrayIndx = (int)pArray[20];
   if ( arrayIndx > 0 )
   {
      Id = getTimeSeriesIdFromPArray( pArray, arrayIndx );
      dataType = getTimeSeriesCodeFromPArray( pArray, arrayIndx+2 );

      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
	 "4-id = %s datatype = %s", Id, dataType );
      }
      
      if ( !strcmp(dataType, "AIAI") )
      {
         res2 = (int)pArray[arrayIndx+2];
	 count2 = getNumberOfElementsInTimeSeries( res2 );
         writeOneTimeSeries( outputTsFilePtr, Id, dataType, res2, aits,
	                     count2 );
      }
   }
    
   /* APIS time series */
   arrayIndx = (int)pArray[21];
   if ( arrayIndx > 0 )
   {
      Id = getTimeSeriesIdFromPArray( pArray, arrayIndx );
      dataType = getTimeSeriesCodeFromPArray( pArray, arrayIndx+2 );

      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
	 "5-id = %s datatype = %s", Id, dataType );
      }
      
      if ( !strcmp(dataType, "APIS") )
      {
         res2 = (int)pArray[arrayIndx+2];
	 count2 = getNumberOfElementsInTimeSeries( res2 );
         writeOneTimeSeries( outputTsFilePtr, Id, dataType, res2, apits,
	                     count2 );
      }
   }

   /* FGIX and FEIX time series */ 
   arrayIndx = (int)pArray[23];
   if ( arrayIndx > 0 )
   {
      /* FGIX time series */
      Id = getTimeSeriesIdFromPArray( pArray, arrayIndx );
      dataType = getTimeSeriesCodeFromPArray( pArray, arrayIndx+2 );

      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
	 "6-id = %s datatype = %s", Id, dataType );
      }
      
      if ( !strcmp(dataType, "FGIX") )
      {
         writeOneTimeSeries( outputTsFilePtr, Id, dataType, res3, fits,
	                     count3 );
      }
  
      /* FEIX time series */
      Id = getTimeSeriesIdFromPArray( pArray, arrayIndx+4 );
      dataType = getTimeSeriesCodeFromPArray( pArray, arrayIndx+6 );

      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
	 "7-id = %s datatype = %s", Id, dataType );
      }
      
      if ( !strcmp(dataType, "FEIX") )
      {
         writeOneTimeSeries( outputTsFilePtr, Id, dataType, res4, feits,
	                     count4 );
      }
   }

   /* APIC - Data will store 5-value per time step so our approach was to split
    * them up and write them to FEWS as separate time series (APIC1 thru. APIC5)
    */
   arrayIndx = (int)pArray[28];
   if ( arrayIndx > 0 )
   {
      Id = getTimeSeriesIdFromPArray( pArray, arrayIndx );
      dataType = getTimeSeriesCodeFromPArray( pArray, arrayIndx+2 );
      
      if ( !strcmp(dataType, "APIC") )
      {
         res2 = (int)pArray[arrayIndx+2];
	 count2 = getNumberOfElementsInTimeSeries( res2 );

	 float *apicTS = (float *)malloc( FIVE_VALTS * count2 * sizeof(float) );

         /* Split 5-value per time step to separate time sereis */
	 for ( i = 0; i < count2; i++ )
	 {
	     apicTS[i] = apicts[i*FIVE_VALTS];
	     apicTS[1*count2+i] = apicts[i*FIVE_VALTS+1];
	     apicTS[2*count2+i] = apicts[i*FIVE_VALTS+2];
	     apicTS[3*count2+i] = apicts[i*FIVE_VALTS+3];
	     apicTS[4*count2+i] = apicts[i*FIVE_VALTS+4];
	 }

	 for ( i = 0; i < FIVE_VALTS; i++ )
	 {
	    if ( getFewsDebugFlag() > 3 )
	    {
	       logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
               "8-id = %s datatype = %s", Id, apicDT[i] );
	    }
	    
            writeOneTimeSeries( outputTsFilePtr, Id, apicDT[i], res2, 
                                apicTS + i * count2, count2 );  
	 }
         
	 /* free memory for apicTS */
         if ( apicTS != NULL )
	    free( apicTS );
      }
   }
   
   /* AEIS or ATI time series aeits*/
   arrayIndx = (int)pArray[29];
   if ( arrayIndx > 0 )
   {
      Id = getTimeSeriesIdFromPArray( pArray, arrayIndx );
      dataType = getTimeSeriesCodeFromPArray( pArray, arrayIndx+2 );

      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
	 "9-id = %s datatype = %s", Id, dataType );
      }
      
      if ( !strcmp(dataType, "AEIS") && (int)pArray[13] == 1 )
      {
         res2 = (int)pArray[arrayIndx+2];
         count2 = getNumberOfElementsInTimeSeries( res2 );
         writeOneTimeSeries( outputTsFilePtr, Id, dataType, res2, aeits,
	                     count2 );
      }
      else if ( !strcmp(dataType, "ATI") && (int)pArray[13] == 2 )
      {
         res2 = (int)pArray[arrayIndx+2];
         count2 = getNumberOfElementsInTimeSeries( res2 );
         writeOneTimeSeries( outputTsFilePtr, Id, dataType, res2, atits,
  	                     count2 ); 
      }
   }
   
   /* PSRO time series */
   arrayIndx = (int)pArray[30];
   if ( arrayIndx > 0 )
   {
      Id = getTimeSeriesIdFromPArray( pArray, arrayIndx );
      dataType = getTimeSeriesCodeFromPArray( pArray, arrayIndx+2 );

      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
	 "10-id = %s datatype = %s", Id, dataType );
      }
      
      if ( !strcmp(dataType, "PSRO") )
      {
         writeOneTimeSeries( outputTsFilePtr, Id, dataType, res1, frsts,
	                     count1 );
      }
   }
   
   /* Close output time series outputs.txt file */
   fclose( outputTsFilePtr );

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "End writeOutputTS() ..." );
   }
   
} /* getRequiredInputTS() -------------------------------------------------- */
