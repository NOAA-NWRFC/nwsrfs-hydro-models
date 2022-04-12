/******************************************************************************
     Filename:  writeOutputTS.c

     Description:
     ============
        This function write out the output time series data to output.txt file 	
     
     Input: 
     -----
     float  pArray   - contain output ts information
     float  *tci     - runoff timeseries(INFW)
     float  *fgix    - frozen ground index (FGIX)
     float  *frzd    - forzen depth (SFGD)
     float  *ssm     - soil moisture (SSM#) at each layer
     float  *sst     - soil temperature (SST#) at each layer

     Output:
     ------

     Change History
     ==============

     DATE          VERSION    PROGRAMMERS           NOTES
     ----------    -------    -----------------     ----------------------
     02/18/09      1          Cham Pham             Initial implementation
                          
******************************************************************************/
#include "sacsmaht.h"

void writeOutputTS( float pArray[], float *tci, float *fgix, float *frzd, 
                    float *uztwc, float *uzfwc, float *lztwc, float *lzfsc,
		    float *lzfpc, float *adimc, float *ssm, float *sst )
{
   int   resolution, drivingTS, numElemTS;
   int   i, j, iver;
   int   numSoilLayer, numUserDefinedSoilLayer;
   char  *Id, *dataType;   
   char  ssmDataType[DTYPESIZE], 
	 sstDataType[DTYPESIZE];

   float *moisture = NULL;
   float *temp = NULL;

   int   *dataDTime = NULL;
   int   *count = NULL;

   FILE *outputTsFilePtr = NULL;
   
   if ( getFewsDebugFlag() > 4 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "Begin getRequiredInputTS() ...\n");
   }

   /* Open the output TS file */
   outputTsFilePtr = fopen( getOutputTsFileName(), "w+" );

   if ( outputTsFilePtr == NULL )
   {
      logMessageWithArgsAndExitOnError( FATAL_LEVEL,
      "ERROR: Could not open %s\n", getOutputTsFileName() );
   }
   
   /* Model time step - po(1) */
   resolution = (int)pArray[0];     
   
   /* Get number of elements time series */
   numElemTS = getNumberOfElementsInTimeSeries( resolution );
   count = &numElemTS;
   
   /* INFW time series  */
   Id = getTimeSeriesIdFromPArray(pArray, 6);
   dataType = getTimeSeriesCodeFromPArray(pArray, 8);

   writeOneTimeSeries( outputTsFilePtr, Id, dataType, resolution, tci,
                       *count );	 

   /* SFGD frozen depth time series */
   writeOneTimeSeries( outputTsFilePtr, Id, "SFGD", resolution, frzd,
	               *count );
   
   /* FGIX frozen ground index time series */
   writeOneTimeSeries( outputTsFilePtr, Id, "FGIX", resolution, fgix,
	               *count );

   /* UZTWC, UZFWC, LZTWC, LZFSC, LZFPC, and ADIMC time series */
   writeOneTimeSeries( outputTsFilePtr, Id, "UZTWC", resolution, uztwc,
                       *count );
   writeOneTimeSeries( outputTsFilePtr, Id, "UZFWC", resolution, uzfwc,
                       *count );
   writeOneTimeSeries( outputTsFilePtr, Id, "LZTWC", resolution, lztwc,
                       *count );
   writeOneTimeSeries( outputTsFilePtr, Id, "LZFSC", resolution, lzfsc,
                       *count );
   writeOneTimeSeries( outputTsFilePtr, Id, "LZFPC", resolution, lzfpc,
                       *count );
   writeOneTimeSeries( outputTsFilePtr, Id, "ADIMC", resolution, adimc,
                       *count );
   
   /* Get number of soil layer */
   numSoilLayer = (int)pArray[21];             /* model calculated */
   numUserDefinedSoilLayer = (int)pArray[23];  /* user defined     */

   if ( numUserDefinedSoilLayer > 0 )
      numSoilLayer = numUserDefinedSoilLayer;
   
   /* Get version number to decide to write out temperature or not */
   iver = (int)pArray[22];
   
   for ( i = 0; i < numSoilLayer; i++ )
   {
      moisture = (float*)malloc( *count * sizeof(float) );
      temp = (float*)malloc( *count * sizeof(float) );

      memcpy( moisture, ssm + (i * *count), *count * sizeof(float) );
      memcpy( temp, sst + (i * *count), *count * sizeof(float) );

      sprintf(ssmDataType, "%s%d","SSM", i );
      ssmDataType[4] = '\0';
      
      sprintf(sstDataType, "%s%d","SST", i );
      sstDataType[4] = '\0';
      
      if ( getFewsDebugFlag() > 4 )
      {	 
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL, "%s - %s\n", 
	                                   ssmDataType, sstDataType );

         for ( j = 0; j < *count; j++ )
	    logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	    "moisture(%d,%d) = %f temp = %f\n", j, i, moisture[j], temp[j] );
      }
      
      writeOneTimeSeries( outputTsFilePtr, Id, ssmDataType, resolution,
	                  moisture, *count );

      if ( iver > 1 ) 
      {
         writeOneTimeSeries( outputTsFilePtr, Id, sstDataType, resolution,
	      	             temp, *count );
      }
      
      /* free memories */
      free( moisture );
      free( temp );
   }
   
   /* Close output time series outputs.txt file */
   fclose( outputTsFilePtr );

   if ( getFewsDebugFlag() > 4 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "End writeOutputTS() ...\n" );
   }
   
} /* getRequiredInputTS() -------------------------------------------------- */
