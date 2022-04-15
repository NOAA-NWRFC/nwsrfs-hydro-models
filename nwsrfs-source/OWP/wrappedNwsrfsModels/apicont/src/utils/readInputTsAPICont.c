/******************************************************************************
     Filename:  readInputTsAPICont.c

     Description:
     ============
        This function read required and options input time series from ts.txt 
     file and pass them to FORTRAN module.	
     
     Input: 
     -----
     float  pArray[] - contain input ts information 

     Output:
     ------
     float  **pts    - RAIM (Precipitation) input ts data in unit/dim MM L 
     float  **pets   - MAPE (Potential evaporation)                   MM L
     float  **scts   - SASC (Areal snow cover)                        DEGC DLES
     float  **wets   - SWE  (Water-equivilant)                        MM L
     float  **tats   - MAT  (Air temperature)                         DEGC TEMP
     
     Change History
     ==============

     DATE          VERSION    PROGRAMMERS           NOTES
     ----------    -------    -----------------     ----------------------
     05/08/09      1          Cham Pham             Initial implementation
                          
******************************************************************************/
#include "apicont.h"

void readInputTS( float pArray[], float **pts, float **pets, float **scts,
                 float **wets, float **tats, int mapeTimeInterval )
{
   char  *Id = NULL, 
	 *dataType = NULL;
   
   int   timeInterval; 
   int   pLocation;
   int   count;
   
   int   i;
  
   TimeSeries *inputTS = NULL;
   

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "START readInputTS() ...\n");
   }

   /* RAIM (Required) time series is the driver input TS for this model */
   Id = getTimeSeriesIdFromPArray( pArray, 8 );
   dataType = getTimeSeriesCodeFromPArray( pArray, 10 );
   timeInterval = (int)pArray[6];     
   
   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
      "id: %s type: %s tstep: %d", Id, dataType, timeInterval );
   }
   
   count = getNumberOfElementsInTimeSeries( timeInterval );
   inputTS = getOneTimeSeries( Id, dataType, timeInterval, &count );
   *pts = inputTS->value;

   /* MAPE time series is required only if AEI, ATI or frozen ground options
    * used
    */
   pLocation = (int)pArray[14];
   if ( pLocation > 0 ) 
   {
      Id = getTimeSeriesIdFromPArray( pArray, pLocation );
      dataType = getTimeSeriesCodeFromPArray( pArray, pLocation+2 );
      /*timeInterval = 6;*/
      
      timeInterval = mapeTimeInterval;
    
      if ( getFewsDebugFlag() > 3 )
      {	 
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	 "id: %s type: %s tstep: %d", Id, dataType, timeInterval );
      }
      
      count  = getNumberOfElementsInTimeSeries( timeInterval );
      inputTS = getOneTimeSeries( Id, dataType, timeInterval, &count ); 
      
   
      /* user set timestep other than 24 hour; do 24h accumulation */ 
      if(mapeTimeInterval > 0 && mapeTimeInterval < 24)
      {
          computeDailyMAPE( inputTS , mapeTimeInterval);  
      }   
      *pets = inputTS->value; 
   }
   
   /* SASC time series is option */
   pLocation = (int)pArray[15];
   if ( pLocation > 0 )
   {
      Id = getTimeSeriesIdFromPArray( pArray, pLocation );
      dataType = getTimeSeriesCodeFromPArray( pArray, pLocation+2 );
      timeInterval = (int)pArray[pLocation+2];

      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	 "id: %s type: %s tstep: %d", Id, dataType, timeInterval );
      }
      
      count = getNumberOfElementsInTimeSeries( timeInterval );
      inputTS = getOneTimeSeries( Id, dataType, timeInterval, &count );
      *scts = inputTS->value;
   }
   
   /* SWE time series is option */
   pLocation = (int)pArray[16];
   if ( pLocation > 0 )
   {
      Id = getTimeSeriesIdFromPArray( pArray, pLocation );
      dataType = getTimeSeriesCodeFromPArray( pArray, pLocation+2 ); 
      timeInterval = (int)pArray[pLocation+2];

      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	 "id: %s type: %s tstep: %d", Id, dataType, timeInterval );
      }
      
      count = getNumberOfElementsInTimeSeries( timeInterval );
      inputTS = getOneTimeSeries( Id, dataType, timeInterval, &count ); 
      *wets = inputTS->value;
   }
   
   /* MAT time series is required only if AEI, ATI or frozen ground options 
    * used 
    */
   pLocation = (int)pArray[17];
   if ( pLocation > 0 )
   {
      Id = getTimeSeriesIdFromPArray( pArray, pLocation );
      dataType = getTimeSeriesCodeFromPArray( pArray, pLocation+2 );
      timeInterval = (int)pArray[pLocation+2];

      if ( getFewsDebugFlag() > 3 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	 "id: %s type: %s tstep: %d", Id, dataType, timeInterval );
      }
      
      count = getNumberOfElementsInTimeSeries( timeInterval );
      inputTS = getOneTimeSeries( Id, dataType, timeInterval, &count ); 
      *tats = inputTS->value;
   }

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "END readInputTS() ...\n");
   }

} /* readInputTS() ---------------------------------------------------------- */

/******************************************************************************
     Filename:  readInputTsAPICont.c

     Description:
     ============
        This function read input time series from ts.txt file and pass them to
     FFG operation.	
     
     Input: 
     -----
     float  pArray[] - contain input ts information 

     Output:
     ------
     float   *pts    - RAIM (Precipitation) input ts data in unit/dim MM L 
     float  **tats   - MAT  (Air temperature)                         DEGC TEMP
     
     Change History
     ==============

     DATE          VERSION    PROGRAMMERS           NOTES
     ----------    -------    -----------------     ----------------------
     12/18/09      1          Cham Pham             Initial implementation
                          
******************************************************************************/
void readTsFFG( float pArray[], float *pts, float **tats )
{
   char  *Id = NULL, 
	 *dataType = NULL;
   
   int   timeInterval; 
   int   pLocation;
   int   count;
   int   i;

   TimeSeries *inputTS = NULL;

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "START readTsFFG() ...\n");
   }

   /* RAIM (Required) time series is the driver input TS for this model */
   Id = getTimeSeriesIdFromPArray( pArray, 8 );
   dataType = getTimeSeriesCodeFromPArray( pArray, 10 );

   timeInterval = (int)pArray[6];     

   if ( getFewsDebugFlag() > 0 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
      "id: %s type: %s tstep: %d", Id, dataType, timeInterval );
   }
   
   count = getNumberOfElementsInTimeSeries( timeInterval );

   inputTS = getOneTimeSeries( Id, dataType, timeInterval, &count );
   *pts = inputTS->value[0];

   /* MAT time series is required only if AEI, ATI or frozen ground options 
    * used 
    */
   pLocation = (int)pArray[17];

   if ( pLocation > 0 )
   {
      Id = getTimeSeriesIdFromPArray( pArray, pLocation );
      dataType = getTimeSeriesCodeFromPArray( pArray, pLocation+2 );
      timeInterval = (int)pArray[pLocation+2];

      if ( getFewsDebugFlag() > 0 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	 "id: %s type: %s tstep: %d", Id, dataType, timeInterval );
      }
      
      count = getNumberOfElementsInTimeSeries( timeInterval );
      inputTS = getOneTimeSeries( Id, dataType, timeInterval, &count ); 
      *tats = inputTS->value;
   }

   if ( getFewsDebugFlag() > 0 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "END readTsFFG() ...\n");
   }

} /* readTsFFG() ---------------------------------------------------------- */



/******************************************************************************/

void computeDailyMAPE( TimeSeries *inputTS , int inputMAPETimeStep )
{

   float mape = 0.0;  
   int i;
   int missingDataPoints = 0;
   int expectPointCount, count;
   
   if(inputTS->timeStep != 24 && inputMAPETimeStep != 0)
   {    
        
         expectPointCount = 24/inputMAPETimeStep; 
         count = inputTS->count;
         /* calculate MAPE one day only */
         if ( inputTS->count > expectPointCount) count = expectPointCount;
         missingDataPoints = expectPointCount - count;
         for(i = 0; i<missingDataPoints ;i++)
	 {
            /* set mape = to last ts value */
	    mape = mape + inputTS->value[count-1];	
	    printf("mape =%f\n",mape);    
	 }
	 
	 for(i = 0;i<count;i++)
         {
           mape = mape + inputTS->value[i];  	              
         }	 
      
	 inputTS->value[0] = mape;
	
         inputTS->count = 1;
         inputTS->timeStep = 24;

   }
   
   
}

