/******************************************************************************
     Filename:  getRequiredInputTS.c

     Description:
     ============
        This function read required input time series from ts.txt file 
     and pass them to FORTRAN module.	
     
     Input: 
     -----
     float  pArray   - contain input ts information 

     Output:
     ------
     int    *count   - number of time steps

     Change History
     ==============

     DATE          VERSION    PROGRAMMERS           NOTES
     ----------    -------    -----------------     ----------------------
     02/18/09      1          Cham Pham             Initial implementation
                          
******************************************************************************/
#include "sacsmaht.h"

void getRequiredInputTS( float pArray[], int *count )
{
   int   i, resolution, numElemTS;

   char  *Id[NUMIN], *dataType[NUMIN];   
   
   float *dataValue[NUMIN] = {NULL,};
   
   int   *dataDTime = NULL;
   int   ivers = 0;

   TimeSeries *inputTS = NULL;

   if ( getFewsDebugFlag() > 4 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "Begin getRequiredInputTS() ...\n");
   }

   /* Model time step - po(1) */
   resolution = (int)pArray[0];     
   ivers = (int)pArray[22];

   /* Get number of elements time series */
   numElemTS = getNumberOfElementsInTimeSeries( resolution );
   *count = numElemTS;

   /* RAIM time series is the driver for this model */
   Id[0] = getTimeSeriesIdFromPArray(pArray, 3);
   dataType[0] = getTimeSeriesCodeFromPArray(pArray, 5);
   
   inputTS = getOneTimeSeries( Id[0], dataType[0] , resolution, count );  
   dataValue[0] = inputTS->value;
   dataDTime = inputTS->dateTime;

   /* Read optional input time series */
   if ( ivers > 1 )
   {
      /* Get SASC time series */
      Id[1] = getTimeSeriesIdFromPArray( pArray, 9 );
      dataType[1] = getTimeSeriesCodeFromPArray( pArray, 11 );

      /* Get MAT time series */
      Id[2] = getTimeSeriesIdFromPArray( pArray, 13 );
      dataType[2] = getTimeSeriesCodeFromPArray( pArray, 15 ); 

      /* Get SWE time series */
      Id[3] = getTimeSeriesIdFromPArray( pArray, 16 );
      dataType[3] = getTimeSeriesCodeFromPArray( pArray, 18 ); 

      /* Get SNSG time series */
      Id[4] = getTimeSeriesIdFromPArray( pArray, 19 );
      dataType[4] = getTimeSeriesCodeFromPArray( pArray, 21 ); 

      for ( i = 1; i < NUMIN-1; i++ )
      {
          inputTS = getOneTimeSeries( Id[i], dataType[i], resolution, count );  
       
          if ( getFewsDebugFlag() > 4 )
          {
             logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
             "id(%d): %s ; dataType: %s count: %d\n", 
	     i, Id[i], dataType[i], *count );
          }

          dataValue[i] = inputTS->value;
      }
   }
   
   /* Get MAPE time series (optional) */
   Id[5] = getTimeSeriesIdFromPArray( pArray, 25);
   dataType[5] = getTimeSeriesCodeFromPArray( pArray, 27 );

   if ( !strcmp(dataType[5], "MAPE") )
   {
      int mapeRes = 24;

      /* Get number of elements time series */
      int mapeElemTS = getNumberOfElementsInTimeSeries( mapeRes );

      if ( getFewsDebugFlag() > 4 )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,  
         "Used %s input time series AND ElemTS = %d\n", 
	 dataType[5], mapeElemTS );
      }

      inputTS = getOneTimeSeries( Id[5], dataType[5], mapeRes, &mapeElemTS );
      dataValue[5] = inputTS->value;
   }

   /* Pass all required and optional input time series data to FORTRAN module */
   etinit1_( pArray, dataValue[0], dataValue[1], dataValue[2], dataValue[3], 
	     dataValue[4], dataValue[5], dataDTime, count ); 

   if ( getFewsDebugFlag() > 4 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "End getRequiredInputTS() ...\n");
   }
   
} /* getRequiredInputTS() -------------------------------------------------- */
