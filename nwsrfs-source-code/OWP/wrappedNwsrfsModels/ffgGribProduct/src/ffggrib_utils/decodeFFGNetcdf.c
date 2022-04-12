/******************************************************************************
 *      Filename: decodeFFGNetcdf.c
 *
 *      Description:
 *      ============
 *         This file contains decodeFFG () function that will decode gridded 
 *         FFG values then store information needed to ffgData struct.
 *
 *      Change History
 *      ==============
 *      DATE          VERSION    PROGRAMMERS           NOTES
 *      ----------    -------    -----------------     ----------------------
 *      05/18/10      1          Cham P.               Initial implementation
 *                                                                              
 ******************************************************************************/

#include "ffgGribProduct.h"
#include "decodeNetCDF.h" 

/* Opens and reads netCDF files ----------------------------------------------*/
void decodeFFG ( char *filename )
{
   int i;
   int sizeArray;

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "decodeFFG() - Start to decode netcdf FFG data ...\n");
   }

   /* Open netcdf file */
   openAndInquiryNETCDF( filename );

   /* Get dimension info */
   getDimension( );
   
   /* Retrieve TIME data --------------------------------------------------*/
   getNetcdfData( "time", &ffgData.time_size, (size_t**)&ffgData.timeArray );
         

   if ( getFewsDebugFlag() > 5 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "sizeArray = %d\n", ffgData.time_size );
      for ( i = 0; i < ffgData.time_size; i++ )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	 "%.0lf; // %s(%d)\n", ffgData.timeArray[i], "time", i );
      }
   }

   /* Retrieve Y data -----------------------------------------------------*/
   getNetcdfData( "y", &ffgData.y_size, (size_t**)&ffgData.yArray );
      
   if ( getFewsDebugFlag() > 5 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "sizeArray = %d\n", ffgData.y_size );
      for ( i = 0; i < ffgData.y_size; i++ )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	 "%.0f; // %s(%d)\n", ffgData.yArray[i], "y", i );
      }
   }

   /* Retrieve y variable with '_FillValue' and 'units' attributes */
   float *fillValue;
   int attSize;

   getAttributeValue( "y", "_FillValue", &attSize, (size_t**)&fillValue );
//   printf("FillValue = %f - attSize: %d\n",*fillValue, attSize );

   char *units;
   getAttributeValue( "y", "units", &attSize, (size_t**)&units );
   //printf("y- units = %s - attSize: %d\n", units, attSize );
   
   /* Retrieve X data ----------------------------------------------------*/
   getNetcdfData( "x", &ffgData.x_size, (size_t**)&ffgData.xArray );

   if ( getFewsDebugFlag() > 5 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "sizeArray = %d\n", ffgData.x_size );
      for ( i = 0; i < ffgData.x_size; i++ )
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	 "%.0f; // %s(%d)\n", ffgData.xArray[i], "x", i );
      }
   }


   /* Retrieve FFG data ----------------------------------------------------*/
   int ffg_size;
   getNetcdfData( "FFG", &ffg_size, (size_t**)&ffgData.ffgArray );

   //Only need for level1 testing
   if ( getFewsDebugFlag() >= 5 )
   {

      FILE *ofile = NULL;
      int datasize = ffgData.x_size * ffgData.y_size;
      int idur, x, y;
      ofile = fopen("ffgGridNetcdf.txt", "w+");
      if ( ofile == NULL )
	 	fprintf(stderr, "open file failed\n");
      
      int i;
      for ( i = 0; i < ffg_size; i++ )
      {
          if ( ffgData.ffgArray[i] > 0. )
          {
             logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	     "%.3f; // %s(%d)\n", ffgData.ffgArray[i], 
	     "FFG", i );
	   }
      }

      int idx = 0;
      for ( idur = 0; idur < ffgData.time_size; idur++ )
      {
         fprintf( ofile,"Number of Points in the X-direction=%d\n", 
	          ffgData.x_size);
         fprintf( ofile, "Number of Points in the Y-direction=%d\n", 
	          ffgData.y_size);
         fprintf( ofile, "\nTime step duration=%s hrs\n\n", 
	          dur_suffix[idur]);
         for ( y = ffgData.y_size-1; y >= 0; y-- )
	 {
	    fprintf(ofile, "row=%d\n", y);
	    fprintf(ofile, "FFG value=");
            for ( x = 0; x < ffgData.x_size; x++ )
            {
	       idx = x + (ffgData.x_size*y) + 
		     (ffgData.y_size*ffgData.x_size*idur);
	       if ( ffgData.ffgArray[idx] > 0. )
	       {
	          fprintf(ofile, "%16.1f ", ffgData.ffgArray[idx]);
	       }
            }

	    fprintf(ofile, "\n");
	 }
       }

       fclose(ofile);
   }

   /* Retrieve FFG variable with 'long_name' attribute */
   char *unit;
   char *nullstr = NULL; //use for retrieve GLOBAL ATTRIBUTE values
   
/*   int *lat = NULL, *lon = NULL, *lonOrigin = NULL, *xCell = NULL, *yCell = NULL;   */
   
   getAttributeValue( nullstr, "title", &attSize,
                   (size_t**)&unit );
//   printf("FFG- title = %s - attSize: %d\n", unit, attSize );
   
   //retrieve global attribute latOfFirstGridPoint
/*   getAttributeValue( nullstr, "latOfFirstGridPoint", &attSize, (size_t**)&lat );
   ffgData.latOfFirstGridPoint = (int)*lat;
   
   if (lat != NULL)
   {
       free(lat);
       lat = NULL;
   }
*/   
   
//   printf("latOfFirstGridPoint = %d - attSize: %d\n", ffgData.latOfFirstGridPoint, attSize );
   
   //retrieve global attribute lonOfFirstGridPoint
/*   getAttributeValue( nullstr, "lonOfFirstGridPoint", &attSize, (size_t**)&lon );
   ffgData.lonOfFirstGridPoint = (int)*lon;
   
   if (lon != NULL)
   {
       free(lon);
       lon = NULL;
   }*/
//   printf("lonOfFirstGridPoint = %d - attSize: %d\n",ffgData.lonOfFirstGridPoint, attSize );

   //retrieve global attribute lonOrigin
/*   getAttributeValue( nullstr, "lonOrigin", &attSize, (size_t**)&lonOrigin );
   ffgData.lonOrigin = (int)*lonOrigin;
   
   if (lonOrigin != NULL)
   {
       free(lonOrigin);
       lonOrigin = NULL;
   }*/
//   printf("lonOrigin = %d - attSize: %d\n",ffgData.lonOrigin, attSize );
   
   //retrieve global attribute xCellSize
/*   getAttributeValue( nullstr, "xCellSize", &attSize, (size_t**)&xCell );
   ffgData.xCellSize = (int)*xCell;
   
   if (xCell != NULL)
   {
       free(xCell);
       xCell = NULL;
   } */
   
//   printf("xCellSize = %d - attSize: %d\n", ffgData.xCellSize, attSize );
   
   //retrieve global attribute yCellSize
/*   getAttributeValue( nullstr, "yCellSize", &attSize, (size_t**)&yCell );
   ffgData.yCellSize = (int)*yCell;
   
   if (yCell != NULL)
   {
       free(yCell);
       yCell = NULL;
   }*/
//   printf("yCellSize = %d - attSize: %d\n", ffgData.yCellSize, attSize );
   
   char *ffgUnit = NULL;
   getAttributeValue( "FFG", "units", &attSize, (size_t**)&ffgUnit );
   
   if (ffgUnit != NULL)
   {
       free(ffgUnit);
       ffgUnit = NULL;
   }
   
   //printf("FFG- units = %s - attSize: %d\n", ffgUnit, attSize );

   /* Close the netcdf file. */
   closeNetCDF( );


   /* free space */
   
   if (fillValue != NULL)
   {
      free(fillValue);
      fillValue = NULL;
   }
     
   
   if (unit != NULL)
   {
      free(unit);
      unit = NULL;
   }  
   
   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "decodeFFG() - End to decode netcdf FFG data ...\n" );
   }
}
