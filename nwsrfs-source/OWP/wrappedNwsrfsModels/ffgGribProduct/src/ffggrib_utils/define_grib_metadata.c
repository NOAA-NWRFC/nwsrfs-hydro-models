/******************************************************************
*   void define_grib_metadata()
* GENERAL INFORMATION:  define grib_lbl[] array which will be passed in
*                       packgrib()
* PROGRAMMER:           Jingtao Deng
* CREATION DATE:        May, 2010
* ORGANIZATIONL         OHD11/HSEB
* MACHINE:              Linux
* MODIFICATION HISTORY:   
  *******************************************************************/
#include "ffgGribProduct.h"

void define_grib_metadata(int *grib_lbl,                          
			  int duration_cnt )			  
			  
{
   const int parameterCodeId[]={221,222,223,224,225};
      
   /* define grib meta data into grib_lbl[] */
   
   /* GRIB Edition number */
   grib_lbl[0] = 1;
   
   /* grid Identification */
   grib_lbl[1] = 255;  /* non standard grid */
   
   /* parameter table version number */
   grib_lbl[2] = 128;
   
   /* Origiating center ID */
   grib_lbl[3] = 9;
   
   /* Generating Process ID number */
   grib_lbl[4] = ffg_config->processId;  /* for NWSFFGS in Table A */
   
   /* Reserved - currently ignored */
   grib_lbl[5] = 0;
   
   /* GDS included or not (0 - no, 1 - yes)*/
   grib_lbl[6] = 1;
   
   /* Parameter code, use lookup table 128 to find the parameter code */
   if (duration_cnt == 1)
      grib_lbl[7] = parameterCodeId[0];
   else if (duration_cnt == 2)
      grib_lbl[7] = parameterCodeId[1];
   else if (duration_cnt == 3)
      grib_lbl[7] = parameterCodeId[2];
   else if (duration_cnt == 4)
      grib_lbl[7] = parameterCodeId[3];
   else if (duration_cnt == 5)
      grib_lbl[7] = parameterCodeId[4];         
   
   /* indicator of level type */
   grib_lbl[8] = 1;  /* surface */
   
   /* value of first level */
   grib_lbl[9] = 0;
   
   /* value for second level */
   grib_lbl[10] = 0;
   
   /* model computation time year, month, day, hour, minute*/
   grib_lbl[11] = ffg_config->computationYear;
   grib_lbl[12] = ffg_config->computationMon;
   grib_lbl[13] = ffg_config->computationDay;
   grib_lbl[14] = ffg_config->computationHrMin;
   
   /* Forecast Time Unit, table 4 */
   grib_lbl[15] = ffg_config->forecastTimeUnit;
   
   /* P1 period of time */
   grib_lbl[16] = 0;
   
   /* P2 period of time */
   grib_lbl[17] = 24;
   
   /* time range indicator (table 5) */
   grib_lbl[18] = ffg_config->timeRangeIndicator;
   
   /* number included in Average */
   grib_lbl[19] = 0;
   
   /* number of missing Grids in Average */
   grib_lbl[20] = 0;
   
   /* sub_center ID from table C */
   grib_lbl[21] = ffg_config->subCenterId;
   
   /* decimal scale factor */
   grib_lbl[22] = ffg_config->decimalScaleFactor;
   
   /* Binary Data section flag */
   grib_lbl[23] = 0;
   
   /* width in bits of a packed data point */
   grib_lbl[24] = 16;
   
   /* polar stereographic or lambert */
   grib_lbl[25] = ffg_config->projectionType;
   
   /* PDS reserved, ignored */
   grib_lbl[26] = 0;
   
   /* PDS reserved, ignored */
   grib_lbl[27] = 0;
   
   /* GDS Data representation type */
   grib_lbl[28] = 5;
   
   /* number of points in the X-direction */
   grib_lbl[29] = (int)ffgData.x_size ;
   
   /* number of points in the Y-direction */
   grib_lbl[30] = (int) ffgData.y_size;
   
   /* Latitude of the first gridpoint (*1000) */
   grib_lbl[31] = ffg_config->latOfFirstGridPoint;
   
   /* longitude of the first gridpoint (*1000) */
   grib_lbl[32] = ffg_config->lonOfFirstGridPoint;
   
   /* resolution and component flags */
   grib_lbl[33] = 8;
   
   /* longitude of grid orientation (*1000) */
   grib_lbl[34] = ffg_config->lonOrigin;
   
   /* X-direction grid length in meters */
   grib_lbl[35] = ffg_config->xCellSize;
   
   /* Y-direction grid length in meters */
   grib_lbl[36] = ffg_config->yCellSize;
   
   /* Projection center flag */
   grib_lbl[37] = 0;
   
   /* scanning mode flags */
   grib_lbl[38] = 64;
   
   /* For Polar stereographic */
   if (ffg_config->projectionType == 32)
   {
      grib_lbl[39] = 0;
      grib_lbl[40] = 0;
      grib_lbl[41] = 0;
      grib_lbl[42] = 0;
   }
            

   return;
}	   			
