/*****************************************************************************************
* FILENAME:             dump_test_grib.c
* GENERAL INFORMATION:  read from generated FFGBIN* GRIB file, retrieve the data and write
                        into grib_test_file.txt, then compare with the data (ffgGridNetcdf.txt)
			retrieved from input netCDF file.
* PROGRAMMER:           Jingtao Deng
* CREATION DATE:        May, 2010
* ORGANIZATIONL         OHD11/HSEB
* MACHINE:              Linux
* MODIFICATION HISTORY:
******************************************************************************************/

#include "ffgGribProduct.h"


void dump_test_grib(char *grib1FN)
{
  char input_grib_file[MAX_FILE_LEN]="";
  char grib_test_file[MAX_FILE_LEN] = "grib_test_file.txt";
  FILE *input_grib_file_ptr = NULL;
  FILE *grib_test_file_ptr = NULL;
  GRIBRecord grib_rec;
  size_t nrec=0;
  int i, j, m, status;
  
  grib_test_file_ptr = fopen(grib_test_file, "w");
  if (grib_test_file_ptr == NULL)
  {
      logMessageWithArgsAndExitOnError(FATAL_LEVEL,
	                               "Can not open the grib test file=%s", grib_test_file);     				   
  } 
  
      
  for (i=0; i < ffgData.time_size; i++)
  {
      nrec = 0;
      strcpy(input_grib_file, "");
      strcpy(input_grib_file, grib1FN );                
      strcat(input_grib_file, dur_suffix[i]);

      input_grib_file_ptr = fopen(input_grib_file, "rb");
      if (input_grib_file_ptr == NULL)
      {
          logMessageWithArgsAndExitOnError(FATAL_LEVEL,
	                                   "Can not open the grib file=%s", input_grib_file);         
      } 
      
      /* initialize grib_rec structure */
      
      grib_rec.buffer = NULL;
      grib_rec.pds_ext = NULL; 
      grib_rec.gridpoints = NULL;
      
      /* call unpackgrib() */
      while (( status = unpackgrib(input_grib_file_ptr, &grib_rec)) == 0)
      {
         nrec++;
      }
      
      if (nrec == 1)
      {
         fprintf(grib_test_file_ptr, "Number of Points in the X-direction=%d\n", grib_rec.nx);
	 fprintf(grib_test_file_ptr, "Number of Points in the Y-direction=%d\n", grib_rec.ny);
         fprintf(grib_test_file_ptr, "\nTime step duration=%s hrs\n\n", dur_suffix[i]);
	
         for (m=grib_rec.ny-1; m>= 0; m--)
	 {
	    fprintf(grib_test_file_ptr, "row=%d\n", m);
	    fprintf(grib_test_file_ptr, "FFG value=");	  
	    fflush(grib_test_file_ptr);
	    
	    for (j=0; j< grib_rec.nx; j++)
	    {	       	           
	       if( grib_rec.gridpoints[m][j] > 0. )
	         fprintf(grib_test_file_ptr, "%16.1f ", (float) grib_rec.gridpoints[m][j]);
	    }
	    fprintf(grib_test_file_ptr, "\n");
	 }      
      
      }  
      
      if (status == -1)
      {
         if ( getFewsDebugFlag() > DEBUG_LEVEL_FLAG )
         { 
            logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
                                             "EOF - end of file found %s\n", input_grib_file);
         }
      }         
      else
      {
         if ( getFewsDebugFlag() > DEBUG_LEVEL_FLAG )
         { 
            logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
                                             "Read error after %d records\n", nrec);
         }
      
      }        
     
      fclose(input_grib_file_ptr);     
  
  }
  
  fclose(grib_test_file_ptr);
  
  return;
}
