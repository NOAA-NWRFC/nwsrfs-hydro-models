/*******************************************************************************
* FILENAME:             ffgGribProduct.c
* GENERAL INFORMATION:  convert gridded FFG in FEWS from NetCDF format
*                       to GRIB1 format.
*                       Emulate legacy program "Prodgen" in NWSRFS to generate 
*                       GRIB1 format FFG product, but instead of XMRG format.
* PROGRAMMER:           Cham P.
* PROGRAMMER:           Jingtao Deng
* CREATION DATE:        May, 2010
* ORGANIZATIONL         OHD11/HSEB
* MACHINE:              Linux
* MODIFICATION HISTORY:
*******************************************************************************/

#include "ffgGribProduct.h"

int main( int argc, char **argv )
{
  /* Variables that hold GRIB Parameters for packgrib function */
  int    grib_lbl[GRIB_META_DATA_LEN]; /* array that holds the values for the 
					  GRIB meta data */
  char   pds_ext[256] = {'\0'};        /* optional user-filled supplemental 
					  information to be appended to the GRIB
				          to the GRIB PDS section. Here is not 
					  to be used, initialized at empty 
					  string */
  size_t pds_ext_length = 0;           /* length of pds_ext string */
  float  xmissing = MISSING_VAL;       /* is the value used to indicate missing
					  data in the grid */
  size_t odim = COPYSIZE;              /* size of output_buffer file, should be
					  at least as large as the
                                          expected length of the GRIB grid */
  size_t *output_buffer= NULL;         /* returned value from packgrib(), 
					  buffer containing the GRIB
                                          representation of the grid */
  size_t length;                       /* returned value from packgrib(), it 
					  the  total length in octets (8-bit
                                          bytes) of the GRIB grid */
  size_t idim;                         /* x dimension */
  float  *cdfvargrid=NULL;	       /* main array holding the actual data 
					  values for each duration */
					  
  /* other variables */
  clock_t start, end;
  double cpu_time_used;
  int arraysize;
  int status, i, j;	
  FILE *grib_file_ptr = NULL;				  	
  char *ffgnetcdfFN = NULL;
  char *grib1FN = NULL;
  char *ffgConfigFN = NULL;
  char output_file[MAX_FILE_LEN] = "";    
  
  start = clock();
  
  /* initialize grib_lbl[]*/
  memset( grib_lbl, 0, sizeof(grib_lbl) );

  /* check for appropriate input arguments */
  if ( readOptions(argc, argv) < 0 )
  {
     exit(0);
  }
  
  /* Malloc input arguments.txt file */
  ffgConfigFN = (char *)malloc( sizeof(char) * MAX_FILE_LEN );
    
  if ( ffgConfigFN == NULL )
  {
     logMessageWithArgsAndExitOnError( FATAL_LEVEL,
                                       "Problem with memory allocation for input configuration file %s", 
                                        ffgConfigFN );
  }
  
  /* Retrieve configuration data from arguments.txt file */ 
  strcpy( ffgConfigFN, argv[1] );
  
  /* initialize ffg_config structure */
   ffg_config = (ffgConfigStr *)malloc(sizeof(ffgConfigStr));
     
  retrieveFFGConfig( ffgConfigFN );
                
  if (ffg_config ==  NULL)
  {
     logMessageWithArgsAndExitOnError( FATAL_LEVEL,
                                       "No data retrieved from input configuration file %s",
				       ffgConfigFN);
     				       
  }
  /* create Diagnostic (diag.txt) file name */
      
  setDiagFileName( ffg_config->diagFileName );
  getDiagFileName( );
 
  /* Set fews debug flag */ 
  setFewsDebugFlag( ffg_config->debugFlag );
   
  if ( getFewsDebugFlag() > DEBUG_LEVEL_FLAG )
  {
     logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
                                      "The ffg configuration file = %s\n", ffgConfigFN );
     logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
                                       "The FewsDebugFlag = %d\n", getFewsDebugFlag() );
 
     logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
                                       "The output log file = %s\n", getDiagFileName() );
  }  
  
  /* malloc input NetCDF file */
  ffgnetcdfFN = (char *)malloc( sizeof(char) * MAX_FILE_LEN );
  
  if( ffgnetcdfFN == NULL )
  {
     logMessageWithArgsAndExitOnError( FATAL_LEVEL,
                                      "Problem with memory allocation for input netCDF file %s", ffgnetcdfFN );
  }	
  
  /* Get input (ffg netcdf) file name from ffg_config structure*/
  strcpy( ffgnetcdfFN, ffg_config->inputNetcdfFile );
  
  if ( getFewsDebugFlag() > DEBUG_LEVEL_FLAG )
  {
     logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
                                      "The input FFG NetCDF file = %s\n", ffgnetcdfFN );
  }

   
  /* Decode FFG data in FEWS netcdf format, define global structure ffgData */
   decodeFFG( ffgnetcdfFN );
   
  
  /* Get the output grib file name prefix from ffg_config structure */
  grib1FN = (char *)malloc( sizeof(char) * MAX_FILE_LEN );

  if ( grib1FN == NULL )
  {
     logMessageWithArgsAndExitOnError( FATAL_LEVEL,
                                       "Problem with memory allocation for output grib file %s", grib1FN );
  }

  /* Get output file name */
  strcpy( grib1FN, ffg_config->outputGrib1FilePref );  
      
  /* For each duration, generate the grib file */
  if ( ffgData.time_size < 1 )
  {
     logMessageWithArgsAndExitOnError( FATAL_LEVEL,
                                       "The number of FFG duration is less than 1" );
  }
  
  idim = (size_t)ffgData.x_size;
  arraysize = ffgData.x_size * ffgData.y_size;      
 
  
  /* Output grib file for appropirate duration */
  for ( i = 0; i < ffgData.time_size; i++ )
  {
      sprintf( output_file, "%s%s", grib1FN, dur_suffix[i] );

      if ( getFewsDebugFlag() > DEBUG_LEVEL_FLAG )
      { 
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
                                          "The generated GRIB1 file = %s", output_file );
      }        
      
      /* Malloc output_buffer which is output from packgrib()*/
      output_buffer = (size_t *)malloc( sizeof(size_t) * odim );
  
      if ( output_buffer == NULL )
      {         
	 logMessageWithArgsAndExitOnError(FATAL_LEVEL,
                                         "Problem with memory allocation for output_buffer");
      }
    
      /* Define GRIB meta data to array grib_lbl[] */
      define_grib_metadata(grib_lbl, i+1);

      /* Call packgrib() function for each duration */
                   
      cdfvargrid = (float *)malloc( sizeof(float) * arraysize );

      logMessageWithArgsAndExitOnError( DEBUG_LEVEL, "the whole number=%d", 
	                                i * arraysize+arraysize-1 );
      for ( j = 0; j < arraysize; j++ )
      {
      
         cdfvargrid[j] = ffgData.ffgArray[i*arraysize+j];
	 
	 /* change inch to mm in grib product */
	 if (cdfvargrid[j] > xmissing)
	   cdfvargrid[j] = 25.4*cdfvargrid[j];
	          
      }
              
      status = packgrib( grib_lbl, pds_ext, &pds_ext_length, cdfvargrid, 
	                 &idim, &xmissing, output_buffer, &odim, &length );

      if ( status != 0 )
      {	 
         logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
	                                   "The packgrib() function returns error with status=%d", status );
      }
      else
      {
         if ( getFewsDebugFlag() > DEBUG_LEVEL_FLAG )
         { 
            logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
                                             "Success from packgrib() function for duration=%s hour",dur_suffix[i]);	  
         }					     
      }  

      /* Generate output filename and write Grib into it */
      output_grib( output_file, output_buffer, length );	
       
      /* Free space */
       
      if ( output_buffer != NULL )
      {
         free( output_buffer );
	 output_buffer = NULL;	    
      }
      	  
      if ( cdfvargrid != NULL )
      {
         free( cdfvargrid );
	 cdfvargrid = NULL;
      }  
  }
  
  /* generate the grib_test_file.txt retrieving from the generated GRIB1 files in order to
     compare with the data retrieving from NetCDF file. This test only apply if the FewsDebugFlag is set as 5 */
  if (getFewsDebugFlag() == 5) 
     dump_test_grib(grib1FN);   
  
  /* Free space */
  if ( ffgnetcdfFN != NULL )
  {
     free( ffgnetcdfFN );
     ffgnetcdfFN = NULL;
  }

  if ( ffgConfigFN != NULL )
  {
     free( ffgConfigFN );
     ffgConfigFN = NULL;
  }

  if ( grib1FN != NULL )
  {
     free( grib1FN );
     grib1FN = NULL;
  }
  
  if (ffg_config != NULL )
  {
     free( ffg_config);
     ffg_config = NULL;
  }  
  
   if (ffgData.timeArray != NULL)
   {
      free(ffgData.timeArray);
      ffgData.timeArray = NULL;
   }
   if (ffgData.yArray != NULL)
   {
      free(ffgData.yArray);
      ffgData.yArray = NULL;
   }
   if (ffgData.xArray != NULL)
   {
      free(ffgData.xArray);
      ffgData.xArray = NULL;
   }
   
   if (ffgData.ffgArray != NULL)
   {
      free(ffgData.ffgArray);
      ffgData.ffgArray = NULL;
   } 

  /* Compute CPU time for the whole process */
  end = clock();
 
  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;

  logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
                                   "===> TOTAL RUN TIME CONVERTING NetCDF to GRIB1: %.2f (seconds)", 
  cpu_time_used );  
  
//  closeDiagFile();
 
  return ( SUCCESS );
}
