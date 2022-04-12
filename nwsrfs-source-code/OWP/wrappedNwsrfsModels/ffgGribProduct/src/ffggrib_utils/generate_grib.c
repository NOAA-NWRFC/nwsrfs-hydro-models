/*****************************************************************************************
* FILENAME:             generate_grib.c
* GENERAL INFORMATION:  generate grib file (e.g FFGBIN#) for each time step.                      
* PROGRAMMER:           Jingtao Deng
* CREATION DATE:        May, 2010
* ORGANIZATIONL         OHD11/HSEB
* MACHINE:              Linux
* MODIFICATION HISTORY:
******************************************************************************************/
/*********************************************************************
void output_grib()
************************************************************************/
#include "ffgGribProduct.h"

void output_grib(char *output_file, 
                size_t *output_buffer,
		size_t length)
{   
   int status;
   FILE  *grib_file_ptr = NULL;
   
   if ((grib_file_ptr = fopen(output_file, "w")) == (FILE *) NULL)
   {
      logMessageWithArgsAndExitOnError(FATAL_LEVEL,
                                      "Unable to open grib output file %s", output_file);
      return;
     
   }
   else
   {      
      status = write_grib_tofile(grib_file_ptr, output_buffer, length);
      fclose(grib_file_ptr);
   }

   return;
}

 
/***********************************************************************
   int write_grib_tofile()
   write out the GRIB data to the output buffer
************************************************************************/
int write_grib_tofile(FILE *grib_file_ptr,
                      size_t *output_buffer,
		      size_t  length)
{
   
   int status,i, j;
   
   /* test purpose */
   char crcrlf[3]={'\r','\r','\n'};
   unsigned char header[WMO_HEADER_LEN]={'\0'};
   char wmohdr1[7] = {'\0'};
   char wmohdr2[5]={'\0'};
   unsigned char aspace={' '};
   struct tm *curgmtime;
   time_t curtime;
   char adayhrmin[7]={'\0'};
   
   
   /* define wmohdr1 for site id*/
   strcpy(wmohdr2, ffg_config->siteId);
   
   /* define wmo id */
   strcpy(wmohdr1, ffg_config->wmoId);
   
   /* define current system GMT  dayhourmin */
   time( &curtime);
	
   curgmtime = gmtime (&curtime);
	
   sprintf(adayhrmin,"%02d%02d%02d",curgmtime->tm_mday,curgmtime->tm_hour,curgmtime->tm_min);
   
   /* define header e.g. "ZEGZ98 KTUA 281200"*/
   j=0;

   for (i=0;i<strlen(wmohdr1);i++)
   {
      header[j]=(unsigned char) wmohdr1[i];
      j++;

   }
   header[j]=aspace;
   j++;

   for (i=0;i<strlen(wmohdr2);i++)
   {
      header[j]=(unsigned char) wmohdr2[i];
      j++;
   }
   header[j]=aspace;
   j++;

   for (i=0;i<strlen(adayhrmin);i++)
   {
      header[j]= (unsigned char) adayhrmin[i];
      j++;
   }

         
   status = fwrite(header,sizeof(unsigned char),18, grib_file_ptr);
   if (status == 0)
      logMessageWithArgsAndExitOnError(WARNING_LEVEL,
                                      "Possible problem writing header to grib file.\n");  		
		
   status = fwrite(crcrlf,sizeof(unsigned char),3, grib_file_ptr);
   if (status == 0)
     logMessageWithArgsAndExitOnError(WARNING_LEVEL,
                                      "Possible problem writing header to grib file.\n");       

   status = fwrite((unsigned char *)output_buffer, sizeof(unsigned char), length, grib_file_ptr);
   
   if (status == 0 || length < 100)
   {
      if ( getFewsDebugFlag() > DEBUG_LEVEL_FLAG )
         logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
                                         "Possible problem in writing GRIB file");
   }  
   else
   {
      if ( getFewsDebugFlag() > DEBUG_LEVEL_FLAG )
         logMessageWithArgsAndExitOnError(DEBUG_LEVEL,
                                          "Write to grib file");
   }   
   return 0;
}   
