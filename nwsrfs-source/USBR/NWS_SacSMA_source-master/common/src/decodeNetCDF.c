/******************************************************************************
 *      Filename: decodeNetCDF.c
 *
 *      Description:
 *      ============
 *         This file contains functions such as  
 *         getDimension()      - Get dimensions
 *         getAttributeValue() - Retrieve attribute value 
 *         getNetcdfData()     - Retrieve data (max array size is 4)
 *         openAndInquiryNETCDF() - Open netCDF file and get number of
 *                                  dimensions, number of variables and
 *                                  number of attributes
 *         closeNetCDF()       - close netCDF 
 *     
 *      Note: see example how to use those libraries
 *            (wrappedNwsrfsModels/ffgGribProduct/src/ffggrib_utils dir)
 *
 *      Change History
 *      ==============
 *      DATE          VERSION    PROGRAMMERS           NOTES
 *      ----------    -------    -----------------     ----------------------
 *      05/18/10      1          Cham P.               Initial implementation
 *                                                                              
 ******************************************************************************/
#include "decodeNetCDF.h"

/* Get dimension -------------------------------------------------------------*/
void getDimension( )
{
    int id;

    if ( _ndims <= 0 )
    {
       logMessageWithArgsAndExitOnError( FATAL_LEVEL,
       "dimensions: %d %d\n", _ndims );
    }

    for( id = 0; id < _ndims; id++ ) 
    {
       nc_inq_dim( _ncid, id, dims[id].name, &dims[id].size );
       //printf( "%s(%d)\n", dims[id].name, dims[id].size );
    }
}


/* Retrieve variable attributes */
void getAttributeValue( char *varName, char *attName, int *attLen, 
                        size_t **vals )
{
   nc_type attType;
   int     iv, ia, idvar;
   char    variableName[NC_MAX_NAME];

   int     useGlobalAtt = 0;  //false

   if ( getFewsDebugFlag() > 4 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "Start getAttributeValue()" );
   }

   for ( iv = 0; iv < _nvars; iv++ )
   {
      if ( nc_inq_varname(_ncid, iv, variableName) )
      {
            logMessageWithArgsAndExitOnError( FATAL_LEVEL,
            "Could not inquire variables name (%s)\n", variableName );
      }

      idvar = iv;

      if ( varName == NULL )
      {
	 //printf("VAR NAME EQUAL NULL\n");
	 idvar = NC_GLOBAL;
	 useGlobalAtt = 1;
	 varName = " "; 
      }

      //printf ( "varName: %s variableName = %s nc_global = %d\n",varName, 
//	      variableName, NC_GLOBAL);   
//
      if ( !strcmp(variableName, varName) || useGlobalAtt == 1 )
      {
         //printf ( "VARNAME: %s idvar = %d nc_global = %d\n",varName, 
	   //       idvar, NC_GLOBAL);   

	 ncattinq(_ncid, idvar, attName, &attType, attLen); 

         //printf("varid = %d %s: %s\n",idvar, variableName, attName);
         //printf("type = %d attlen = %d typelen = %d\n",
	   //    attType, *attLen, nctypelen(attType));

	 if ( *attLen == 0 )
	 {
	    *vals = '\0';
            *attLen = 1;
	 }
	 else
	 {
	    *vals = malloc((unsigned)*attLen * nctypelen(attType));
            if ( !*vals )
	    {
	       nc_close( _ncid );
	       free ( *vals );
	       logMessageWithArgsAndExitOnError( FATAL_LEVEL,
	       "Malloc of space for  attribute values failed ..." );
	    }

            switch ( attType ) 
	    {
	      case NC_CHAR:
	         nc_get_att_text( _ncid, idvar, attName, (char*)*vals );
	         break;
	      case NC_SHORT:
	         nc_get_att_short( _ncid, idvar, attName, (short*)*vals );
	         break;
	      case NC_INT:
	         nc_get_att_long( _ncid, idvar, attName, (long*)*vals );
	         break;
	      case NC_FLOAT:
	         nc_get_att_float( _ncid, idvar, attName, (float*)*vals );
	         break;
	      case NC_DOUBLE:
	         nc_get_att_double( _ncid, idvar, attName, 
		                              (double*)*vals );
	         break;
	      default:
	          logMessageWithArgsAndExitOnError( FATAL_LEVEL,
	          "ERROR - variable attributes data: bad type...\n" );
	    }
	 }
	 break;
      }//if ( !strcmp(variableName, varName) )
   }// for iv looping
   
   if ( getFewsDebugFlag() > 4 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "End getAttributeValue() - Attribute:  %s - Len: %d\n", 
      attName, *attLen );
   }

}
/* Retrieve data -------------------------------------------------------------*/
void getNetcdfData( char strname[], int *sizeArr, size_t **anyValues )
{
   int id, iv;
   
   *sizeArr = -1;

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "Start getNetcdfData() - Variable:  %d - %s\n", _nvars, strname );
   }

   for ( iv = 0; iv < _nvars; iv++ ) 
   {
      if ( nc_inq_var( _ncid, iv, var.name, &var.type, &var.ndims, var.dims,
		     &var.natts ) )
      {
	 logMessageWithArgsAndExitOnError( FATAL_LEVEL,
	 "Could not inquire variables ...\n" );
      }
   
      if ( !strcmp(var.name, strname) )
      {
         size_t start[] = {0, 0, 0, 0};  //max 4D
         size_t count[] = {0, 0, 0, 0};  //max 4D

	 //printf("%s ndims: %d\n",var.name, var.ndims);

	 *sizeArr = 1;

         for( id = 0; id < var.ndims; id++ )
         {
	   //printf("%s %d\n",dims[var.dims[id]].name, dims[var.dims[id]].size);
	    
            *sizeArr *=dims[var.dims[id]].size;
            start[id] = 0;
            count[id] = dims[var.dims[id]].size;	    
         }

         //printf("VAR: var.type(%d); nctypelen(%d)\n",var.type,
	 //      nctypelen(var.type) );

	 *anyValues = malloc( *sizeArr * nctypelen(var.type) );

         if ( *anyValues == NULL )
	 {
	    logMessageWithArgsAndExitOnError( FATAL_LEVEL,
	    "Malloc of space for anyValues array failed ...\n" );
	 } 

         int status = -1;

         switch ( var.type ) 
	 {
	    case NC_CHAR:
	       status = nc_get_vara_text( _ncid, iv, start, count, 
		                          (char*)*anyValues );
	       break;
	    case NC_SHORT:
	       status = nc_get_vara_short( _ncid, iv, start, count, 
		                           (short*)*anyValues );
	       break;
	    case NC_INT:
	       status = nc_get_vara_long( _ncid, iv, start, count, 
		                         (long*)*anyValues );
	       break;
	    case NC_FLOAT:
	       status = nc_get_vara_float( _ncid, iv, start, count, 
		                           (float*)*anyValues );
	       break;
	    case NC_DOUBLE:
	       status  = nc_get_vara_double( _ncid, iv, start, count, 
		                             (double*)*anyValues );
	       break;
	    default:
	       logMessageWithArgsAndExitOnError( FATAL_LEVEL,
	       "ERROR - vardata: bad type...\n" );
	 }

         if ( status != NC_NOERR )
	 {
	      free ( *anyValues );
	      anyValues = NULL;
	      nc_close ( _ncid );
              logMessageWithArgsAndExitOnError( FATAL_LEVEL,
	      "Error reading array from file %s.\n");
	 }

	 break;

      }//end if ( !strcmp(var.name, strname) )
   }

   if ( *sizeArr <= 0 )
   { 
      logMessageWithArgsAndExitOnError( WARNING_LEVEL,
      "Could not find %s data from netcdf file\n", strname );
   }

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "End getNetcdfData() - Variable:  %d - %s\n", _nvars, strname);
   }
}

/* Opens and reads netCDF files ----------------------------------------------*/
void openAndInquiryNETCDF( char *filename )
{
   int i;
   int sizeArray;

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "Start openAndInquiryNETCDF() ...\n");
   }

   /* Open netcdf file */
   int retval =  nc_open( filename, NC_NOWRITE, &_ncid );

   if ( retval != NC_NOERR  )
   {
      logMessageWithArgsAndExitOnError( FATAL_LEVEL,
      "ERROR: Could not open the netcdf file: %s\n", filename );
   }
    
   /* Get number of dimensions, number of variables, number of global
    * atts, and dimension id of unlimited dimension, if any
    */
   if( nc_inq( _ncid, &_ndims, &_nvars, &_ngatts, &_unlimdim ) ) 
   {
      logMessageWithArgsAndExitOnError( FATAL_LEVEL,
      "\n Could not get number of dimension, variable and globall"
      " attribute in %s file\n", filename );
   }

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "ndims: %d nvars: %d ngatts: %d recdim: %d\n",
      _ndims, _nvars, _ngatts, _unlimdim );
   }

   if ( getFewsDebugFlag() > 3 )
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "End openAndInquiryNETCDF() ..." );
   }
}

/* Close netCDF file */
void closeNetCDF()
{
   nc_close( _ncid );
}
