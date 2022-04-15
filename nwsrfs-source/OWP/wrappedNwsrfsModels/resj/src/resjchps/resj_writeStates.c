/*****************************************************************************
  * Module Name: resj_writeStates()
  *
  * Original Author:  Cham Pham / Ai Vo
  * 
  * Module Creation Date: 08/18/08
  * 
  * Description:
  *   Write output states values to file
  *
  * Global Variables Used:
  *   Type        Name           Origin    Description
  *
  *   int         NumCOinflow              number of lagk co inflow
  *   char*       *unitStrs                string contains model unit 
  *                                        (METRIC / ENGLISH)
  *
  *
  * Calling Arguments:
  *
  * Name          Input/Output    Type       Description
  * PO            In              float[]    contains parameter information
  * CO            In              float[]    CarryOver data
  *
  *  Change History
  *  ==============
  *  DATE          VERSION    PROGRAMMERS        NOTES*
  *  ----------    -------    -----------------  ----------------------
  * 02/24/11      1          Cham, Kuang, Russ   Bug 426 - increase the position
  *            *                                 to hold the CO string index
  *            *
  ***************************************************************************/
#include "resj_main.h"

int NumCOinflow;

void resj_writeStates( float POarray[], float COarray[], 
                       char* outputStatesFileName )
{
   int i, j;
   char key[100];
   char tmpval[512],tmpstr[80];
   float fValue[17];

   FILE *outputStateFilePtr;
   char *stringval = NULL;
   char *substring = NULL;
  
   int  sIndex, nIndex; 
   char nextfield[7];
   char stateKeys[13], idString[13];
   char methodType[13];
   char reservoirID[13];
   
   int coinflow_size, coinflow_bytes;
   int numCOelements = (int)POarray[3];
   int strSize = (int)(numCOelements*sizeof(float)+1);

   char *char_co = (char*)malloc( strSize );
   
   if ( getFewsDebugFlag() >= 3 ) 
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
      "Entering resj_writeStates()....");
   } 
   
   outputStateFilePtr = fopen( outputStatesFileName, "w+" );
      
   if ( outputStateFilePtr == NULL )
   {
 
      logMessageWithArgsAndExitOnError( FATAL_LEVEL,
         "ERROR: could not open the %s\n", outputStatesFileName );

   }
   memset( char_co, '\0', sizeof(char_co) );

   memcpy( char_co, COarray, strSize-1 );

   char_co[ strSize-1 ] = '\0';
   
   if ( getFewsDebugFlag() >= 3 ) 
   { 
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
           "writeStates: ouput CO array =  %s\n", char_co );
   } 	    

   /*reservoir model always uses metric unit - check nwsrfs document*/
   writeStringStateToFile( outputStateFilePtr, "UNIT", "METRIC" );
   
   if ( char_co == NULL ) 
   {
      logMessageWithArgsAndExitOnError( FATAL_LEVEL,
      "ERROR: ouput CO array is empty\n");
   }

   stringval = strpbrk(char_co, "RNM");
   substring = stringval;

   while ( TRUE )
   {
      strncpy( stateKeys, substring, 12 );      
      stateKeys[12] = '\0';    
      removeTrailingSpaces( stateKeys );      
      strncpy( idString, substring+12, 12 );                    
      idString[12] = '\0';     
      removeTrailingSpaces( idString );      
      tmpval[8]='\0';
      strncpy( nextfield, substring+24, 6 );
      nextfield[6] = '\0'; 
 
      /* RESERVOIR ---------------------------------------------------- */
      if ( !strcmp(stateKeys,"RESERVOIR") )
      {
         nIndex = 0;
         for ( j = 0; j < MAX_RESERVOIR_ELEMENT; j++ )
	 {	    
	    sprintf(key,"%s_%s_%s",stateKeys, idString, reservoirKey[j]);
	    /*RESERVOIR carryover [12+12+4+8+8+8+8+8+8+8+8] */
	    strncpy(tmpval, &substring[THIRTY+nIndex*8], 8);
	    nIndex ++;	    
            tmpval[8] = '\0';
	    fValue[j] = atof( tmpval ); 	       
	    writeFloatStateToFile( outputStateFilePtr, key, fValue, j+1 );
	    if ( getFewsDebugFlag() >= 3 ) 
	    { 
               logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	             "writeStates:  %s %s\n", key, tmpval );
	    } 	    
	 }       
      }
      /* NODE --------------------------------------------------------- */
      else if ( !strcmp( stateKeys, "NODE" ) )
      {
         nIndex = 0;
         for ( j = 0; j < MAX_NODE_ELEMENT; j++ )
	 {
	    sprintf( key, "%s_%s_%s", stateKeys, idString, nodeKey[j] );
	    strncpy( tmpval, &substring[THIRTY+nIndex*8], 8 );
	    nIndex ++;	    
            tmpval[8] = '\0'; 
	    if ( !strncmp( tmpval, "*FUTURE*", 8 ) && (j > 1) )
            {
	       fValue[j] = -888.0;
	    }
	    else if( !strncmp( tmpval, "MISS", 4 ) && (j > 3) )
	    {
	       fValue[j] = -999.0; 
	    }
	    else
            {	            
	       fValue[j] = atof( tmpval ); 	       
	    }
	    writeFloatStateToFile( outputStateFilePtr, key, fValue, j+1 );
	    if ( getFewsDebugFlag() >= 3 ) 
	    { 
               logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	             "writeStates:  %s %s\n", key, tmpval );
	    }
	 }
   
      }
      /* METHODS ----------------------------------------------------- */
      else if ( !strcmp( stateKeys, "METHOD" ) )
      {
         strncpy( methodType, substring+THIRTY, 12 );
	 methodType[12]='\0';	
	 removeTrailingSpaces( methodType );	 
	 strncpy( reservoirID, substring+THIRTY+12, 12 );
	 reservoirID[12]='\0';	
	 removeTrailingSpaces( reservoirID );
	  /* Method - SPILLWAY */
	 if ( !strcmp( methodType, "SPILLWAY" ) )
	 {  
            sprintf( key, "%s_%s_%s_%s", methodType, idString, reservoirID, 
		                         "INITIALSPILL" );	      
	    strncpy( tmpval, &substring[FIFTYFOUR], 8 );
	    fValue[0] = atof( tmpval ); 	       
            writeFloatStateToFile( outputStateFilePtr, key, fValue, 1 ); 
	    if ( getFewsDebugFlag() >= 3 ) 
	    { 
               logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	             "writeStates:  %s %s\n", key, tmpval );
	    }
	 }
	 /* Method - SETWITHDRAW */
	 else if ( !strcmp( methodType, "SETWITHDRAW" ) )
	 {
            nIndex = 0;
            for ( j = 0; j < MAX_SETWITHDRAW_ELEMENT; j++ )
	    {
	       sprintf( key, "%s_%s_%s_%s", methodType, idString, reservoirID, 
		                           setRelElvWidKey[j] );
	       if ( j < 2 )
	       {
		  strncpy(tmpval, &substring[FIFTYFOUR+nIndex*4], 4 );
		  nIndex ++; 	       
		  if( !strcmp(tmpval,"    ") )
		  {
		     strcpy(tmpval,"NONE");
		     writeStringStateToFile( outputStateFilePtr, key, tmpval );
		  }
		  else
		  {
		     tmpval[4] = '\0';
		     fValue[j] = atof( tmpval );
	             writeIntStateToFile( outputStateFilePtr, key, fValue, j+1 );
	             if ( getFewsDebugFlag() >= 3 ) 
	             { 
                        logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	                      "writeStates:  %s %s\n", key, tmpval );
	             } 	    
		  }
                  
	       }
	       else
	       {
		  strncpy( tmpval, &substring[FIFTYFOUR+8], 8 );
		  tmpval[8] = '\0';
		  fValue[j] = atof( tmpval );
	          writeFloatStateToFile( outputStateFilePtr, key, fValue, j+1 );
	          if ( getFewsDebugFlag() >= 3 ) 
	          { 
                     logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	                   "writeStates:  %s %s\n", key, tmpval );
	          } 	    
	       }
	      
	    }
	 }
	 /* Method - CALCINFLOW */
	 else if ( !strcmp(methodType, "CALCINFLOW" ) )
	 {
            for ( j = 0; j < MAX_CALINFLOW_ELEMENT; j++ )
	    {

	       
	       sprintf( key, "%s_%s_%s_%s", methodType, idString, reservoirID, 
		                            calcInflowKey[j] );
	       if(!strcasecmp( calcInflowKey[j], "REMAININGVOL" ) )
	       {	       
		  strncpy( tmpval, &substring[FIFTYFOUR], 16 );
		  tmpval[16] = '\0';
	       }
	       else
	       {
		  strncpy( tmpval, &substring[FIFTYFOUR+2*8], 8 );
		  tmpval[8] = '\0';
	       }
	         
	       if( !strncmp( tmpval, "MISSINGX", 8 ) )
               {
        	  fValue[j] = -999.0; 
               }
               else 
               {
        	  fValue[j] = atof( tmpval ); 
               }
	       writeFloatStateToFile( outputStateFilePtr, key, fValue, j+1 );	
               if ( getFewsDebugFlag() >= 3 ) 
	       { 
                   logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	                     "writeStates:  %s %s\n", key, tmpval );
	       }
            } /* end j loop */

	 }
	 /* Method - ADJUST */
	 else if ( !strcmp(methodType,"ADJUST") )
	 {
            sprintf( key, "%s_%s_%s_%s", methodType, idString, reservoirID, 
		                         "BLENDTS" );
	    strncpy( tmpval, &substring[FIFTYFOUR], 4 );	        
            tmpval[4] = '\0';
	    fValue[0] = atof( tmpval ); 	       
            writeIntStateToFile( outputStateFilePtr, key, fValue, 1 ); 
	    if ( getFewsDebugFlag() >= 3 ) 
	    { 
               logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	             "writeStates:  %s %s\n", key, tmpval );
	    }
	 }
	 /* Method - SETRELEASE or SETELEVATION */
	 else if ( !strcmp( methodType, "SETRELEASE" ) || 
	           !strcmp( methodType, "SETELEVATION" ) )
	 {
            nIndex = 0;
            for ( j = 0; j < MAX_SETRELELEV_ELEMENT; j++ )
	    {
	       sprintf( key, "%s_%s_%s_%s", methodType, idString, reservoirID, 
		                            setRelElvWidKey[j] );	

	       strncpy( tmpval, &substring[FIFTYFOUR+nIndex*4], 4);
	       nIndex ++;  
	       tmpval[4] ='\0';	       
	       fValue[j] = atof( tmpval );
	       writeIntStateToFile( outputStateFilePtr, key, fValue, j+1 ); 
	       if ( getFewsDebugFlag() >= 3 ) 
	       { 
                   logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	             "writeStates:  %s %s\n", key, tmpval );
	       }
	    }

	    strncpy( tmpstr, &substring[FIFTYFOUR+24+8+44], 10);
	    sIndex = 0;
	    int k = 0;
	    while ( !strcmp(tmpstr, "SETRELEASE") || 
	            !strcmp(tmpstr, "SETELEVATION") )
	    {
		strncpy( tmpstr, &substring[24+nIndex*76], 10);	     
		strncpy( tmpval, &substring[24+nIndex*76+nIndex*4], 4);        	
		nIndex ++;
                tmpval[4] ='\0';
		fValue[k] = atof( tmpval );
		writeFloatStateToFile( outputStateFilePtr, key, fValue, k+1 );
		k++;
		if ( getFewsDebugFlag() >= 3 ) 
	        { 
                   logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	             "writeStates:  %s %s\n", key, tmpval );
	        }  
            }

	 }
	 /* METHOD: LOOKUP3 carryover (24+4+4+4+4+8) */
	 else if ( !strcmp( methodType, "LOOKUP3" ) )
	 {
            nIndex = 0;

            for ( j = 0; j < MAX_LOOKUP3_ELEMENT; j++ )
	    {
	       
	       sprintf( key, "%s_%s_%s_%s", methodType, idString, reservoirID, 
		                            lookup3Key[j] );

	       if ( j < 4 )
	       {
		  strncpy( tmpval, &substring[FIFTYFOUR+nIndex*4], 4);
		  nIndex ++;  
		  tmpval[4] = '\0';
		  fValue[j] = atof( tmpval ); 
		  if( !strncmp (tmpval, "SKIP", 4) )
		  {
	             fValue[j] = -999; 
		  }        	  
		  writeIntStateToFile( outputStateFilePtr, key, fValue, j+1 );
	       }
	       else
	       {
		  strncpy(tmpval, &substring[FIFTYFOUR+16], 8 );  
		  tmpval[8] = '\0';                  
        	  if( !strncasecmp ( tmpval, "MISS",4) )
        	  { 
			 fValue[j]= -999.0;
        	  }
		  else
		  {
	              fValue[j] = atof( tmpval );
        	  }		  
        	  writeFloatStateToFile( outputStateFilePtr, key, fValue, j+1 );
	       }
	       if ( getFewsDebugFlag() >= 3 ) 
	       { 
                  logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	                "writeStates:  %s %s\n", key, tmpval );
	       } 	    

	    }
	 }
      }/* METHOD - REACH LAGK ------------------------------------------- */
      else if( !strcmp( stateKeys, "REACH" ) )
      {
         strncpy( methodType, substring+THIRTY, 12 );
	 methodType[12]='\0';	
	 removeTrailingSpaces( methodType );
	 nIndex = 0;
	
          /* Compute number of CO inflow */
	 coinflow_size = strcspn( substring, "*");
	 coinflow_bytes = (coinflow_size-24) - FIFTYFOUR;
	 NumCOinflow = (int)(coinflow_bytes / 8.);
	 
	 for ( j = 0; j < NumCOinflow; j++ )
	 {
            sprintf( key, "%s_%s_%s_#%d", methodType, idString, lagkKey[0], j );
	    strncpy(tmpval, &substring[FIFTYFOUR+nIndex*8], 8 );
	    tmpval[8] = '\0';
	    nIndex++;  
	    fValue[j] = atof(tmpval);
            writeFloatStateToFile( outputStateFilePtr, key, fValue, j+1 );
	 }
	 
	 for ( j = 1; j < MAX_LAGK_ELEMENT; j++ )
	 {
	    sprintf(key, "%s_%s_%s", methodType, idString, lagkKey[j] );
	    strncpy(tmpval, &substring[FIFTYFOUR+8+8] + j*8 +
		    coinflow_bytes-24, 8 );
	    tmpval[8] = '\0';
	    fValue[j] = atof( tmpval );
	    writeFloatStateToFile( outputStateFilePtr, key, fValue, j+1 );
	 }
      }

      /* If it is last string set, then exists the while loop */
      if ( atoi(nextfield) == -99999) {
         break; 
      }

      substring = strpbrk( stringval+atoi( nextfield ), "RNM" );
      if ( substring == NULL ) 
      {
  	 break;
      }
      
   }/* end while (TRUE) loop */
   
   /* Close output state file (statesO.txt) */
   fclose( outputStateFilePtr );
   free( char_co );
   
   if ( getFewsDebugFlag() >= 3 ) 
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
      "End resj_writeState()....");
   } 

} /* resj_writeStates() ---------------------------------------------------- */
