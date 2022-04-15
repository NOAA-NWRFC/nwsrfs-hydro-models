/*****************************************************************************
  * Module Name: resj_readStates()
  *
  * Original Author:   Cham Pham
  * 
  * Module Creation Date: 08/18/08
  * 
  * Description:
  *   Read states values from statesI.txt and fill in the read information
  *   into CO array
  * 
  *  Global Variables set:
  *   Type        Name           Origin    Description
  *
  *   int         NumCOinflow              number of lagk CO inflow
  *   char*       *unitStrs                string contains model unit 
  *                                        (METRIC / ENGLISH)
  *
  * Calling Arguments:
  *
  * Name          Input/Output    Type       Description
  * PO            In              float[]    contains parameter information
  * CO            out             float[]    CarryOver data
  *
  *
  *  Change History
  * ==============
  * DATE          VERSION    PROGRAMMERS        NOTES*
  * ----------    -------    -----------------  ----------------------
  * 02/24/11      1          Cham, Kuang, Russ  Bug 426 - increase the position
  *                                             to hold the CO string index 
  ***************************************************************************/
  
#include "resj_main.h"

int NumCOinflow;
char *unitStr;

void resj_readStates( float POarray[], float COarray[] )
{
   int  i, j;
   int  numbOfStates;
   int  stateUnit;
   int  numCOindex, index;
   int  coinflow_size; 
   int  coinflow_bytes;
   
   char *stringval = NULL,
	*strState = NULL;
   char *substring = NULL;

   char idKey[13], CompMthdKey[80];
   char ownerIdKey[13], ownerId[13];
   char methodName[13]; 
   char curKey[256];
   char strVal[512];
   char lastfield[7];
   char tmpstr[80];
   float floatVal, Funit[9];	
   int  strSize = 0; 

   /* Read states information from statesI.txt file */
   readStatesFromFile( &numbOfStates, &stateUnit );

   /* Read Unit string - stateUnit = 1 (METRIC); = 0 (ENGLISH) */
   if ( stateUnit > 0 )
      unitStr = populateStringStateValue( "UNIT", Funit, 1, strlen("METRIC") );
   else
      unitStr = populateStringStateValue( "UNIT", Funit, 1, strlen("ENGLISH") );

   /* Get the number of CO elements in floating-point array */
   numCOindex = (int)POarray[3];
   if ( numCOindex <= 0 ) 
   {
      logMessageAndExitOnError(FATAL_LEVEL, "ERROR: numCOindex less/equal 0" );
   }

   strSize = numCOindex * sizeof(float) + 1;

   /* Allocate memory for string char_co and copy floating-point array COarray
    * to string char_co 
    */
   char *char_co = (char*)malloc( strSize );

   if ( char_co == NULL ) {
      logMessageAndExitOnError(FATAL_LEVEL, "ERROR: reading carryover char_co");
   }
   
   memcpy( char_co, COarray, strSize - 1);
   char_co[ strSize-1 ] = '\0';
   
   /* Search first character in char_co string set, that is R(eservoir), 
    * R(each), N(ode) or M(ethod) 
    */
   stringval = strpbrk( char_co, "RNM" );

   /* Copy stringval to substring */
   substring = stringval;

   if ( getFewsDebugFlag() >= 3 ) {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "==> stringval:\n%s\n\n", stringval );
   }
   
   /* Initialize index to zero */
   index = 0;  

   memset(CompMthdKey, 0, sizeof(CompMthdKey));
   memset(idKey, 0, sizeof(idKey));
   memset(methodName, 0, sizeof(methodName));
   memset(ownerId, 0, sizeof(ownerId));
   while ( TRUE ) 
   {

       if ( getFewsDebugFlag() >= 3 ) { 
          logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	  "----- START NEW ------\nINDEX(%d): %s\n\n", 
	  index, substring );
       }

       strncpy(CompMthdKey, substring, 12);   /* Find component or method key */
       removeTrailingSpaces( CompMthdKey );
       CompMthdKey[12] = '\0';

       strncpy(idKey, substring+12, 12);      /* Find id key                  */
       removeTrailingSpaces( idKey );
       idKey[12] = '\0';

       /* RESERVOIR component ----------------------------------------------- */
       if ( !strcmp(CompMthdKey, "RESERVOIR") ) 
       {
	  strVal[0] = '\0';	

          for ( j = 0; j < MAX_RESERVOIR_ELEMENT; j++ )
          {
             tmpstr[0] = '\0';
             sprintf(curKey, "%s_%s_%s", CompMthdKey, idKey, reservoirKey[j]);
             strState = populateStringStateValue( curKey, &floatVal, j+1, 8 );
             sprintf(tmpstr,"%8.8s", strState);
             strncat(strVal, tmpstr, 8);
          } /* end j loop */
	  
          strVal[64] = '\0';
	  if ( getFewsDebugFlag() >= 3 ) {
	     logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
	     "\nstrVal: %s\n", strVal );
	  }
          memcpy( char_co+index+THIRTY, strVal, 64*sizeof(char) );
       } 
       /* NODE component ---------------------------------------------------- */
       else if ( !strcmp( CompMthdKey, "NODE" ) )
       {
	  strVal[0] = '\0';	
	  
	  for ( j = 0; j < MAX_NODE_ELEMENT ; j++ )
	  {
             tmpstr[0] = '\0';

	     sprintf( curKey, "%s_%s_%s", CompMthdKey, idKey, nodeKey[j] );
	     strState = populateStringStateValue( curKey, &floatVal, j+1, 8 );

             sprintf(tmpstr, "%f", atof(strState));
	     
             /* Check for new INFLOW and DIVERSION carryover */
	     if ( (atof(tmpstr) == -888.) && (j > 1) ) {
	        sprintf(tmpstr, "%8.8s", "*FUTURE*");
	     }
	     /* Check for old DIVERSION carryover missing */
	     else if ( (atof(tmpstr) == -999)  && (j > 3) ) {
		sprintf( tmpstr, "%-8.8s", "MISS" );
	     }
	     
	     strncat(strVal, tmpstr, 8);
	  } /* end j loop */
	  
          strVal[48] = '\0';
	  if ( getFewsDebugFlag() >= 3 ) {
	     logMessageWithArgsAndExitOnError( DEBUG_LEVEL, "strVal: %s", 
		                               strVal );
	  }
	  memcpy( char_co+index+THIRTY, strVal, 48*sizeof(char));
       }
       /* REACH LAGK Method ------------------------------------------------- */
       else if ( !strcmp( CompMthdKey, "REACH") )
       {
          strVal[0] = '\0';

          strncpy( methodName, substring+THIRTY, 12 );  /* Find method name       */
          removeTrailingSpaces( methodName );
          methodName[12] = '\0';
	  
	  /* Compute number of CO inflow */ 
	  coinflow_size = strcspn(substring, "*");
          coinflow_bytes = (coinflow_size-24) - FIFTYFOUR;
	  NumCOinflow = (int)( coinflow_bytes / 8. );
	  
	  if ( getFewsDebugFlag() >= 3 ) {
	     logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
             "----->NumCOinflow = %d", NumCOinflow );
	  }

	  for ( j = 0; j < NumCOinflow; j++ )
	  {
	     tmpstr[0] = '\0';

	     sprintf( curKey, "%s_%s_%s_#%d", methodName, idKey, lagkKey[0], j);
	     strState = populateStringStateValue( curKey, &floatVal, j+1, 8 );
	     sprintf( tmpstr,"%8.8s", strState );
	     strncat( strVal, tmpstr, 8 );
	  }

	  for ( j = 1; j < MAX_LAGK_ELEMENT; j++ )
	  {
	     tmpstr[0] = '\0';

	     sprintf( curKey, "%s_%s_%s", methodName, idKey, lagkKey[j] );
	     strState = populateStringStateValue( curKey, &floatVal, j+1, 8 );
	     sprintf( tmpstr,"%8.8s", strState );
	     strncat( strVal, tmpstr, 8 );
	  }

	  strVal[(coinflow_bytes)+24] = '\0';

	  if ( getFewsDebugFlag() >= 3 ) {
	     logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
             "strVal: %s", strVal );
	  }
	  memcpy( char_co+index+FIFTYFOUR, strVal, (coinflow_bytes+24)*sizeof(char) );
       }
       /* METHODS ----------------------------------------------------------- */
       else if ( !strcmp( CompMthdKey, "METHOD" ) )               
       {
	  strncpy( methodName, substring+THIRTY, 12 ); /* Find method name        */
          removeTrailingSpaces( methodName );
          methodName[12] = '\0';
	  
	  strncpy( ownerId, substring+42, 12 ); /* Find owner id              */
          removeTrailingSpaces( ownerId );
          ownerId[12] = '\0';
          
          /* Method - ADJUST */   
	  if ( !strcmp(methodName, "ADJUST") )
	  {
	     strVal[0] = '\0';	  
              
	     sprintf(curKey, "%s_%s_%s_%s", methodName, idKey, 
		                            ownerId, "BLENDTS");
	     strState = populateStringStateValue( curKey, &floatVal, j+1, 8 );
	     sprintf(strVal, "%4.4s", strState); 
             strVal[4] = '\0';
	     if ( getFewsDebugFlag() >= 3 ) {
	        logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
	        "strVal: %s", strVal );
	     }
	     memcpy( char_co+index+FIFTYFOUR, strVal, 4*sizeof(char) );
	  }
	  /* Method - CALCINFLOW */
	  else if ( !strcmp(methodName, "CALCINFLOW" ) )
          {
	     strVal[0] = '\0';	

	     for ( j = 0; j < MAX_CALINFLOW_ELEMENT; j++ )
	     {
		tmpstr[0] = '\0';

		sprintf(curKey,"%s_%s_%s_%s", methodName, idKey, 
		                              ownerId, calcInflowKey[j]);
		strState = populateStringStateValue( curKey, &floatVal, 
			                                j+1, 8 );
		sprintf( tmpstr,"%f", atof(strState) );
		
	        if ( atof(strState) == -999. ) { /* reset -999 to "MISSINGX"  */
		   sprintf(tmpstr, "%s", "MISSINGX");
		}

		if ( !strcmp( calcInflowKey[j], "REMAININGVOL") ) {
		   strncat( strVal, tmpstr, 16 );
	        }
		else {
		   strncat( strVal, tmpstr, 8 );
		}
	     }
	     
             strVal[48] = '\0';
	     if ( getFewsDebugFlag() >= 3 ) {
	        logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
	        "strVal: %s", strVal );
	     }
	     memcpy( char_co+index+FIFTYFOUR, strVal, 48*sizeof(char) );
          }
	  /* Method - LOOKUP3 */
          else if ( !strcmp(methodName, "LOOKUP3") )
	  {
	     strVal[0] = '\0';	

	     for ( j = 0; j < MAX_LOOKUP3_ELEMENT; j++ )
	     {
		tmpstr[0] = '\0';

		sprintf( curKey, "%s_%s_%s_%s", methodName, idKey, 
		                               ownerId, lookup3Key[j] );
		strState = populateStringStateValue( curKey, &floatVal,
		                                     j+1, 8 );

		if ( !strcmp(lookup3Key[j], "INITIALTRANSFER") )
		{
		   sprintf( tmpstr,"%f", atof( strState ));
		   if ( atof( strState ) == -999. ) {
		      sprintf( tmpstr,"%-8.8s", "MISS" );
		   }
		   strncat( strVal, tmpstr, 8 );
		}
		else
		{
		   sprintf( tmpstr,"%4d", atoi( strState ) );
		   if ( atoi( strState ) == -999 ) {
		      sprintf( tmpstr,"%s", "SKIP" );
		   }
		   strncat(strVal, tmpstr, 4);
		}
	     }

             strVal[24] = '\0';
	     if ( getFewsDebugFlag() >= 3 ) {
	        logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
                "strVal: %s", strVal );
	     }
	     memcpy(char_co+index+FIFTYFOUR, strVal, 24*sizeof(char));
	  }
	  /* Method - SPILLWAY */
	  else if ( !strcmp( methodName, "SPILLWAY" ) )
	  {
	     strVal[0] = '\0';	
	     sprintf( curKey, "%s_%s_%s_%s", methodName, idKey, 
		                            ownerId, "INITIALSPILL" );
	     strState = populateStringStateValue( curKey, &floatVal, j+1, 8 );
	     sprintf( strVal, "%f", atof( strState ) ); 
             strVal[8] = '\0';
	     if ( getFewsDebugFlag() >= 3 ) {
	        logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
	        "strVal: %s", strVal );
	     }
	     memcpy( char_co+index+FIFTYFOUR, strVal, 8*sizeof(char) );
	  }
	  /* Method - SETRELEASE or SETELEVATION */
	  else if ( !strcmp( methodName, "SETRELEASE" ) ||
		    !strcmp( methodName, "SETELEVATION" ) )
	  {
	     strVal[0] = '\0';	

	     for ( j = 0; j < MAX_SETRELELEV_ELEMENT; j++ )
	     {
		tmpstr[0] = '\0';

		sprintf( curKey, "%s_%s_%s_%s", methodName, idKey,
		                               ownerId, setRelElvWidKey[j]);
		logMessageWithArgsAndExitOnError( DEBUG_LEVEL,"curkey: %s\n",
		      curKey);
		strState = populateStringStateValue( curKey, &floatVal, 
		                                     j+1, 8 );
		sprintf( tmpstr,"%4d", atoi( strState ));
		strncat( strVal, tmpstr, 4 );
	     }

             strVal[8] = '\0';
	     if ( getFewsDebugFlag() >= 3 ) {
	        logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
	        "strVal: %s\n", strVal );
	     }
	     memcpy(char_co+index+FIFTYFOUR, strVal, 8*sizeof(char));
	  }
	  /* Method - SETWITHDRAW */ 
	  else if ( !strcmp( methodName, "SETWITHDRAW" ) )
	  {
	     strVal[0] = '\0';	
	     
	     for ( j = 0; j < MAX_SETWITHDRAW_ELEMENT; j++ )
	     {
		tmpstr[0] = '\0';

		sprintf(curKey, "%s_%s_%s_%s", methodName, idKey,
		                               ownerId, setRelElvWidKey[j]);
		strState = populateStringStateValue( curKey, &floatVal,
		                                     j+1, 8 );
	        if ( !strcmp( setRelElvWidKey[j], "INITIALTRANSFER" ) )
		{
		   sprintf( tmpstr,"%f", atof( strState ));
		   strncat( strVal, tmpstr, 8 );
		}
		else
		{	   
		   sprintf( tmpstr, "%4d", atoi( strState ) );
                   if ( !strcasecmp( strState, "NONE" ) ) {
                      sprintf( tmpstr, "%4s", " ");    
		   }
		   strncat( strVal, tmpstr, 4 );
		}
	     }

             strVal[16] = '\0';
	     if ( getFewsDebugFlag() >= 3 ) {
	        logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
	        "strVal: %s\n", strVal );
	     }
	     memcpy( char_co+index+FIFTYFOUR, strVal, 16*sizeof(char) );
	  }
       } /* end Methods if() */
		
       /* Find the last string set in the char_co array a value of -999 */
       lastfield[0] = '\0'; 
       strncat( lastfield, substring+24, 6 );
       lastfield[6] = '\0'; 
       
       /* Get the index for next string set */
       index = (int)atoi(lastfield);

       /* If it is last string set, then exists the while loop */
       if ( index == -99999 ) {
          break; 
       }

       /* Get the next string set */
       substring = strpbrk( stringval+index, "RNM" );
       if ( substring == NULL ) {
  	  break;
       }

   } /* end while() loop */

   if ( getFewsDebugFlag() >= 3 ) {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
      "Input COarray = %s\n\n", char_co );
   }
   
   /* Copy carryover in string char_co array to floating array */
   memcpy( COarray, char_co, strlen( char_co ) );

   /* free memory for char_co string */
   free( char_co );

   if ( getFewsDebugFlag() >= 3 ) {
      logMessageAndExitOnError( DEBUG_LEVEL, "Exit resj_readStates.c file\n" );
   }

} /* readStates() ---------------------------------------------------------- */
