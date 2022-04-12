/******************************************************************************
     Filename: ressngl_ReadWriteStates.c

     Description:
     ============
	This file contains functions such as setKey(), 
	getSchemaUtilityInformation(),  readStateRessngl(), and
	writeStateRessngl().

     Change History
     ==============

     DATE          VERSION    PROGRAMMERS           NOTES
     ----------    -------    -----------------     ----------------------
     05/09/08      1          Cham Pham             Initial implementation
			  
******************************************************************************/

#include "ressngl_ReadWriteStates.h"

			     
/******************************************************************************
     Module: setkey()

     Description:
     ============
	This function will set the current key format.
     
     Input: 
     -----
     int    ncode    - the schema/utility code 
     int    nlevel   - the schema/utility level

     Output:
     ------
     char*  curKey   - the key fromat string

     Change History
     ==============

     DATE          VERSION    PROGRAMMERS           NOTES
     ----------    -------    -----------------     ----------------------
     05/09/08      1          Cham Pham             Initial implementation
			  
******************************************************************************/
void setKey( int ncode, int nlevel, char *curKey )
{
   switch ( ncode )
   {   
      case SETQ:      /* 102 */
	     sprintf(curKey,"%s%d_", specificCOKey[0], nlevel);
             break;
      case SETH:      /* 103 */
	     sprintf(curKey,"%s%d_", specificCOKey[1], nlevel );
             break;
      case FILLSPILL: /* 105 */
	     sprintf(curKey,"%s%d_", specificCOKey[2], nlevel);
	     break;
      case SPILLWAY:  /* 106 */
	     sprintf(curKey,"%s%d_", specificCOKey[3], nlevel);
	     break;
      case STPOOLQ:   /* 106 */
	     sprintf(curKey,"%s%d_", specificCOKey[4], nlevel);
	     break;
      case INDSRCHGE: /* 111 */
	     sprintf(curKey,"%s%d_", specificCOKey[5], nlevel);
	     break;
      case FLASHBDS:  /* 112 */
	      sprintf(curKey,"%s%d_", specificCOKey[6], nlevel);
	      break;
      case POWERGEN:  /* 113 */
	      sprintf(curKey,"%s%d_", specificCOKey[7], nlevel);
	      break;
      case RULEADJ:   /* 151 */
	      sprintf(curKey,"%s%d_", specificCOKey[8], nlevel);
	      break;
      case ADJUST:    /* 154 */
	      sprintf(curKey,"%s%d_", specificCOKey[9], nlevel);
	      break;
      case BACKFLOW:  /* 155 */
	      sprintf(curKey,"%s%d_", specificCOKey[10], nlevel);
	      break;
      default:
	      break;  
   }

   if ( getFewsDebugFlag() > 3 )
   {
      logMessage( DEBUG_LEVEL, "End setKey() ...." );
   }

} /* setKey() ---------------------------------------------------------------*/

/******************************************************************************
     Module: getSchemaUtilityInformation()

     Description:
     ============
	This function will get the Schema/Utility information from POarray.
     
     Input: 
     -----
     float  POarray[]  - contains parameter information
     float  COarray[]  - contains carryover values

     Output:
     ------
     CarryOver_t     - structure contains CO infomation

     Change History
     ==============

     DATE          VERSION    PROGRAMMERS           NOTES
     ----------    -------    -----------------     ----------------------
     01/09/09      1          Cham Pham             Initial implementation
			  
******************************************************************************/
void getSchemaUtilityInformation( float POarray[], float COarray[], 
                                  CarryOver_t SU[] )
{
   int i, icode, codelevel, index;
   
   /* Initialize carryover index */
   COIDX = 0;
     
   /* Loop through the Schema/Utility location */
   for ( i = 0; i < (int)POarray[19]; i++ ) 
   {
      icode =(int)( POarray[20 + i * 4] / 10 );
      codelevel = (int)( (POarray[20 + i * 4] / 10. - icode ) * 10 );
      index = i * 4 + 4;

      if ( getFewsDebugFlag() > 4 ) 
      {
         logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	 "POarray[19+index] = %d\n",(int)POarray[19+index] );
      }
      
      if ( (int)POarray[19+index] > 0 )
      {
         SU[COIDX].icode = icode;
         SU[COIDX].ilevel = codelevel;
         SU[COIDX].iloc = POarray[19+index];
         SU[COIDX].nco = 0;
	 
         if ( getFewsDebugFlag() > 4 ) 
	 {
            logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	    " %d %d %d\n",SU[COIDX].icode, SU[COIDX].ilevel,
	    SU[COIDX].iloc );
	 }
	 
         COIDX++;
      }
      
   }/*end i looping */
   
   if ( getFewsDebugFlag() > 3 )
   {
      logMessage( DEBUG_LEVEL, "End getSchemaUtilityInformation() ...." );
   }

}/* getSchemaUtilityInformation() ------------------------------------------ */

/******************************************************************************
     Module: readStateRessngl()

     Description:
     ============
	This function will read the carryover (state) value(s) from stateI.txt
     file.
     
     Input: 
     -----
     float  POarray[]	- contains schema/utility code 

     Output:
     ------
     float  COarray[]	- contains carryover values

     Change History
     ==============

     DATE          VERSION    PROGRAMMERS           NOTES
     ----------    -------    -----------------     ----------------------
     05/09/08      1          Cham Pham             Initial implementation
			  
******************************************************************************/
void readStateRessngl( float POarray[], float COarray[] )
{
   int   i, n;
   int   numbOfStates, stateUnit, nStates;
   int   itype, num, icode, codelevel;
   int   index, nco, incrCO;
   float Funit[7];
   char  currKey[256]; 
   char  *unitStr = NULL;
   CarryOver_t iSU[500];

   /* Get (PO(15)) number of carryover values (general and specific) needed */
   nStates = POarray[14];    

   /* Read states information from statesI.txt file */
   readStatesFromFile( &numbOfStates, &stateUnit );

   if ( getFewsDebugFlag() > 4 ) 
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
      "stateunit (%d)\t#states (%d), (%d)",
       stateUnit, nStates, numbOfStates );
   }
   
   /* Read Unit string ALWAYS 'METRIC' */
   unitStr = populateStringStateValue( "UNIT", Funit, 1, strlen("METRIC") );
   
   /* Check number of input States with the number of carray over in P array */
   if ( numbOfStates != nStates ) 
   {
      logMessageAndExitOnError( FATAL_LEVEL, "Number of input states and number"
	                       " of states read from PO array are NOT equal." );
   }
   
   /* Populate the GENERAL carryover values (including 6 fields) */
   for ( i = 0; i < GENERAL_CARRYOVER; i++ ) 
   {
      populateFloatStateValue( generalCOKey[i], COarray, i+1 );   
   }

   /* Populate the SPECIFIC carryover values */
   getSchemaUtilityInformation( POarray, COarray, iSU );

   incrCO = GENERAL_CARRYOVER;
  
   for ( i = 0; i < COIDX; i++ )
   {  
      icode = iSU[i].icode;
      
      nco = iSU[i+1].iloc - iSU[i].iloc;

      if ( i == COIDX-1 ) nco = nStates - iSU[i].iloc + 1; 

      setKey( icode, iSU[i].ilevel, currKey );
      
      if ( (nco > 0) &&
	   (icode==SETQ || icode==SETH || icode==FILLSPILL || 
	    icode==SPILLWAY || icode==STPOOLQ || icode==INDSRCHGE ||
	    icode==FLASHBDS || icode==POWERGEN || icode==RULEADJ || 
	    icode==ADJUST || icode==BACKFLOW) )
      {         
         if ( getFewsDebugFlag() > 4 ) 
	 {
            logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	    "icode (%d); idxcode(%d); nco(%d)\n", 
	    iSU[i].icode, iSU[i].ilevel, nco);
	 }
	  
         populateArrayOfFloatStateValues( currKey, nco, COarray, incrCO+1 );

         if ( getFewsDebugFlag() > 4 ) 
	 {
            logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	    "(%d.%d):  NCO: %d\tincrCO(%d)\n", 
	    icode,  iSU[i].ilevel, nco, incrCO );
            for ( n = 0; n < nco; n++ ) 
	    {
               logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	       "key: %s%d value: %f\n", currKey, n, COarray[nco+n] );
	    }
         }
         
	 incrCO += nco;
      }
   } /*end i loop */

   if ( getFewsDebugFlag() > 3 ) 
   {
      for ( i = 0; i < nStates; i++ ) 
      {
	 logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	 "COarray[%d] = %f\n",i+1, COarray[i] );
      }
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL, "End readState() ....\n" );
   }
   
} /* readStatesRessngl() --------------------------------------------------- */

/******************************************************************************
     Module: writeStateRessngl()

     Description:
     ============
	This function will write the carryover (state) values to stateO.txt
     file.
     
     Input: 
     -----
     float  COarray[]	- contains carryover values
     float  POarray[]   - contains parameter infomation
     
     Output:
     ------
     NONE

     Change History
     ==============

     DATE          VERSION    PROGRAMMERS           NOTES
     ----------    -------    -----------------     ----------------------
     05/09/08      1          Cham Pham             Initial implementation
			  
******************************************************************************/
void writeStateRessngl( float POarray[], float COarray[], 
                        char* outputStatesFileName )
{
   int i, n;
   int icode, codelevel, index;
   int incrCO, nco, nStates;
   char curKey[256];
   FILE *outputStateFilePtr = NULL;
   CarryOver_t oSU[500];
   
   /* Get number of carryover values PO(15) */ 
   nStates = POarray[14];      
   
   /* Open state file */
   outputStateFilePtr = fopen( outputStatesFileName, "w+" );
   
   if ( outputStateFilePtr == NULL )
   {
      logMessageWithArgsAndExitOnError( FATAL_LEVEL,
      "ERROR: could not open the %s\n", outputStatesFileName );
   }

   /* Write out unit */  
   writeStringStateToFile( outputStateFilePtr, "UNIT", "METRIC" );

   /* Write GENERAL carryover to file */
   for ( i = 0; i < GENERAL_CARRYOVER; i++ ) 
   {
      writeFloatStateToFile(outputStateFilePtr, generalCOKey[i], COarray, i+1);
   }
   
   /* Write SPECIFIC carryover to file */
   getSchemaUtilityInformation( POarray, COarray, oSU );
   
   incrCO = GENERAL_CARRYOVER;
   
   for ( i = 0; i < COIDX ; i++ )
   {
      icode = oSU[i].icode;
      nco = oSU[i+1].iloc - oSU[i].iloc;
      if ( i == COIDX-1 ) nco = nStates - oSU[i].iloc + 1;
      
      setKey( icode, oSU[i].ilevel, curKey );
      
      if ( (nco > 0) &&
	   (icode==SETQ || icode==SETH || icode==FILLSPILL || 
	    icode==SPILLWAY || icode==STPOOLQ || icode==INDSRCHGE ||
	    icode==FLASHBDS || icode==POWERGEN || icode==RULEADJ || 
	    icode==ADJUST || icode==BACKFLOW) )
      {
         writeArrayOfFloatStatesToFile( outputStateFilePtr, curKey, COarray, 
	                                incrCO+1, nco );
         if ( getFewsDebugFlag() > 4 )
         {
            for ( n = 0; n < oSU[i].nco; n++ ) 
	    {
               logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	       "icode(%d) ilevel(%d) curKey(%s%d) nco(%d) CO(%f)\n",
	        oSU[i].icode, oSU[i].ilevel, curKey, n, oSU[i].nco, 
	        COarray[incrCO+n] );
	    }
	 }
	  
	 incrCO += nco;
      }
   } 

   /* Close output state file (stateO.txt) */
   fclose( outputStateFilePtr );

   if ( getFewsDebugFlag() > 3 ) 
   {
      logMessageWithArgsAndExitOnError( DEBUG_LEVEL, "End writeState() ....\n");
   }

} /* writeStateRessngl() --------------------------------------------------- */
