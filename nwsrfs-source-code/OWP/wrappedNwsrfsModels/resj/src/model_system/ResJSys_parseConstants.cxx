//------------------------------------------------------------------------------
// ResJSys :: parseConstants - parses the topology info out of the file.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 20 Feb 1998  Daniel Weiler, RTi	Created initial version.
// 14 May 2001	James R. VanShaar, RTi	Improved error handling
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//	fname	I	Control file name.
//
//------------------------------------------------------------------------------
#include "ResJSys.h"
#include "Method.h"
#include "resj_main.h"


int ResJSys :: parseConstants( char** param_list, int n_items )
{
	char	routine[]="(ResJSys :: parseConstants) -", **list = NULL, 
		**comp_list = NULL; 
	int 	nlist = 0, i, clist = 0, totErrs=0;
	Component* comp = NULL;

	for( i = 0; i< n_items; i++ ) {
		if( strlen( param_list[i] ) == 0 || param_list[i][0] == '#' ){
			continue;
		}
		// Break each string from the param_list  
		list = BreakStringList( param_list[i], " \n\t", 
			DELIM_SKIP_BLANKS, &nlist);

		// Do constant parameters exist in control file?
		//if( !strcasecmp( list[0], "CONSTANT" ) ) {
		//printf("list 0 = %s, nlist = %d\n", list[0], nlist);
		if( strcasecmp( list[0], "CONSTANT" ) == 0 ) {
			if( nlist != 3 ) {
				//totErrs++;								
				logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		                "%s %s CONSTANT identifier "
				"\"%s\" incorrectly formed.",
				errorStr, routine, param_list[i]);
				/*AVCP
				PrintError( routine,"CONSTANT identifier "
					"\"%s\" incorrectly formed.",
					param_list[i] );
				
				list = FreeStringList( list );	
				continue;*/
			}

			// Break out the 
			// "ComponentIdentifier.ConstantIdentifier" separately
			comp_list = BreakStringList( list[1], ".",
				DELIM_SKIP_BLANKS, &clist );
			//printf("list 1 = %s, clist = %d\n", list[1], clist);

			if( clist != 2 ) {
				//totErrs++;					
				
				logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		                "%s %s CONSTANT identifier "
				"\"%s\" malformed.",
				errorStr, routine, list[1] );
				/* AVCP comp_list = FreeStringList( comp_list );
					
			        list = FreeStringList( list );
				
				PrintError( routine, "CONSTANT identifier "
					"\"%s\" malformed.", list[1] );				
				continue;*/
			}
			
			/*for (int ii = 0; ii < clist; ii++) {
				printf("comp list %d = %s, ", ii, comp_list[ii]);
			}
			printf("\n");*/
			
			// Try to get the Component from the identifier
			comp = _root->getComponentPtr( comp_list[0] );
			if( comp == NULL ) {
				//totErrs++;	
				logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		                "%s %s Troubles getting "
				"component '%s' for CONSTANT \"%s\".",
				errorStr, routine, comp_list[0], list[1] );
				/*AVCP list = FreeStringList( list );
				comp_list = FreeStringList( comp_list );
								
				PrintError( routine, "Troubles getting "
					"component '%s' for CONSTANT \"%s\".",
					comp_list[0], list[1] );
			
				continue;*/
			}

			// Set the CONSTANT values on the component
			if( !IsDouble( list[2] ) ) {
			   
			   logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		           "%s %s %s value '%s' not in a "
			   "recognizable format for %s.",
			   errorStr, routine, list[0], list[2], list[1] );
		      
			   /* AVCP
			   PrintError( routine, "%s value '%s' not in a "
					"recognizable format for %s.", list[0],
					list[2], list[1] );
			   */
			   /*AVCP list = FreeStringList( list );
			   comp_list = FreeStringList( comp_list );	
				continue;*/
			}
			else if( comp->addConstant( comp_list[1], list[2] ) ) {
			   /* AVCP
			   PrintError( routine, "Troubles adding CONSTANT "
					"%s.", list[1] );
			   */		   
			   logMessageWithArgsAndExitOnError( FATAL_LEVEL, 
		           "%s %s Troubles adding CONSTANT "
			   "%s.", errorStr, routine, list[1] );	
			   /*AVCP list = FreeStringList( list );
			   comp_list = FreeStringList( comp_list );	
		  	   continue;*/
			}
			comp_list = FreeStringList( comp_list );
		}
		list = FreeStringList( list );
	}

	/*AVCP if ( totErrs > 0 ) {
		return( STATUS_FAILURE );
	}*/

	// We either didn't find constants any or read them all correctly
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ResJSys_parseConstants.cxx,v $";
 static char rcs_id2[] = "$Id: ResJSys_parseConstants.cxx,v 1.4 2006/10/26 15:31:08 hsu Exp $";}
/*  ===================================================  */

}
