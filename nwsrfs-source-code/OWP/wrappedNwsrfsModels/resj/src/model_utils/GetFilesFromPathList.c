/*-----------------------------------------------------------------------------
** HMGetFilesFromPathList -	given a string list and a file name, return a
**				list of paths
**-----------------------------------------------------------------------------
** Copyright:	See the COPYRIGHT file.
**-----------------------------------------------------------------------------
** Notes:	(1)	This routine takes a string list of directories and a
**			a file name and returns a string list of the full file
**			name.
**-----------------------------------------------------------------------------
** History:
** 02 Jan 1996	Steven A. Malers, RTi	First version, for use with the RTiDSS.
**-----------------------------------------------------------------------------
** Variable	I/O	Description
**
** file		I	Name of file to append to all the paths.
** fullfile	L	Full path of file.
** i		L	Loop counter for paths.
** listlen	O	The length of the returned string list.
** newlist	O	List of full paths.
** npaths	L	Number of strings in "paths".
** paths	I	The list of paths to be appended to.
** routine	L	Name of this routine.
**-----------------------------------------------------------------------------
*/

#include "resj_main.h"
#include "ResJ.h"

char **GetFilesFromPathList ( char **paths, char *file, int *listlen )
{	char	fullfile[256], **newlist = (char **)NULL,
		routine[] = "GetFilesFromPathList";
	int	i, npaths;

	/*
	** Check for NULL list, and file...
	*/

	*listlen = 0;
	if ( !paths ) {
		/*AVCP PrintWarning ( 10, routine,*/
	        logMessageWithArgsAndExitOnError( DEBUG_LEVEL,
	        "%s (%s) - NULL path list", warnStr, routine );
		return newlist;
	}
	if ( !file ) {
		/*AVCP PrintWarning ( 10, routine, */
	        logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
	        "%s (%s) - NULL file name", warnStr, routine );
		return newlist;
	}
	if ( !*file ) {
		/*AVCP PrintWarning ( 10, routine,*/
	        logMessageWithArgsAndExitOnError( DEBUG_LEVEL, 
	        "%s (%s) - Empty file name", warnStr, routine );
		return newlist;
	}

	StringListLength ( paths, &npaths );

	if ( npaths == 0 ) {
		return newlist;
	}

	for ( i = 0; i < npaths; i++ ) {
		/*
		** Add each string to the list...
		*/
		sprintf ( fullfile, "%s%c%s", paths[i], DIRSEP_CHAR, file );
		newlist = AddToStringList ( newlist, fullfile, listlen );
		if ( !newlist ) {
			break;
		}
	}
	return newlist;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_utils/RCS/GetFilesFromPathList.c,v $";
 static char rcs_id2[] = "$Id: GetFilesFromPathList.c,v 1.1 1999/02/18 15:16:45 dws Exp $";}
/*  ===================================================  */

}
