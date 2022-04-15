/* ----------------------------------------------------------------------------
** HMFileReadable - check to see whether a file is readable
** ----------------------------------------------------------------------------
** version:	1.1
** depends on:	<stdio.h>
** returns:	1 if file is readable, 0 if not
** ----------------------------------------------------------------------------
** history:
**
** 1.0 (?)		Steven A. Malers	Created function.
** 1.1 (9-11-92)	SAM, RTi		Standardized header.
** ----------------------------------------------------------------------------
** notes:	none
** ----------------------------------------------------------------------------
** variables:
**
** filename	.... name of file that is to be checked
** fp		.... pointer to the file
** ----------------------------------------------------------------------------
*/
#include "ResJ.h"
#include "resj_main.h"
int FileReadable ( char *filename )
{	FILE	*fp;

	if ( (fp = fopen(filename, "r") ) == NULL )  return 0;
	else {	fclose ( fp );
		return 1;
	}

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source:/fs/hseb/ob81/ohd/ofs/src/resj_utils/RCS/FileReadable.c,v $";
 static char rcs_id2[] = "$Id: FileReadable.c,v 1.2 2004/09/08 17:14:23 hank Exp $";}
/*  ===================================================  */

}
