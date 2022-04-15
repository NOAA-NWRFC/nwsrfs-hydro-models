/*****************************************************************************
  * Module Name: removeTrailingSpaces()
  *
  * Original Author:  Ai Vo / Cham Pham
  * 
  * Module Creation Date: 08/18/08
  * 
  * Description:
  *   Read states values from statesI.txt and fill read information
  *   into CO array
  *
  * Calling Arguments:
  *
  * Name          Input/Output    Type       Description
  * PO            In              float[]    contains parameter information
  * CO            out             float[]    CarryOver data
  ***************************************************************************/
#include <stdio.h>
#include <string.h>

void removeTrailingSpaces(char string[])
{
  char *token = NULL;
  
   /* remove trailing spaces  */
  for ( token = strtok(string," "); token != NULL; token = strtok(NULL," ") )
  {}

}
