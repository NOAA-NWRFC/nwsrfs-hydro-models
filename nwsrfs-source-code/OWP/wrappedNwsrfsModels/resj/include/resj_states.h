/******************************************************************************
   Filename: resj_states.h

   Description:
   ============
   Header file for read and write states variables to output and input file.
   
   

   Change History
   ==============
   DATE          VERSION    PROGRAMMERS        NOTES
   ----------    -------    -----------------  ----------------------
   08/09/08      1          Ai Vo / Cham Pham  Initial implementation

******************************************************************************/
#ifndef RESJ_STATES_
#define RESJ_STATES_

#include <strings.h>

#define MAX_COMP_METHOD_ELEMENT        10
#define MAX_RESERVOIR_ELEMENT          8 
#define MAX_NODE_ELEMENT               6 
#define MAX_CALINFLOW_ELEMENT          5 
#define MAX_LOOKUP3_ELEMENT            5
#define MAX_SETRELELEV_ELEMENT         2
#define MAX_SETWITHDRAW_ELEMENT        3
#define MAX_LAGK_ELEMENT               4  
#define TRUE                           1  
#define FALSE                          0 
#define FIFTYFOUR                      54
#define THIRTY                         30
/* 
   global variables that are set in readStates routine and being used by
   writeStates routine 
*/
extern char *unitStr;

/* 
   global variable that are being used by  readStates routine and 
   writeStates routine 
*/

static char ComponentMethodKey[][80] = {"RESERVOIR", "NODE", "LAGK",
                                        "LOOKUP3", "SETRELEASE", "SETWITHDRAW",
                                        "SETELEVATION", "ADJUST", "SPILLWAY",
                                        "CALCINFLOW"};

static char reservoirKey[][80] = {"INITIALRELEASE", "INITIALPOOL",
                                  "INITIALWITHDRAW", "INITIALINFLOW",
                                  "PREVIOUSRELEASE", "PREVIOUSPOOL",
                                  "PREVIOUSWITHDRAW", "PREVIOUSINFLOW" };

static char nodeKey[][80] = {"INITIALDISCHARGE", "PREVIOUSDISCHARGE",
                             "INITIALINFLOW", "PREVIOUSINFLOW",
                             "INITIALDIVERSION", "PREVIOUSDIVERSION"};

static char lagkKey[][80] = {"COINFLOW", "INITIALOUTFLOW", "INITIALSTORAGE",
                             "INITIALLAGGEDINFLOW"};

static char lookup3Key[][80] = {"BLENDTS","BLENDTBL","COLINDEX","ROWINDEX",
                                "INITIALTRANSFER"};

static char setRelElvWidKey[][80] = {"BLENDTS","BLENDTBL", "INITIALTRANSFER"};

static char calcInflowKey[][80] = {"REMAININGVOL", "STARTINFLOW", "STARTPOOL",
                                   "STARTRELEASE", "STARTWITHDRAW"};

/* routines from resj_utils directory */

void resj_readStates( float POarray[], float COarray[] );
void resj_writeStates( float POarray[], float COarray[] ,char* statesFileName);
void removeTrailingSpaces( char string[] );

#endif
