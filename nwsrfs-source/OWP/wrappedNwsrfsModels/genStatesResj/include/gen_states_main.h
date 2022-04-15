#ifndef _GEN_STATES_MAIN_
#define _GEN_STATES_MAIN_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <stdarg.h>
#include <math.h>
#include <ctype.h>
#include <malloc.h>

#include "fortranCommonIncludes.h"
#include "logging.h"
#include "timeseries.h"
#include "utilities.h"
#include "states.h"
#include "resj_states.h"
#include "resj_main.h"

void write_states( float POarray[], float COarray[], char* outputStatesFileName );
#endif
