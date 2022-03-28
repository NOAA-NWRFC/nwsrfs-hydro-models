#ifndef _COMMONINCLUDE_H
#define _COMMONINCLUDE_H

#define DATESTRINGLENGTH 10

//defined in  utilities.c
// used in states.c and utilties.c - references static variable
char *getStateFileName();

//defined in utilities.c
//used in timeseries.c and utilities.c - references static variable
char *getTsFileName();

//defined in  utilities.c
//used in timeseries.c and utilities.c - references static variable
int getJulianDayForStartOfRun();

//defined in  utilities.c
//used in timeseries.c and utilities.c - references static variable
int getJulianHourForStartOfRun();

//defined in  utilities.c
//used in timeseries.c and utilities.c
char *trim(char *inputString, int maxLen); 
#endif
