#ifndef _TIMESERIES_H_
#define _TIMESERIES_H_

#define MISSING_VALUE -999.

typedef struct {
	char id[9];	
	char type[5];	
	int timeStep;
	int count;
	float *value;
	int* dateTime;
	char dimensions[20];
	char units[10];
} TimeSeries;

TimeSeries *getOneTimeSeries(char *basinId, char *tsType, int timeStep, int *count);

TimeSeries * readAllInputTimeSeries();

void  writeOneTimeSeries(FILE *outputTsFilePtr, char * tsId, char * tsType, int timeStep, float * qs, int count);

extern void mdyh2_(int*, int*, int*, int*, int*, int*, int*, int*, char[]);

void freeTimeSeries();

#endif
