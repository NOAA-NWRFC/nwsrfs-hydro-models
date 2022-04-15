#ifndef _STATES_H
#define _STATES_H

#define NON_EXISTENT_STATE -999
#define STATE_KEY_SIZE 50

void readStatesFromFile(int *nStates, int *pUnit);

/* populate/read states */
int *populateArrayOfIntStateValues(char *key, int numElementsInArray, float *cArray, int index);

void populateArrayOfFloatStateValues(char *key, int numElementsInArray, float *cArray, int index);

char **populateArrayOfStringStateValues(char *key, int numElementsInArray, float *cArray, int index);

int populateIntStateValue(char *key, float *cArray, int index);

char * populateStringStateValue(char *key, float *cArray, int index, int length);

float populateFloatStateValue(char *key, float *cArray, int index);

void populateArrayOfFloatStateValuesPair(char *key1,char *key2, 
                       int numElementsInArray, float *cArray, int index);

/* write states */
void writeIntStateToFile(FILE *filePtr, char *key, float *cArray, int index);

void writeFloatStateToFile(FILE *filePtr, char *key, float *cArray, int index);

void writeStringStateToFile(FILE *filePtr, char *key, char* str);

void writeArrayOfIntStatesToFile(FILE *filePtr, char *key, float *cArray, int index, int arrayLength);

void writeArrayOfFloatStatesToFile(FILE *filePtr, char *key, float *cArray, int index, int arrayLength);

void writeArrayOfFloatStatesPairsToFile(FILE *filePtr, char *key1, char *key2, float *cArray, int index, int arrayLength);

void freeStates();

#endif
