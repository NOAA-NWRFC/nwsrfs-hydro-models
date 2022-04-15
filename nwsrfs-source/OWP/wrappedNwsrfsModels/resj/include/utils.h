#ifndef utils
#define utils

extern int get_apps_defaults(char *request, int *request_len, char *reply, int *reply_len);
extern int InitFILEs ( void );
extern int SortFQuick ( float *data, int ndata, int *sort_order, int sflag );
//int IsValidYear ( int year );
int getpid();

#endif
