#define closeModFile() close_mod_file_()
typedef struct
{
   char *modName;
   char ** identifiers;
   float date;
   float value;
   int numIdentifiers;
   float effectiveDate;
}MODS;

void get_effective_date_(float *effDate);
int getNumIdentifiers();
void close_mod_file();
void get_num_dates_and_dates_(int *numDates, float dates[], int *n_args, ...);
void is_upstream_or_both_or_neither_(int *idType);
void getModCount(int *cnt);
void get_num_matches_(int *numMatches, int *n_args, ...);
void get_num_values_and_values_(int *numValues, float values[], int *n_args, ...);
void get_codes_from_ssarresv_(int *numIdents, int idents[], int *n_args, ...);
int getCharAtEnd(char *str);
int isModFileEmpty(char *fileName);
void get_dates_values_apicont_(int *numMatch, float dates[], float values[],
                              int idco[], int *n_args, ...);

int parseModsFile(char *fileName, int *modFileSize);
void freeMods();  
