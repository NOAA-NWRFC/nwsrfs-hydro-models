/*

    unpackgrib.h
    
    function prototypes for unpackgrid_.c file
    
*/


#ifndef UNPACKGRIB_H
#define UNPACKGRIB_H

typedef struct {
  int total_len,pds_len,pds_ext_len,gds_len,bds_len;
  int ed_num,table_ver,center_id,gen_proc,grid_type,param,level_type,lvl1,
    lvl2,fcst_units,p1,p2,t_range,navg,nmiss,sub_center_id,bds_flag,pack_width;
  int gds_included,bms_included;
  int yr,mo,dy,time;
  int offset;  /* offset in bytes to next GRIB section */
  int D;
  int data_rep,nx,ny,rescomp,scan_mode,proj;
  double slat,slon,elat,elon,lainc,loinc,olon;
  int xlen,ylen;
  unsigned char *buffer,*pds_ext;
  double ref_val,**gridpoints;
  int ngy;
} GRIBRecord;

void getBits(unsigned char *buf,int *loc,size_t off,size_t bits);
double ibm2real(unsigned char *buf,size_t off);
int unpackIS(FILE *fp,GRIBRecord *grib_rec);
void unpackPDS(GRIBRecord *grib_rec);
void unpackGDS(GRIBRecord *grib_rec);
void unpackBDS(GRIBRecord *grib_rec);
int unpackgrib(FILE *fp,GRIBRecord *grib_rec);


#endif /* #ifndef UNPACKGRIB_H */
