CC/*  routine model_setup_init.f
CC*   sets path for ssarresv to open params.txt file used by pin51
CC*   input:   path
CC/      
      SUBROUTINE MODEL_SETUP_INIT(PATH)
      
      COMMON /UFREEX/ NFIELD,
     *   IFTYPE(50),IFCNT(50),IFSTRT(50),IFSTOP(50),
     *   ICDBUF,ICDSTR,ICDSTP,NRDCRD,IPRBLN,IPRCRD,MAXFLD,IPRMPT,
     *   NPUCRD,PFCOMA,IPRTCD,ICKDAT,ICDTMP,IOPREP,ICDSPC,
     *   ICDQTE,NFLDNQ,CMTCHR
      LOGICAL PFCOMA
      CHARACTER*1 CMTCHR
      CHARACTER*100 ICDBUF
      
      COMMON /UIOX/ LP,ICD,LPD,LPE,ICDPUN,LSYS
      
      DATA ICD/5/ 
      DATA CMTCHR/'$'/
      INCLUDE 'flogm'  
      INTEGER MUNI51         
      CHARACTER*200 PATH
      CHARACTER*200 PATHANDFILENAME     
       
C open a file with operation definition
C pin assumes operation definition in unit ICD

      PATHANDFILENAME=TRIM(PATH)
      
      OPEN(ICD,ERR=100,ACCESS='SEQUENTIAL',FORM='FORMATTED',
     1     FILE=PATHANDFILENAME)
      MUNI51=89      
      OPEN(MUNI51,ERR=100,FORM='FORMATTED',STATUS='SCRATCH')  
      RETURN      
100   WRITE(MESSAGESTRING,120)PATHANDFILENAME
      call logfromfortran( FATAL_LEVEL, MESSAGESTRING )
120   FORMAT('*** ERROR *** SSARRESV: Can not open file:',A200)

      END
