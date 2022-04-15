      SUBROUTINE mod24( PO, MOD_FILE_SIZE, numMod )

      INCLUDE 'flogm'
      INCLUDE 'fctime'  
C
C AEICQN mod 'common/mod135'
C
      COMMON/MOD135/NDT35,IDT35(5),VAL35(5)              
C
C APICBASF and APICCO mods 'common/fapico'
C
      COMMON/FAPICO/NAPICS,APICOS(12,10)
C   
C FTIME common block contains total number of timesteps
C used to replace references to NDD*NTIM24 with NTIMESTEPS
C
      INTEGER MOD_FILE_SIZE, NUMMATCH   
      INTEGER STARTDATE, numMod
 
      INTEGER IDCO(8), DTCO(10)  
      INTEGER IDATES1(10), IDATES2(numMod)
      REAL*4  DATES1(10), VALUES1(10)
      REAL*4  DATES2(numMod), VALUES2(numMod)
   
      REAL  EFFECTIVEDATE, PVALUE, VALSCO(7,10)
      
      DIMENSION PO(1) 
!CP      CHARACTER*256 MODFILE_PATH_AND_NAME, MODFILENAME
      CHARACTER*8   NOKEYWD 
      CHARACTER     MNAME(4)*9, WHICH_APICOS(7)*5
      CHARACTER*4   MODTZC
      
      DATA MODTZC/'   Z'/
C      
C Initialize common/mod135
C
      NDT35 = 0
      DO I=1,5
        IDT35(I)=0
        VAL35(I)=0.0
      END DO
C
C Initialize mod variables
C
      DO I = 1, numMod
         IDATES2(I) = 0
         DATES2(I)  = 0.0
         VALUES2(I) = 0.0
      END DO
C
C Initialize common/FAPICO/
C
      NAPICS = 0
      IHZERO = 0
      IDT = PO(7)
C 
C First quadrant option: 1 = use AEI
C
      IVOPT = PO(14) 

C FOR CALCULATING NTIMESTEPS use c common function numberoftimesteps      
!      call numberoftimesteps(modelTimeStep, numMod)
C
C Fill in AEICQN, AIADJ, APICBASF and APICCO mods if any
C
!CP      MODFILENAME=TRIM(MODFILE_PATH_AND_NAME) 
!CP      CALL parse_mods_file(MODFILENAME, MOD_FILE_SIZE, numMod)

      IF ( MOD_FILE_SIZE.GT.0) THEN 
         call get_effective_date(EFFECTIVEDATE)
         STARTDATE = INT(EFFECTIVEDATE)
c
c Initialize mod string name
c
         MNAME(1)(1:6) = 'AEICQN'
         MNAME(2)(1:5) = 'AIADJ'
         MNAME(3)(1:8) = 'APICBASF'
         MNAME(4)(1:6) = 'APICCO'
         NOKEYWD(1:7)  = 'UNKNOWN'
c
c        Get information reading from mods.txt file and fill in  
c        values and dates array for AEICQN, AIADJ, APICBAFS and
c        APICCO mods
c
C         WRITE(*, *) 'get info'
         DO K=1,3  

            call get_dates_values_apicont(NUMMATCH, DATES1, VALUES1, 
     >                                   IDCO, 2, MNAME(K), NOKEYWD)
          IF ( NUMMATCH .GT. 0 ) THEN

            DO I=1,NUMMATCH
              IDATES1(I) = INT(DATES1(I))
            END DO
c
c           Performs AEICQN mod
c
            IF ( MNAME(K)(1:6).EQ.'AEICQN' ) THEN 
               CALL MAEICQ(IDATES1, NUMMATCH, VALUES1, IVOPT,
     .                     IHZERO, MODTZC) 
c
C            WRITE(*, *) 'done MAEICQ'
c           Performs AIADJ mod
c
            ELSEIF ( MNAME(K)(1:5).EQ.'AIADJ' ) THEN
               CALL MAIADJ(VALUES1(1), IDATES1(1), PVALUE, IHZERO, 
     .                     MODTZC)
               PO(25) = PVALUE
c
c           Perform APICBAFS mod
c
            ELSEIF ( MNAME(K)(1:8).EQ.'APICBASF' ) THEN
C               WRITE(*, *) 'ready for MAPICB'
               CALL MAPICB(IDATES1, NUMMATCH, VALUES1, IHZERO, MODTZC)
C               WRITE(*, *) 'done for MAPICB'
            END IF
          END IF
         END DO
c
c        Performs APICCO mod
c
         WHICH_APICOS(1)(1:3)='API'
         WHICH_APICOS(2)(1:3)='SMI'
         WHICH_APICOS(3)(1:4)='SMID'
         WHICH_APICOS(4)(1:4)='BFSC'
         WHICH_APICOS(5)(1:3)='BFI'
         WHICH_APICOS(6)(1:2)='FI'
         WHICH_APICOS(7)(1:3)='FEI'
c
         call get_dates_values_apicont(NUMMATCH, DATES2, VALUES2, IDCO,
     >        8, MNAME(4), WHICH_APICOS(1), WHICH_APICOS(2),
     >        WHICH_APICOS(3), WHICH_APICOS(4), WHICH_APICOS(5),
     >        WHICH_APICOS(6), WHICH_APICOS(7) )
c
        IF ( NUMMATCH .GT. 0 ) THEN
         DO I=1,NUMMATCH
            IDATES2(I) = INT(DATES2(I))  
         END DO
c
C         WRITE(*, *) 'check APICCO'
         IF ( MNAME(4)(1:6).EQ.'APICCO' ) THEN
            CALL MAPICC(IDATES2,NUMMATCH,VALUES2,IDCO,IHZERO,MODTZC)
         END IF
        END IF
c
c        Move values in the FAPICO common block to the FCOAPI common block
c
         CALL FAPITR(PO, IDT) 
c
!CP         call close_mod_file()
      END IF

      RETURN
      END
