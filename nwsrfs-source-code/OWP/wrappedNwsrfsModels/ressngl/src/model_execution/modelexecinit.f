      SUBROUTINE modelexecinit(modelTimeStep, MOD_FILE_SIZE, 
     >                         VM126, numMods, iregTech)

      INCLUDE 'flogm'
      INCLUDE 'fctime'  
      INCLUDE 'errdat'
      INCLUDE 'fratng'
      INCLUDE 'common/mod126'
      INCLUDE 'common/espres'
C
C SETQMEAN mod 'common/mod126'
C
!      COMMON/MOD126/M126ON,MJ126,NVM126,VM126(50000)      
C FTIME common block contains total number of timesteps
C used to replace references to NDD*NTIM24 with NTIMESTEPS
      COMMON/FTIME/NTIMESTEPS,NOBSTIMESTEPS
      COMMON/FENGMT/METRIC
      DATA BLANK/4H    /
      INTEGER STARTJULHRS,ENDJULHRS,TOTALHOURS,RUNENDTIMESTEPS,
     1        OBSTARTTIMESTEPS
      INTEGER MOD_FILE_SIZE, NUMDATES, NUMVALUES   
!CP      CHARACTER*128 MODFILE_PATH_AND_NAME
!CP      CHARACTER*128 MODFILENAME
      INTEGER*4 VALIDDATE, STARTDATE
      REAL DATES(numMods), VALUES(numMods),EFFECTIVEDATE
      DIMENSION VM126(*) 
      CHARACTER*9 MODNAME /'SETQMEAN'/
      
C Initialize common/errdat
      NERRS = 0

C Initialize common/espres
      IREG = iregTech 
      
C Initialize common/fratng
      DO 260 I=1,5
      RIVERN(I)=BLANK
      RIVSTA(I)=BLANK
      FPTYPE(I)=BLANK
      RFCOMT(I)=BLANK
260   CONTINUE

      GZERO =-999.   !or -999. ????
      LOCH = 1     !use same info in fcinit_top/TEXT/fdefrc.f
      STGMIN=-999. !use same info in fcinit_top/TEXT/fdefrc.f
      LXTOPW=-999  !use same info in fcinit_top/TEXT/fdefrc.f     
      LXELEV=-999  !use same info in fcinit_top/TEXT/fdefrc.f
      ABELOW=0.0   !
      FLOODN=-999. !
      SLOPE=0.01   !
      LASDAY=0     !   
      IPOPT=1      !
      RFSTG=-999.  !
      RFQ=-999.    !
      IRFDAY=-999  !
      
      DO 256 I=1,25
256   EMPTY(I)=0.000001

      DO 270 I=1,225
270   XRC(I)=BLANK

C
C Initialize Mod variables
C
      DO I = 1, numMods
         DATES(I) = 0.0
         VALUES(I) = 0.0
      ENDDO

C Initialize common/mod126
      M126ON=.FALSE.    !fcst_mods/TEXT/mods.f
      MJ126 = 0 
      NVM126 = numMods !124
      DO I=1, NVM126 
        VM126(I) = 0.0
      END DO
C
C     FILL IN SETQMEAN MODS IF ANY
C
!CP      MODFILENAME=TRIM(MODFILE_PATH_AND_NAME) 
!CP      CALL parse_mods_file(MODFILENAME, MOD_FILE_SIZE, numMods)

      IF ( MOD_FILE_SIZE.GT.0) THEN 
         call get_effective_date(EFFECTIVEDATE)
         VALIDDATE = INT(EFFECTIVEDATE)
   
         call get_num_dates_and_dates(NUMDATES, DATES, 1, MODNAME)
         STARTDATE = INT(DATES(1))

         call get_num_values_and_values(NUMVALUES, VALUES, 1, MODNAME) 

         call MSETQM(STARTDATE, VALIDDATE, NUMVALUES, VALUES, 
     >               modelTimeStep, vm126)

!CP         call close_mod_file()

      END IF
C
C Initialize common/fengmt
C
      METRIC = 1 ! because statesI.txt store values in METRIC unit

C FOR CALCULATING NTIMESTEPS use c common function numberoftimesteps      
      call numberoftimesteps(modelTimeStep,NTIMESTEPS)

      IF ( FEWSDEBUG.GE.1 ) THEN          
         WRITE(MESSAGESTRING, 111) modelTimeStep
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 111     FORMAT('Model Time Step = ',I8/)
      ENDIF
   

C FOR CALCULATING NOBSTIMESTEPS
      RUNENDTIMESTEPS = LDA *24 +LHR
      OBSTARTTIMESTEPS = LDACPD *24 + LHRCPD
      NOBSTIMESTEPS = NTIMESTEPS + (OBSTARTTIMESTEPS-RUNENDTIMESTEPS)
      IF ( FEWSDEBUG.GE.1 ) THEN
         WRITE(MESSAGESTRING, 222) NTIMESTEPS
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 222     FORMAT('Ntime Steps = ',I8/)
         WRITE(MESSAGESTRING, 333) NOBSTIMESTEPS
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 333     FORMAT('NObstime Steps = ',I8/)
      END IF
      
      RETURN
      END
