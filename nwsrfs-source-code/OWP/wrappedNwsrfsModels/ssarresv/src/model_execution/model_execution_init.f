CC/*  routine model_execution_init.f
CC*   stores in mod values from mod files
CC*   input:   MODEL_TIME_STEP
CC*            MODFILE_PATH_AND_NAME
CC*/   
      SUBROUTINE MODEL_EXECUTION_INIT(MODEL_TIME_STEP,
     1                                MOD_FILE_SIZE, numMods)
!CP  1                               MODFILE_PATH_AND_NAME, numMods)
               
      INCLUDE 'flogm'
      COMMON/FCTIME/IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,NOW(5),
     .  LOCAL,NOUTZ,NOUTDS,NLSTZ,IDA,IHR,LDA,LHR,IDADAT
C FTIME common block contains total number of timesteps
C used to replace references to NDD*NTIM24 with NTIMESTEPS
C also contains NOBSTIMESTEPS; NUMTIMESTEPS IN OBS PERIOD
      COMMON/FTIME/NTIMESTEPS,NOBSTIMESTEPS     
C MOD151 is SSARRESV common block
C we will fill this using the contents of mods.txt
      PARAMETER (NROWS=907)
      COMMON/MOD151/RRC(NROWS,2)
      INTEGER STARTJULHRS,ENDJULHRS,TOTALHOURS,RUNENDTIMESTEPS,
     1        OBSTARTTIMESTEPS 
     
!      CHARACTER*128 MODFILE_PATH_AND_NAME
!      CHARACTER*128 MODFILENAME     

      CHARACTER*30 WHICH_RESERVOIR
      CHARACTER*2 UPSTREAM_KEYWORD /'US'/
      CHARACTER*2 DOWNSTREAM_KEYWORD /'DS'/
      CHARACTER*30 MODNAME /'SSARREG'/     
      INTEGER NUMRES, NUMREG, NUMDATES, NUMVALUES, NUMIDENTS  
      INTEGER MOD_FILE_SIZE
      REAL EFFECTIVEDATE  
CVR   REAL DATES(100)
CVR   REAL VALUES(100)
      REAL DATES(numMods)
      REAL VALUES(numMods)
      INTEGER IDENTS(numMods)

!      REAL REGOPTIONS(numMods)
!      REAL REGOPTIONS(100)
!      INTEGER IDENTS(100)
       
C     FILL IN SSARESV MODS IF ANY        

!CP BUG348 MODFILENAME=TRIM(MODFILE_PATH_AND_NAME)    
!CP BUG348 CALL parse_mods_file(MODFILENAME, MOD_FILE_SIZE, numMods)
            
      IF(MOD_FILE_SIZE.GT.0) THEN
c
C INITIALIZE ARRAYS
c
          DO I = 1, numMods
            DATES(i) = 0.0
            VALUES(i) = 0.0
            IDENTS(i) = 0
          ENDDO 
c
C temporarily force switch from obs to forecast here
c CP BUG348 
c	 IF (FEWSDEBUG.GE.1) THEN
c            WRITE(MESSAGESTRING,*)'Including mods from file =' 
c     1      ,MODFILENAME
c            call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
c            WRITE(MESSAGESTRING,*)'Mods file size is = '
c     1      ,MOD_FILE_SIZE
c            call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
c         ENDIF
C     IF JUST 'DS' NUMRES = 1, load in RRC(1,*)
C     IF 'US' and 'DS' NUMRES = 2, load 'US' in RRC(1,*) and 'DS' in RRC(2,*)	 
	 NUMRES=0
	 CALL is_upstream_or_both_or_neither(NUMRES)
	 
         IF (NUMRES.EQ.0)THEN
            WRITE(MESSAGESTRING,*)'MISSING "DS" or "US" KEYWORD, '//
     1     'SSARREG MOD WILL BE IGNORED' 
            call logfromfortran(WARNING_LEVEL, MESSAGESTRING)
	 ENDIF
	 
	 DO I= 1,NUMRES
            IF(I.EQ.1)THEN
	       IF(NUMRES.EQ.1)THEN
	          WHICH_RESERVOIR(1:2)=DOWNSTREAM_KEYWORD
	       ELSE
	          WHICH_RESERVOIR(1:2)=UPSTREAM_KEYWORD
	       ENDIF
	    ELSE
	       WHICH_RESERVOIR(1:2)=DOWNSTREAM_KEYWORD 
	    ENDIF
C     NEED TO KNOW HOW MANY REGULATION VALUES
C     QUERY USING MODNAME and WHICH_RESERVOIR
            NUMREG=0
	    call get_num_matches(NUMREG, 3, MODNAME, -1, WHICH_RESERVOIR)	  
C     INITIALIZE FIRST 7 positions of each reservoir
C     UNUSED SLOTS ARE SET TO 0.0
            RRC(1,I) = 7 + NUMREG*3
            RRC(2,I) = 0.0
C     WHAT IS EFFECTIVE DATE - AS JULIAN HOUR 
            EFFECTIVEDATE=0
	    call get_effective_date(EFFECTIVEDATE)
	    RRC(3,I) = EFFECTIVEDATE
            RRC(4,I) = 0.0
            RRC(6,I) = 0.0
            RRC(7,I) = 100.0
C     IN SETS OF 3, FILL REGULATION HRS, VALUE, and CODE STARRTING AT POSITION 8
C     QUERY FOR ARRAY OF DATES, VALUES, AND REGOPTIONS USING MODNAME and 
C     WHICH_RESERVOIR

            MODINDEX = 8

	    call get_num_dates_and_dates(NUMDATES, DATES, 2, MODNAME, 
     1	    WHICH_RESERVOIR)

     	    call get_num_values_and_values(NUMVALUES, VALUES, 3, MODNAME, 
     1	    -1, WHICH_RESERVOIR)

            call get_codes_from_ssarresv(NUMIDENTS, IDENTS, 2, -1,
     1	    WHICH_RESERVOIR)

	    DO J = 1, NUMREG
	       RRC(MODINDEX,I)=DATES(j)
	       RRC(MODINDEX+1,I)=VALUES(j)
	       RRC(MODINDEX+2,I)=IDENTS(j)
	       MODINDEX = MODINDEX +3
            ENDDO

	    IF (FEWSDEBUG.GE.1) THEN
	      WRITE(MESSAGESTRING, 2)(RRC(M,I),M=1,3),RRC(6,I),RRC(7,I)
                  call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 2               FORMAT(' RRC = ',1X,5(F8.1,1X))
  		  WRITE(MESSAGESTRING, 22)(RRC(M,I),M=8,modindex)
                  call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 22              FORMAT('       ',1X,15(F8.1,1X))
            ENDIF	   
         ENDDO
!CP BUG348         call close_mod_file()
      ENDIF 
C END FILLING RRC COMMON BLOCK

C START FILLING FTIME COMMON BLOCK
C     FOR CALCULATING NTIMESTEPS use c common function numberoftimesteps      
      call numberoftimesteps(MODEL_TIME_STEP,NTIMESTEPS)
C     FOR CALCULATING NOBSTIMESTEPS
      RUNENDTIMESTEPS = LDA *24 +LHR
      OBSTARTTIMESTEPS = LDACPD *24 + LHRCPD
      NOBSTIMESTEPS = NTIMESTEPS + (OBSTARTTIMESTEPS-RUNENDTIMESTEPS)
C END FILLING FTIME COMMON BLOCK
      IF (FEWSDEBUG.GE.1) THEN
	 WRITE(MESSAGESTRING, 222) NTIMESTEPS
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 222     FORMAT('Number of time Steps = ',I3/)
         WRITE(MESSAGESTRING, 333) NOBSTIMESTEPS
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 333     FORMAT('Number of observation time Steps = ',I3/)
      END IF

      RETURN
      END
