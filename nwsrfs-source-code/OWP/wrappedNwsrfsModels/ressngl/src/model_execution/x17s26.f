C MEMBER X17S26
C  (from old member FCX17S26)
C
      SUBROUTINE X17S26(WD,LOCWD,QMD,ICONV,ISTART,IEND,NTIMD,NQMD,ITYPE)
C-------------------------------------------------------------
C  SUBROUTINE TO COMPUTE DAILY MEANS FROM PERIOD MEANS. IF ONE
C  PERIOD MEAN IS MISSING THEN THE DAILY MEAN WILL BE SET TO
C  MISSING.
C-------------------------------------------------------------
C   ARGUMENT LIST
C
C     WD     - ARRAY TO PULL PERIOD MEANS FROM. TYPICALLY IS
C              THE W(WORK) OR D(TIME-SERIES DATA) ARRAY IN
C              CALLING ROUTINE, HENCE THE NAME.
C     LOCWD  - LOCATION OF START OF INFO IN WD ARRAY
C     QMD    - ARRAY TO HOLD DAILY MEANS.
C     ICONV  - CONVERSION FACTOR (DAILY MEANS NEED TO BE IN UNITS
C              OCMSD.
C     ISTART - START OF SUMMATION (MUST BE FIRST PERIOD OF DAY)
C     IEND   - END OF SUMMATION PERIOD.
C     NTIMD  - NUMBER OF PERIOD WITHIN DAY.
C     NQMD   - NUMBER OF MEAN DAILY VALUES COMPUTED. (OUTPUT)
C     ITYPE  - TYPE OF TIME-SERIES TO CHECK FOR IGNORING IN THE
C              'IGNORETS' MOD.
C-----------------------------------------------------------------
C  ORIGINALLY PROGRAMMED BY - JOE OSTROWSKI - HRL - SEPT 1984
C-----------------------------------------------------------------
C
      INCLUDE 'common/resv26'
      INCLUDE 'common/exg26'
      INCLUDE 'fctime'
!CP   INCLUDE 'common/fdbug'
      INCLUDE 'flogm'
C
      DIMENSION WD(1),QMD(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/x17s26.f,v $
     . $',                                                             '
     .$Id: x17s26.f,v 1.3 1996/07/12 14:05:29 dws Exp $
     . $' /
C    ===================================================================
C
C
C---------------
C  PRINT TRACE INFO HERE IF REQUESTED
C
!CP   IF (IBUG .GE. 1) WRITE(IODBUG,600)
!CP   600 FORMAT('   *** ENTER X17S26 ***')
      IF ( FEWSDEBUG.GE.4 ) THEN
         WRITE(MESSAGESTRING, '(A23)') '   *** ENTER X17S26 ***'
	 call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
      END IF
C
C INITIALIZE VALUES HERE
C
      QSUM = 0.0
      MISS = 0
      NQMD = 0
      ISUTYP = 2
      NDTL = IEND/NTIMD
C
C  DO SUMMATIONS HERE
C
      DO 100 I=ISTART,IEND
C
C  IF A MISSING VALUE ALREADY FOUND WITHIN A DAY, NO NEED TO ADD
C  ANY MORE VALUES TO THE SUM
C
      IF (MISS .NE. 0) GO TO 50
C
C  CHECK TO SEE IF PERIOD MEAN VALUE IS MISSING. ALSO,
C  CHECK IF 'IGNORETS' MOD IS IN EFFECT. TREAT VALUE AS MISSING IF SO
C
      JULI = I*MINODT + (IDA-1)*24
      IF (IFMSNG(WD(LOCWD+I-1)) .EQ. 0 .AND.
     .    IFGNOR(ITYPE,JULI,ISUTYP) .EQ. 0) GO TO 30
C
C  VALUE IS MISSING, INDICATE THAT DAILY VALUE SHOULD BE SET TO MISSING
C
      IF(NTIMD.LE.1) THEN
        QMD(I)=WD(LOCWD+I-1)
        GO TO 100
      ENDIF
      QSUM = -999.0
      MISS = 1
      IF (MOD(I,NTIMD) .NE. 0) GO TO 100
C
C  AT END OF DAY, SO CONVERT AND STORE DAILY VALUES
C
      NQX = I/NTIMD
      QMD(NQX)=-999.
C
C  RESET VALUES FOR NEXT DAILY SUMMATION
C
      QSUM = 0.00
      MISS = 0
      GO TO 100
C
C  VALUE IS FOUND. ADD IT TO DAILY SUM.
C
   30 CONTINUE
      IF(NTIMD.LE.1) THEN
        QMD(I)=WD(LOCWD+I-1)
        NQMD=I
        GO TO 100
      ENDIF
      QSUM = QSUM + WD(LOCWD+I-1)
C
C  CHECK TO SEE IF WE'RE AT END OF DAY TO STORE DAILY VALUE
C
   50 CONTINUE
      IF (MOD(I,NTIMD) .NE. 0) GO TO 100
C
C  AT END OF DAY, SO CONVERT AND STORE DAILY VALUES
C
      IF(MISS.EQ.0) THEN
        NQMD = I/NTIMD
        QMD(NQMD) = QSUM/ICONV
      ELSE
        NQX = I/NTIMD
        QMD(NQX)=-999.
      ENDIF
C
C  RESET VALUES FOR NEXT DAILY SUMMATION
C
      QSUM = 0.00
      MISS = 0
C
  100 CONTINUE
C
C  PRINT DAILY VALUES IF REQUESTED
C
!CP      IF (IBUG .LT. 2) GO TO 690
!CP      WRITE(IODBUG,630)
! 620    FORMAT(1X,8F10.0)
      IF ( FEWSDEBUG.GE.4 ) THEN
         WRITE(MESSAGESTRING, 630)
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
  630    FORMAT(/10X,'** MEAN DISCHARGE VALUES **'/)
   
!CP      WRITE(IODBUG,620) (WD(LOCWD+I-1),I=ISTART,IEND)
         NPRINT=IEND
         IF ( NPRINT.GT.100 ) NPRINT=100
         WRITE(FORMATSTR, *) '%10.0f'
         call logonedimensionarrayfromfortran(DEBUG_LEVEL,NULLLINE,1,8,
     >        FORMATSTR,LOCWD+ISTART-1,LOCWD+NPRINT-1,1,WD)
         

!CP      WRITE(IODBUG,610)
         WRITE(MESSAGESTRING, 610)
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
  610    FORMAT(/10X,'** MEAN DAILY DISCHARGE VALUES **'/)
   
!CP      WRITE(IODBUG,620) (QMD(I),I=1,NDTL)
         NPRINT=NDTL
         IF ( NPRINT.GT.100 ) NPRINT=100
         WRITE(FORMATSTR, *) '%10.0f'
         call logonedimensionarrayfromfortran(DEBUG_LEVEL,NULLLINE,1,8,
     >        FORMATSTR,1,NPRINT,1,QMD)
         
      END IF
C
!CP 690 CONTINUE
!CP   IF (IBUG .GE. 1) WRITE(IODBUG,699)
!CP   699 FORMAT('    *** EXIT X17S26 ***')
      IF ( FEWSDEBUG.GE.4 ) THEN
         WRITE(MESSAGESTRING, '(A23)') '    *** EXIT X17S26 ***'
	 call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
      END IF
      RETURN
      END
