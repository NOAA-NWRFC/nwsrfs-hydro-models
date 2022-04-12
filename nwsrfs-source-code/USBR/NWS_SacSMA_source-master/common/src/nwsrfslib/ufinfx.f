C MODULE UFINFX
C-----------------------------------------------------------------------
C
C  ROUTINE TO CONVERT A CHARACTER STRING TO A INTEGER VALUE.
C
      SUBROUTINE UFINFX (IANS,ISTRT,IBEG,IEND,ISTAT)
C
C  INPUT VARIABLES:
C     ISTRT  - STARTING LOCATION OF CHARACTER STRING IN ICDBUF
C     IBEG   - LOCATION OF FIRST CHARACTER TO BE CONVERTED
C     IEND   - LOCATION OF LAST  CHARACTER TO BE CONVERTED
C
C  OUTPUT VARIABLES:
C     IANS   - NUMERIC VALUE
C     ISTAT  - STATUS CODE
C              1 = INVALID CHARACTER FOUND
C
C  LINE PRINTER, CARD READER, DEBUG, ERROR UNIT AND CARD PUNCH
C  COMMON BLOCK
      COMMON /UIO/ LP,ICD,LPD,LPE,ICDPUN
C  DEBUG OUTPUT UNIT AND TRACE AND DEBUG LEVEL INDICATORS COMMON BLOCK
      COMMON /UDEBUG/ IOGDB,IHCLTR,IHCLDB,IPRTR,IPRDB,IPDTR,IPDDB,IPPTR,
     *                IPPDB,IUTLTR,IUTLDB,IDETR,IDEDB,IDBDUM(4)
C  FREE FORMAT INPUT COMMON BLOCK
      COMMON /UFREEX/ NFIELD,
     *   IFTYPE(50),IFCNT(50),IFSTRT(50),IFSTOP(50),
     *   ICDBUF,ICDSTR,ICDSTP,NRDCRD,IPRBLN,IPRCRD,MAXFLD,IPRMPT,
     *   NPUCRD,PFCOMA,IPRTCD,ICKDAT,ICDTMP,IOPREP,ICDSPC,
     *   ICDQTE,NFLDNQ,CMTCHR
      LOGICAL PFCOMA
      CHARACTER*1 CMTCHR
      CHARACTER*100 ICDBUF  
C
C add include of flogm which contains FEWS logging variables 
      INCLUDE 'flogm'
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/new_alt/RCS/ufinfx.f,v $
     . $',                                                             '
     .$Id: ufinfx.f,v 1.2 1998/07/02 19:41:56 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (FEWSDEBUG.GE.6) THEN
          WRITE (MESSAGESTRING,20)
          call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      ENDIF
C      IF (IUTLTR.GT.1) WRITE (IOGDB,20)
C
      ISTAT=0
C
C  CONVERT FROM CHARACTER TO INTEGER
      NCHAR=IEND-IBEG+1
      IPRERR=1
      CALL UFA2I (ICDBUF(ISTRT:ISTRT),IBEG,NCHAR,IANS,IPRERR,LP,IERR)
      IF (IERR.EQ.0) GO TO 10
C
      ISTAT=1
C
      IF (IUTLDB.GT.1) THEN
C         IF (IBEG.EQ.IEND) WRITE (IOGDB,30) IBEG
         IF (FEWSDEBUG.GE.6) THEN
             WRITE (MESSAGESTRING,30) IBEG
             call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
         ENDIF
C	 IF (IBEG.NE.IEND) WRITE (IOGDB,40) IBEG,IEND
         IF (FEWSDEBUG.GE.6) THEN
             WRITE (MESSAGESTRING,40) IBEG,IEND
             call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
         ENDIF
      ENDIF
C
10     IF (FEWSDEBUG.GE.6) THEN
             WRITE (MESSAGESTRING,50)
             call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
       ENDIF
C10    IF (IUTLTR.GT.1) WRITE (IOGDB,50)
C
      RETURN
C
20    FORMAT (' *** ENTER UFINFX')
30    FORMAT (' IN UFINFX - INTEGER VALUE EXPECTED IN COLUMN ',I2)
40    FORMAT (' IN UFINFX - INTEGER VALUE EXPECTED IN COLUMNS ',I2,
     *   '-',I2)
50    FORMAT (' *** EXIT UFINFX')
C
      END
