C MODULE UFPACK
C-----------------------------------------------------------------------
C
C  ROUTINE TO PACK A1 CHARACTERS INTO A4 CHARACTERS.
C
      SUBROUTINE UFPACK (NPACK,PACKED,ISTRT,IBEG,IEND,ISTAT)
C
C  INPUT VARIABLES
C     NPACK  - NUMBER OF WORDS IN VARIABLE PACKED
C              >0=LENGTH IS NUMBER OF 4-CHARACTER WORDS
C              <0=LENGTH IS NUMBER OF 1-CHARACTER WORDS
C     ISTRT  - STARTING LOCATION OF CHARACTER STRING IN BUFFER
C     IBEG   - LOCATION OF FIRST CHARACTER TO BE CONVERTED
C     IEND   - LOCATION OF LAST  CHARACTER TO BE CONVERTED
C
C  OUTPUT VARIABLES
C     PACKED - PACKED CHARACTERS (INITIALIZED TO BLANK)
C     ISTAT  - STATUS CODE
C              1 = NOT ENOUGH ROOM IN PACKED
C              2 = NPACK IS ZERO
C
      CHARACTER*4 PACKED(1)
C
C  DEBUG OUTPUT UNIT AND TRACE AND DEBUG LEVEL INDICATORS COMMON BLOCK
      COMMON /UDEBUG/ IOGDB,IHCLTR,IHCLDB,IPRTR,IPRDB,IPDTR,IPDDB,IPPTR,
     *                IPPDB,IUTLTR,IUTLDB,IDETR,IDEDB,IDBDUM(4)
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
     .$Source: /fs/hseb/ob72/rfc/util/src/new_alt/RCS/ufpack.f,v $
     . $',                                                             '
     .$Id: ufpack.f,v 1.3 1999/07/06 13:00:53 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (FEWSDEBUG.GE.6) THEN
          WRITE (MESSAGESTRING,*) 'ENTER UFPACK'
	  call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      ENDIF
C      IF (IUTLTR.GT.1) WRITE (IOGDB,*) 'ENTER UFPACK'
C
      ISTAT=0
C
      IF (NPACK.EQ.0) THEN
         ISTAT=2 
         GO TO 20
         ENDIF
C
      IF (NPACK.GT.0) MPACK=NPACK
      IF (NPACK.LT.0) MPACK=-NPACK/4
C
      CALL UMEMST ('    ',PACKED,MPACK)
C
C  SET NUMBER OF WORDS
      LENGTH=IEND-IBEG+1
      NWORDS=(LENGTH+3)/4
C
C      IF (IUTLDB.GT.1) THEN
C         WRITE (IOGDB,*) 'LENGTH=',LENGTH,' NWORDS=',NWORDS,
C     *      ' MPACK=',MPACK
C         ENDIF
C
      IF (FEWSDEBUG.GE.6) THEN
          WRITE (MESSAGESTRING,*) 'LENGTH=',LENGTH,' NWORDS=',NWORDS,
     *      ' MPACK=',MPACK
	 call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      ENDIF
C  CHECK IF MAXIMUM NUMBER OF WORDS EXCEEDED
      IF (NWORDS.GT.MPACK) THEN
         ISTAT=1 
         GO TO 20
         ENDIF
C
C  MOVE CHARACTERS TO PACKED ARRAY
      NBEG=IBEG-1
      DO 10 I=1,LENGTH
         IPOS=ISTRT+NBEG+I-1
         CALL UMOVEX (ICDBUF,IPOS,PACKED,I,1)
10       CONTINUE
C
20     IF (FEWSDEBUG.GE.6) THEN
          WRITE (MESSAGESTRING,*) 'EXIT UFPACK'
	  call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      ENDIF
C20    IF (IUTLTR.GT.1) WRITE (IOGDB,*) 'EXIT UFPACK'
C
      RETURN
C
      END
