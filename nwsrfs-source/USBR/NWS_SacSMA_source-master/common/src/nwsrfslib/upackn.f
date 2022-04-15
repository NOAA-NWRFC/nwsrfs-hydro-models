C MODULE UPACKN
C-----------------------------------------------------------------------
C
C  ROUTINE UPACKN PACKS A1 CHARACTERS INTO A4 CHARACTERS.
C
      SUBROUTINE UPACKN (LEN,UNPKED,NCHAR,PACKED,ISTAT)
C
      CHARACTER*1 PACKED(1),UNPKED(1)
C
C  DEBUG OUTPUT UNIT AND TRACE AND DEBUG LEVEL INDICATORS COMMON BLOCK
      COMMON /UDEBUG/ IOGDB,IHCLTR,IHCLDB,IPRTR,IPRDB,IPDTR,IPDDB,IPPTR,
     *                IPPDB,IUTLTR,IUTLDB,IDETR,IDEDB,IDBDUM(4)
C
C add include of flogm which contains FEWS logging variables 
      INCLUDE 'flogm'
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/new_alt/RCS/upackn.f,v $
     . $',                                                             '
     .$Id: upackn.f,v 1.2 1998/07/02 19:45:36 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (FEWSDEBUG.GE.6) THEN
          WRITE (MESSAGESTRING,10)
	 call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      ENDIF
10    FORMAT (' *** ENTER UPACKN')
C
      ISTAT=0
C
      LENGTH=LEN
C
      IF (LEN.LT.1) GO TO 50
C
      NBYTES=NCHAR*4
C      
      IF (NBYTES.GE.LEN) GO TO 20
         ISTAT=1
         LENGTH=NBYTES
C         
20    DO 30 I=1,LENGTH
         PACKED(I)=UNPKED(1+(I-1)*4)
30       CONTINUE
C
      NLEFT=MOD(LENGTH,4)
      IF (NLEFT.EQ.0) GO TO 50
C
C  MUST FILL REST OF LAST FULL WORD OF PACKED WITH BLANKS
      NFILL=4-NLEFT
      DO 40 I=1,NFILL
         PACKED(LENGTH+I)=' '
40       CONTINUE
C
50    IF (FEWSDEBUG.GE.6) THEN
          WRITE (MESSAGESTRING,60) (PACKED(I),I=1,LENGTH)
	 call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      ENDIF
60    FORMAT (' PACKED=',100A1)
C
C      IF (IUTLTR.GT.1) WRITE (IOGDB,70) ISTAT
      IF (FEWSDEBUG.GE.6) THEN
          WRITE (MESSAGESTRING,70) ISTAT
	 call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      ENDIF
70    FORMAT (' *** EXIT UPACKN : ISTAT=',I2)
C
      RETURN
C
      END
