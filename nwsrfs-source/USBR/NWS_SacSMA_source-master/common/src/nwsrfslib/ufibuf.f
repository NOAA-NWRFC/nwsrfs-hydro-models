C MODULE UFIBUF
C-----------------------------------------------------------------------
C
      SUBROUTINE UFIBUF (ICHR,ISTRT,IEND,ICOL)
C
C  THIS ROUTINE SEARCHES FOR CHARACTER STRING FROM COLUMNS ISTRT TO IEND
C  OF ARRAY ICDBUF.
C
C  IF FOUND, ICOL=COLUMN NUMBER, ELSE ICOL=0
C
      CHARACTER*1 ICHR
C
      COMMON /UCMDBX/ ICMPRU,ICMTRC,ICMDBG
      COMMON /UFREEX/ NFIELD,
     *   IFTYPE(50),IFCNT(50),IFSTRT(50),IFSTOP(50),
     *   ICDBUF,ICDSTR,ICDSTP,NRDCRD,IPRBLN,IPRCRD,MAXFLD,IPRMPT,
     *   NPUCRD,PFCOMA,IPRTCD,ICKDAT,ICDTMP,IOPREP,ICDSPC,
     *   ICDQTE,NFLDNQ,CMTCHR
      INCLUDE 'flogm'
      LOGICAL PFCOMA
      CHARACTER*1 CMTCHR
      CHARACTER*100 ICDBUF
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/ufibuf.f,v $
     . $',                                                             '
     .$Id: ufibuf.f,v 1.2 1998/07/06 13:21:43 page Exp $
     . $' /
C    ===================================================================
C
C
      ICOL=0
C
      IF (IEND.LT.ISTRT) GO TO 30
C
      DO 10 I=ISTRT,IEND
         IF (FEWSDEBUG.GE.6) THEN
CSR            CALL ULINE (ICMPRU,1)
            WRITE (MESSAGESTRING,50) ICDBUF(I:I),ICHR
	    call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
            ENDIF
         IF (ICDBUF(I:I).EQ.ICHR) GO TO 20
10       CONTINUE
      GO TO 30
C
20    ICOL=I
C
30    IF (FEWSDEBUG.GE.6) THEN
CSR         CALL ULINE (ICMPRU,1)
         WRITE (MESSAGESTRING,40) ICHR,ISTRT,IEND,ICOL
	 call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT (' *** EXIT UFIBUF : ICHR=',A1,3X,'ISTRT=',I3,3X,
     *   'IEND=',I3,3X,'ICOL=',I3)
50    FORMAT (' ICDBUF(I:I)=',A1,3X,'ICHR=',A1)
C
      END
