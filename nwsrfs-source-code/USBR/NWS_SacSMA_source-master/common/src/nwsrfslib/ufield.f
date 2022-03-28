C MODULE UFIELD
C-----------------------------------------------------------------------
C
C  ROUTINE TO GET NEXT FIELD FROM RECORDS FROM FILE.
C
      SUBROUTINE UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
C
      CHARACTER*4 STRNG(LSTRNG),ICARDS(1)
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
     .$Source: /fs/hseb/ob72/rfc/util/src/new_alt/RCS/ufield.f,v $
     . $',                                                             '
     .$Id: ufield.f,v 1.3 2001/06/13 08:29:14 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (FEWSDEBUG.GE.6) THEN
CSR         CALL ULINE (ICMPRU,1)
         WRITE (MESSAGESTRING,*) 'ENTER UFIELD'
	 call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
         ENDIF
C
      LMXCRD=9999999
      IREAD=1
C
      CALL UGTFLD (IREAD,LMXCRD,MAXCRD,ICARDS,
     *   NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
C
      IF (FEWSDEBUG.GE.6) THEN
         IF (LSTRNG.GT.0) THEN
            WRITE (MESSAGESTRING,*) 'STRNG=',(STRNG(I),I=1,LSTRNG)
         ELSE
            LSTRNG2=-LSTRNG/4
            WRITE (MESSAGESTRING,*) 'STRNG=',(STRNG(I),I=1,LSTRNG2)
         ENDIF
	 call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      ENDIF
C
      IF (FEWSDEBUG.GE.6) THEN
CSR         CALL ULINE (ICMPRU,1)
         WRITE (MESSAGESTRING,*) 'EXIT UFIELD - ISTAT=',ISTAT
	 call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      ENDIF
C
      RETURN
C
      END
