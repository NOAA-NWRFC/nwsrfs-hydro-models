C MEMBER UREPET
C-----------------------------------------------------------------------
C
      SUBROUTINE UREPET (CHAR,STRING,NREPET)
C
C  ROUTINE UREPET FILLS A VARIABLE WITH A CHARACTER THE SPECIFIED
C  NUMBER OF TIMES.
C
C  INPUT ARGUMENTS
C     CHAR   - CHARACTER TO BE FILLED
C     STRING - CHARACTER STRING TO BE FILLED
C     NREPET - NUMBER OF TIMES CHAR TO BE REPEATED
C
      CHARACTER*1 CHAR,STRING(1)
C
      COMMON /UCMDBX/ ICMPRU,ICMTRC,ICMDBG
      INCLUDE 'flogm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/urepet.f,v $
     . $',                                                             '
     .$Id: urepet.f,v 1.1 1995/09/17 19:02:46 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (FEWSDEBUG.GE.6) THEN
CSR         CALL ULINE (ICMPRU,1)
         WRITE (MESSAGESTRING,50) CHAR,NREPET
	 call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
         ENDIF
C
      IF (NREPET.LE.0) GO TO 20
C
      DO 10 I=1,NREPET
         STRING(I)=CHAR
10       CONTINUE
C
20    IF (FEWSDEBUG.GE.6) THEN
CSR         CALL ULINE (ICMPRU,1)
         WRITE (MESSAGESTRING,60)
	 call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' *** ENTER UREPET - CHAR=',A1,3X,'NREPET=',I4)
60    FORMAT (' *** EXIT UREPET')
C
      END
