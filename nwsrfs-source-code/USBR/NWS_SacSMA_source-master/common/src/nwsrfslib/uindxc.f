C MEMBER UINDXC
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHECK IF CHARACTERS ARE FOUND IN A CHARACTER STRING.
C
      SUBROUTINE UINDXC (STR1,LSTR1,STR2,LSTR2,CHKTYP,IRETRN)
C
C  INPUT VARIABLES -
C     STR1   - CHARACTER STRING TO BE CHECKED FOR STR2
C     LSTR1  - MAXIMUM NUMBER IF CHARACTERS IN VARIABLE CONTAINING
C              CHARACTER STRING TO BE CHECKED FOR STR2
C     STR2   - CHARACTER STRING TO BE CHECKED
C     LSTR2  - MAXIMUM NUMBER IF CHARACTERS IN VARIABLE CONTAINING
C              CHARACTER STRING TO BE CHECKED
C     CHKTYP - TYPE OF CHECK TO BE MADE
C              ONLY=STR1 CAN HAVE ONLY CHARACTERS IN STR2
C              SOME=STR1 CAN ONLY HAVE SOME OF CHARACTERS IN STR2
C
C  OUTPUT VARIABLES -
C     IRETRN   - RETURN CODE
C                -1=INVALID VALUE OF CHECK TYPE
C                 0=STR1 DOES NOT CONTAIN STR2 CHARACTERS
C                 1=STR1 CONTAINS STR2 CHARACTERS
C
C
      CHARACTER*1 STR1(LSTR1),STR2(LSTR2)
      CHARACTER*4 CHKTYP
C
      COMMON /UIOX/ LP,ICD,LPD,LPE,ICDPUN,LSYS
      COMMON /UCMDBX/ ICMPRU,ICMTRC,ICMDBG
      INCLUDE 'flogm'	
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/uindxc.f,v $
     . $',                                                             '
     .$Id: uindxc.f,v 1.1 1995/09/17 19:02:43 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (FEWSDEBUG.GE.6) THEN
CSR         CALL ULINE (ICMPRU,1)
         WRITE (MESSAGESTRING,50)
	 call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
         ENDIF
C
      IF (FEWSDEBUG.GE.6) THEN
CSR         CALL ULINE (ICMPRU,1)
         WRITE (MESSAGESTRING,60) LSTR1,(STR1(I),I=1,LSTR1)
	 call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
CSR         CALL ULINE (ICMPRU,1)
         WRITE (MESSAGESTRING,70) LSTR2,(STR2(I),I=1,LSTR2)
	 call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
CSR	CALL ULINE (ICMPRU,1)
         WRITE (MESSAGESTRING,*) 'CHKTYP=',CHKTYP
	 call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
         ENDIF
C
      IRETRN=0
C
      IF (CHKTYP.NE.'ONLY'.AND.CHKTYP.NE.'SOME') THEN
         IRETRN=-1
         GO TO 40
         ENDIF
C
C  CHECK IF STR2 FOUND IN STR1
      DO 20 I=1,LSTR1
         DO 10 J=1,LSTR2
            IF (FEWSDEBUG.GE.6) THEN
CSR               CALL ULINE (ICMPRU,1)
               WRITE (MESSAGESTRING,80) I,J,STR1(I),STR2(J)
	       call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
               ENDIF
            IF (CHKTYP.EQ.'SOME'.AND.STR1(I).EQ.STR2(J)) GO TO 30
            IF (CHKTYP.EQ.'ONLY'.AND.STR1(I).EQ.STR2(J)) GO TO 20
10          CONTINUE
            IF (CHKTYP.EQ.'ONLY') GO TO 40
20       CONTINUE
      IF (CHKTYP.EQ.'SOME') GO TO 40
C
30    IRETRN=1
C
40    IF (FEWSDEBUG.GE.6) THEN
CSR      CALL ULINE (ICMPRU,1)
         WRITE (MESSAGESTRING,90) IRETRN
	 call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' *** ENTER UINDXC')
60    FORMAT (' LSTR1=',I3,3X,'STR1=',100A1)
70    FORMAT (' LSTR2=',I3,3X,'STR2=',100A1)
80    FORMAT (' I=',I3,3X,'J=',I3,3X,
     *   'STR(I)=',A,3X,'STR2(J)=',A)
90    FORMAT (' *** EXIT UINDXC - IRETRN=',I3)
C
      END
