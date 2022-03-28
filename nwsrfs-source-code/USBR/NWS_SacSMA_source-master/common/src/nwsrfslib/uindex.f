C MEMBER UINDEX
C-----------------------------------------------------------------------
C
C  ROUTINE TO RETURN LOCATION OF CHARACTER IN CHARACTER STRING.
C
      SUBROUTINE UINDEX (STR1,LSTR1,STR2,LSTR2,IPOS)
C
C  INPUT VARIABLES -
C     STR1   - CHARACTER STRING TO BE CHECKED FOR STR2
C     LSTR1  - MAXIMUM NUMBER IF CHARACTERS IN VARIABLE CONTAINING
C              CHARACTER STRING TO BE CHECKED FOR STR2
C     STR2   - CHARACTER STRING TO BE CHECKED
C     LSTR2  - MAXIMUM NUMBER IF CHARACTERS IN VARIABLE CONTAINING
C              CHARACTER STRING TO BE CHECKED
C
C  OUTPUT VARIABLES -
C     IPOS   - STARTING LOCATION WHERE STR2 FOUND IN STR1
C                  0 = STR2 NOT FOUND IN STR1
C
C
      CHARACTER*1 STR1(LSTR1),STR2(LSTR2)
C
      COMMON /UIOX/ LP,ICD,LPD,LPE,ICDPUN,LSYS
      COMMON /UCMDBX/ ICMPRU,ICMTRC,ICMDBG
      INCLUDE 'flogm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/uindex.f,v $
     . $',                                                             '
     .$Id: uindex.f,v 1.1 1995/09/17 19:02:42 dws Exp $
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
         ENDIF
C
      IPOS=0
C
      DO 30 I=1,LSTR1
         IF (FEWSDEBUG.GE.6) THEN
CSR            CALL ULINE (ICMPRU,1)
            WRITE (MESSAGESTRING,80) I,STR1(I),STR2(1)
	    call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
            ENDIF
         IF (STR1(I).NE.STR2(1)) GO TO 30
            NUM=LSTR2-1
            IF (NUM.EQ.0) GO TO 20
            IF (I+NUM.GT.LSTR1) GO TO 40
               DO 10 J=1,NUM
                  IF (FEWSDEBUG.GE.6) THEN
CSR                     CALL ULINE (ICMPRU,1)
                     WRITE (MESSAGESTRING,85) I,J,STR1(I+J),STR2(J+1)
	             call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
                     ENDIF
                  IF (STR1(I+J).NE.STR2(J+1)) GO TO 30
10                CONTINUE
20          IPOS=I
            GO TO 40
30       CONTINUE
C
40    IF (FEWSDEBUG.GE.6) THEN
CSR         CALL ULINE (ICMPRU,1)
         WRITE (MESSAGESTRING,90) IPOS
	 call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT (' *** ENTER UINDEX')
60    FORMAT (' LSTR1=',I3,3X,'STR1=',100A1)
70    FORMAT (' LSTR2=',I3,3X,'STR2=',100A1)
80    FORMAT (' I=',I3,3X,'STR1(I)=',A1,3X,'STR2(1)=',A1)
85    FORMAT (' I=',I3,3X,'J=',I3,3X,
     *   'STR(I+J)=',A1,3X,'STR2(J+1)=',A1)
90    FORMAT (' *** EXIT UINDEX - IPOS=',I3)
C
      END
