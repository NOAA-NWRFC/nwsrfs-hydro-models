C MEMBER XSOH26
C  (from old member FCXSOH26)
C
C DESC COMPUTE MODIFIED PULS CURVE (O VS. S+0/2) FROM ELEV VS Q CURVE.
C---------------------------------------------------------------------
      SUBROUTINE XSOH26(SUNUM,PO,W,LOCOWS,SPEL,SPQ,NSP,CONSTQ,IUSE)
C--------------------------------------------------------------------
C  SUBROUTINE TO COMPUTE A MODIFIED PULS CURVE ( O VS. S+O/2 ) FROM
C  AN ELEVATION VS. DISCHARGE CURVE.
C  THE 'IUSE' VARIABLE CONTROLS THE ADDITION OF A NON-SPILLWAY DISCHARGE
C  TO THE CURVE (=0, DON'T ADD, = 1, ADD TO DISCHARGE)
C
C---------------------------------------------------------------------
C  WRITTEN BY - JOE OSTROWSKI - HRL - SEPT 1983
C---------------------------------------------------------------------
C
      INCLUDE 'common/resv26'
      INCLUDE 'common/exg26'
!CP   INCLUDE 'common/fdbug'
      INCLUDE 'flogm'
C
      DIMENSION PO(1),W(1),LOCOWS(1),SPEL(1),SPQ(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xsoh26.f,v $
     . $',                                                             '
     .$Id: xsoh26.f,v 1.1 1995/09/17 19:07:06 dws Exp $
     . $' /
C    ===================================================================
C
C
!CP   IF (IBUG.GE.1) WRITE(IODBUG,1600)
!CP   1600 FORMAT('   *** ENTER XSOH26 ***')
      IF ( FEWSDEBUG.GE.4 ) THEN
         WRITE(MESSAGESTRING, '(A23)') '   *** ENTER XSOH26 ***'
	 call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
      END IF
C
C  GET BASE AND LEVEL OF CALLING ROUTINE
C
      CALL XPTR26(SUNUM,PO,IORD,IBASE,LEVEL,LOCPM,LOCTS,LOCCO)
C
C  NO NEED TO REDEFINE CURVE IF ALREADY IN CORE.
C
      ISUNUM = IBASE*10 + LEVEL
      IF (MRLOC(5) .EQ. ISUNUM) GO TO 9000
C
C  COMPUTE CURVE
C
      MRLOC(5) = ISUNUM
      NOSOH = NSP
      DO 100 I=1,NSP
      LOCO = LOCOWS(10) + I - 1
      LOCSOH = LOCO + NOSOH
      O = SPQ(I)
      IF (IUSE.EQ.1) O = O + CONSTQ
      W(LOCO) = O
      CALL NTER26(SPEL(I),S,PO(LESELV),PO(LESSTO),NSE,IFLAG,NTERP,IBUG)
      W(LOCSOH) = S + O/2.0
  100 CONTINUE
C
!CP   IF (IBUG.GE.2) WRITE(IODBUG,1690) (W(LOCOWS(10)+I-1),I=1,NOSOH)
!CP   IF (IBUG.GE.2) WRITE(IODBUG,1691) (W(LOCOWS(10)+NOSOH+I-1),
      IF ( FEWSDEBUG.GE.4 ) THEN
         NPRINT=NOSOH
	 IF ( NPRINT.GT. 100) NPRINT=100

         WRITE(MESSAGESTRING, 1690) 
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 1690 FORMAT(/'   *MODIFIED PULS CURVE AS COMPUTED (O VS S+O/2)') 
         WRITE(FORMATSTR, *) '%12.3f'
	 call logonedimensionarrayfromfortran(DEBUG_LEVEL,NULLLINE,1,6,
     >        FORMATSTR,LOCOWS(10),LOCOWS(10)+NPRINT-1,1,W)

         WRITE(FORMATSTR, *) '%12.3f'
	 call logonedimensionarrayfromfortran(DEBUG_LEVEL,NULLLINE,1,6,
     >       FORMATSTR,LOCOWS(10)+NPRINT,LOCOWS(10)+NPRINT+NPRINT-1,1,W)
      END IF
C
C  THAT'S ALL!
C
 9000 CONTINUE
!CP   IF (IBUG.GE.1) WRITE(IODBUG,1699)
!CP   1699 FORMAT('    *** EXIT XSOH26 ***')
      IF ( FEWSDEBUG.GE.4 ) THEN
         WRITE(MESSAGESTRING, '(A23)') '    *** EXIT XSOH26 ***'
	 call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
      END IF
      RETURN
      END
