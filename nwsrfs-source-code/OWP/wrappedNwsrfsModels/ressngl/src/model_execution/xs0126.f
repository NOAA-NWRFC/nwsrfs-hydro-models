C MEMBER XS0126
C  (from old member FCXS0126)
C
      SUBROUTINE XS0126(SUNUM,PO,W)
C---------------------------------------------------------------------
C  SUBROUTINE TO SET UP PROPER PARAMETERS FOR CALL TO PASN26, THE
C  ROUTINE TO EXECUTE THE PASS INFLOW SCHEME.
C---------------------------------------------------------------------
C  WRITTEN BY - JOE OSTROWSKI - HRL - AUGUST 1983
C---------------------------------------------------------------------
C
      INCLUDE 'common/resv26'
      INCLUDE 'common/exg26'
!CP   INCLUDE 'common/fdbug'
      INCLUDE 'flogm'
C
      DIMENSION PO(1),W(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xs0126.f,v $
     . $',                                                             '
     .$Id: xs0126.f,v 1.1 1995/09/17 19:06:51 dws Exp $
     . $' /
C    ===================================================================
C
C
!CP   IF (IBUG.GE.1) WRITE(IODBUG,1600)
!CP   1600 FORMAT('   *** ENTER XS0126 ***')
      IF ( FEWSDEBUG.GE.4 ) THEN
         WRITE(MESSAGESTRING, '(A23)') '   *** ENTER XS0126 ***'
	 call logfromfortran(DEBUG_LEVEL, MESSAGESTRING) 
      END IF
C
C---------------------------------------------------------------------
C  NOT MUCH NEEDS TO BE DONE IN PREPARATION. THE ONLY PARMS NEEDED BY
C  THE SCHEME ARE THE STORAGE - ELEVATION CURVE VALUES.
C
C------------------------------------------
C  CALL THE COMPUTING ROUTINE
C
      CALL PASN26(PO(LESSTO),PO(LESELV))
C
C------------------------------------------
C  THAT'S IT!
C
      SUNUM = 1011.01

!CP   IF (IBUG.GE.1) WRITE(IODBUG,1699)
!CP   1699 FORMAT('    *** EXIT XS0126 ***')
      IF ( FEWSDEBUG.GE.4 ) THEN
         WRITE(MESSAGESTRING, '(A23)') '    *** EXIT XS0126 ***'
	 call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
      END IF
      RETURN
      END
