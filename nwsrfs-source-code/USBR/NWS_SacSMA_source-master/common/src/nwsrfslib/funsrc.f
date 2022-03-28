C MEMBER FUNSRC
C  (from old member FCFSHFRC)
C
C @PROCESS LVL(77)
      SUBROUTINE FUNSRC(H,Q,IADJ,NRCPT,IBUG)
      INCLUDE 'flogm'
!CP   COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/RCSHF/QSHIFT,QORG(112),HORG(112),NRCPTO
      DIMENSION Q(*),H(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mods/RCS/funsrc.f,v $
     . $',                                                             '
     .$Id: funsrc.f,v 1.2 1997/09/22 15:18:36 page Exp $
     . $' /
C    ===================================================================
C
      QSHIFT=0.
!CP   IF(IBUG.NE.0) WRITE(IODBUG,9000)
      IF ( FEWSDEBUG.GE.5 ) THEN
         WRITE(MESSAGESTRING, 9000)
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 9000 FORMAT(/12X,'UNSHIFTED RATING CURVE'/10X,' I     STAGE   DISCHARGE
     .')
      END IF
      IF(IADJ.NE.2) GO TO 500
      DO 100 L=1,NRCPTO
      Q(L)=QORG(L)
      H(L)=HORG(L)
!CP   IF(IBUG.NE.0) WRITE(IODBUG,9005) L,H(L),Q(L)
      IF ( FEWSDEBUG.GE.5 ) THEN
         WRITE(MESSAGESTRING, 9005) L,H(L),Q(L)
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 9005    FORMAT(10X,I2,2F10.2)
      END IF 
  100 CONTINUE
      NRCPT=NRCPTO
  500 RETURN
      END
