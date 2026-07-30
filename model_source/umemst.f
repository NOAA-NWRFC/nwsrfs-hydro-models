C MEMBER UMEMST
C-----------------------------------------------------------------------
C
      SUBROUTINE UMEMST (IVALUE,IARRAY,NUM)
C
C  INITIALIZE VARIABLE WITH SPECIFIED VALUE
C
CCB  IARRAY is a dummy argument whose real length is passed in NUM; the
CCB  legacy DIMENSION IARRAY(1) declaration makes gfortran -fcheck=all abort
CCB  with a spurious "above upper bound of 1" on the first real write. The
CCB  assumed-size form (*) is the portable idiom for such dummies and does
CCB  not change behaviour (see Writing R Extensions, Fortran array bounds).
      DIMENSION IARRAY(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER(len=68)     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/umemst.f,v $
     . $',                                                             '
     .$Id: umemst.f,v 1.1 1995/09/17 19:02:16 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (NUM.LE.0) GO TO 20
C
      DO 10 I=1,NUM
         IARRAY(I)=IVALUE
10       CONTINUE
C
20    RETURN
C
      END
