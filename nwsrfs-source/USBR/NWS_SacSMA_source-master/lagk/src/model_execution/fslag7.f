C MEMBER FSLAG7
C  (from old member FCSUBFN7)
C-----------------------------------------------------------------------
C
      REAL FUNCTION FSLAG7(L,Q,N,LY,TABLE)
C.......................................................................
C
C     THIS FUNCTION LINEARLY INTERPOLATES IN THE SECOND COLUMN
C     OF A TWO DIMENSIONAL MATRIX AND RETURNS THE VALUE FROM
C     THE FIRST COLUMN WHICH CORRESPONDS TO THE LOCATION IN THE
C     SECOND COLUMN WHICH MATCHES A SUPPLIED VALUE.
C.......................................................................
C
C      THIS ROUTINE ORIGINALLY PROGRAMMED BY
C                    GEORGE F. SMITH - HRL   DECEMBER 1979
C.......................................................................
C
C      VARIABLES IN ARGUMENT LIST
C
C        1. L     - OUTPUT - THE NUMBER OF THE PAIR OF VALUES BELOW
C                   THE VALUE FOUND IN THE TABLE
C        2. Q     - THE VALUE TO BE MATCHED IN SECOND COULMN OF TABLE
C        3. N     - THE NUMBER OF PAIRS OF VALUES IN TABLE
C        4. TABLE - THE TABLE TO BE SEARCHED
C.......................................................................
C
      DIMENSION TABLE(2,N)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_lagk/RCS/fslag7.f,v $
     . $',                                                             '
     .$Id: fslag7.f,v 1.1 1995/09/17 19:19:56 dws Exp $
     . $' /
C    ===================================================================
C
      IF(Q.GT.TABLE(2,1))GO TO 10
      FSLAG7=TABLE(1,1)
      L=1
      RETURN
   10 IF(N.EQ.1)GO TO 30
      J1=2
      IF(L.GT.LY) J1=L-LY
      DO 20 J=J1,N
      I=J
      IF(Q.LE.TABLE(2,I))GO TO 40
   20 CONTINUE
   30 FSLAG7=TABLE(1,N)
      L=N
      RETURN
   40 L=I-1
      FSLAG7=TABLE(1,L) + (Q-TABLE(2,L))/(TABLE(2,I)-TABLE(2,L))*
     1                    (TABLE(1,I)-TABLE(1,L))
      RETURN
      END
