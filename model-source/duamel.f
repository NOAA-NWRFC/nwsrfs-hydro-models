      !      PRINT *,'DUAMEL'
      !      PRINT *, UN1,UT,DT,Q(1),NU1,N,M,K,NTAU
      !      PRINT *,'DUAMEL?'
 
      ! SUBROUTINE DUAMEL(Q,UN1,UT,DT,N,MM,K,NTAU,QB)
      ! AWW adding documentation for input variables
      ! Q      : TCI (total channel inflow) vector ... .ie unrouted
      ! U1     : unit hydrograph vector
      ! UN1    : unit hydrograph shape parameter (for gamma dist)
      ! UT     : unit hydrograph scale parameter (for gamma dist)
      ! DT     : timestep of the UH function (in days or fractions thereof)
      ! N      : sim_length + uh_length ...ie length of U1
      ! M      : max UH length?
      ! QB     : routed flow vector
      ! K      : 
      ! NTAU   : 

      ! AWW adding documentation for local variables
      ! SP     : 

C
C===========================================================
C
C     THIS SUBROUTINE PERFORM UNIT HYDROGRAPH ROUTING
C
      SUBROUTINE DUAMEL(Q,UN1,UT,DT,N,MM,K,NTAU,QB)
      IMPLICIT REAL (A-H,O-Z)
      INTEGER A,B,M
      
      REAL, DIMENSION(N-MM), INTENT(IN) :: Q
      REAL, INTENT(IN) :: UN1, UT, DT
      INTEGER, INTENT(IN) :: N, MM, K, NTAU
      REAL, DIMENSION(N), INTENT(OUT) :: QB

      REAL U1(MM)

      !write(*,*) UN1,UT,DT,N,MM,K,NTAU
      !U1 = 0

      M = MM

      IF(UN1 < 0)THEN
        U1(1)=1.
        M = 1
        GOTO 6
      ELSE
        IF (K .EQ. 0) GOTO 6
      END IF
      SP=0.
      TOC=GF(UN1)
      TOC=LOG(TOC*UT)
      !write(*,*)'TOC: ',TOC
      DO 1 I=1,M
      TOP=I*DT/UT
      TOR=(UN1-1)*LOG(TOP)-TOP-TOC
      U1(I)=0.0
      IF(TOR.GT.-8.) THEN
        U1(I)=EXP(TOR)
      ELSE
        IF (I .GT. 1) THEN
          U1(I) = 0.0
          M = I
          GO TO 12
        END IF
      END IF
      SP=SP+U1(I)
    1 CONTINUE
   12 CONTINUE
      IF (SP .EQ. 0) SP=1.0E-5
      SP=1./SP
      DO 7 I=1,M
      U1(I)=U1(I)*SP
    7 CONTINUE
      !do L=1,M
      !  if (U1(L)>0)write(*,*)'U1',L,U1(L)
      !end do 
    6 CONTINUE
      IOC=N+NTAU
      IF(N.GT.M)GO TO 10
      DO 2 I=1,IOC
      QB(I)=0.
      A=1
      IF(I.GT.M)A=I-M+1
      B=I
      IF(I.GT.N)B=N
      DO 3 J=A,B
      IOR=I-J+1
      !if(I.lt.10)write(*,*)QB(I)+U1(J)*Q(IOR)
      QB(I)=QB(I)+Q(J)*U1(IOR)
    3 CONTINUE
    2 CONTINUE
      GO TO 11
   10 DO 4 I=1,IOC
      QB(I)=0.
      A=1
      IF(I.GT.N)A=I-N+1
      B=I
      IF(I.GT.M)B=M
      DO 5 J=A,B
      IOR=I-J+1
      !if(I.lt.10)write(*,*)QB(I)+U1(J)*Q(IOR)
      QB(I)=QB(I)+U1(J)*Q(IOR)
 5    CONTINUE
 4    CONTINUE
 11   RETURN
      END


C
C=================================================================
C
      FUNCTION GF(Y)
      REAL, INTENT(IN)::Y
      REAL::X,H,GF
      GF=0.0
      H=1
      X=Y
 38   IF(X.LE.0.)GO TO 39
      IF(X.EQ.2.)GO TO 42
      IF(X.GT.2.)GO TO 40
      H=H/X
      X=X+1
      GO TO 38
  40  IF(X.LE.3.)GO TO 44
      X=X-1
      H=H*X
      GO TO 38
  44  X=X-2
      H=(((((((.0016063118*X+0.0051589951)*X+0.0044511400)*X+.0721101567
     *)*X+.0821117404)*X+.4117741955)*X+.4227874605)*X+.9999999758)*H
 42   GF=H
  39  RETURN
      END
