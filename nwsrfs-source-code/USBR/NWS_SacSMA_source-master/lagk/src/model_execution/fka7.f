C MEMBER FKA7
C  (from old member FCEX7)
C
      SUBROUTINE FKA7(P,C,QB,NDT,COTIME,IB)
C
C.......................................................................
C
C     THIS SUBROUTINE DOES THE ATTENUATION (K) COMPUTATIONS
C     FOR THE ATLANTA METHOD
C.......................................................................
C
C      SUBROUTINE ORIGINALLY PROGRAMMED BY
C            GEORGE F. SMITH - HRL   DECEMBER 1979
C.......................................................................
C
C      VARIABLES IN ARGUMENT LIST
C        1. P      - THE P ARRAY
C        2. C      - THE C ARRAY
C        3. QB     - INPUT  - LAGGED INFLOW
C                    OUTPUT - ROUTED (ATTENUATED) OUTFLOW
C        4. NDT    - THE NUMBER OF TIME STEPS IN THIS EXECUTION
C        5. COTIME - TIME (IN HOURS RELATIVE TO START OF RUN)
C                    AT WHICH CARRYOVER WILL BE SAVED
C        6. IB     - PRINT DEBUG FLAG, PRINT IF IB = 1
C.......................................................................
C
C      INCLUDE 'common/fdbug'
C      INCLUDE 'common/ionum'
      include 'flogm'
      COMMON/IONUM/IN,IPR,IPU
CFC      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)      
CFC      INCLUDE 'common/fcpuck'
      COMMON /FCPUCK/ ICPUT,ICPUON,INCPU
C
      DIMENSION P(1),C(1),QB(1)
C
      LOGICAL MEANQ,CONK,NOTCPU,LBUG
      CHARACTER *5 INDIMS
      CHARACTER *5 IUNIT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_lagk/RCS/fka7.f,v $
     . $',                                                             '
     .$Id: fka7.f,v 1.1 1995/09/17 18:57:59 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA L3/4HL3  /
      DATA ICPU/4HCPU /
C
Clc      LBUG=.FALSE.
Clc      IF(IB.EQ.1)LBUG=.TRUE.
C
      NOTCPU=.TRUE.
CFC      IF(LBUG.AND.IFBUG(ICPU).EQ.1)NOTCPU=.FALSE.
C
CFC      IF(LBUG)WRITE(IODBUG,600)
      IF(FEWSDEBUG .GE. 1) THEN
        WRITE(MESSAGESTRING,600)
        call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      END IF      
  600 FORMAT(1H0,10X,15H** FKA7 ENTERED)
C
CFC      IF(NOTCPU)GO TO 700
CFC      CALL URTIMR(LAPSE,ICPUT)
CFC      ELAPSE=LAPSE/100.
CFC      XICPUT=ICPUT/100.
CFC      WRITE(IODBUG,701)ELAPSE,XICPUT
CFC  701 FORMAT(1H0,10X,'** ELAPSED CPU TIME = ',F13.2,
CFC     1  ', TOTAL CPU TIME = ',F13.2)
C
700   IBOS=P(16)+1
      IBOS1=IBOS+1
      NPOS=P(IBOS)
      IBOS4=IBOS+NPOS*2+1
      IBOS14=IBOS4+1
      IBK=P(18)
      NPKQ=P(IBK)
      IBK1=IBK+1
C
      CONK=.FALSE.
      IF(NPKQ.EQ.0)CONK=.TRUE.
C
      IF(.NOT.CONK)GO TO 2
      XK1=P(IBK1)
      XK2=XK1
      XK14=XK1
      XK24=XK1
C
    2 ITA=P(5)
      XITA=ITA/4.
C
      X2=C(2)
      Y1=C(3)
      S2ODT=C(4)*2./ITA
      FACT=1.
      IPWARN=0
C
      DTYPIN=P(4)

      IF ( FEWSDEBUG .GT. 3 ) THEN
        WRITE(MESSAGESTRING, 681) 'DTYPIN from param file =', DTYPIN
	call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      END IF 
!CP   WRITE(*,681) 'DTYPIN from param file =', DTYPIN
CFC   CALL FDCODE(DTYPIN,STDUNT,INDIMS,MSNG,NUPDT,TSCALE,NEXTRA,IER)
   
      CALL getDimensionAndUnitInFortran(DTYPIN, INDIMS, IUNIT)

!CP   WRITE(*,*) ' DTYPIN = ', DTYPIN, 'INDIMS =', IDIMS, 'IUNIT=', 
!CP   + IUNIT
!CP   WRITE(*,681)' DTYPIN = ', DTYPIN
!CP   WRITE(*,681)'INDIMS =', IDIMS
!CP   WRITE(*,681)'IUNIT=', IUNIT
      IF ( FEWSDEBUG .GT. 3 ) THEN
        WRITE(MESSAGESTRING, 681) ' DTYPIN = ', DTYPIN
        call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
	WRITE(MESSAGESTRING, 681) 'INDIMS =', IDIMS
        call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
	WRITE(MESSAGESTRING, 681) 'IUNIT=', IUNIT
        call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      END IF
  681 FORMAT(A25, A8) 
  682 FORMAT(A10, A8,A10, A8, A10, A8)  
      MEANQ=.FALSE.
      IF(INDIMS.EQ.'L3  ')MEANQ=.TRUE.
C
CLC      IF(.NOT.LBUG)GO TO 20
         IF(FEWSDEBUG .EQ. 0)GO TO 20
CFC      IF(FEWSDEBUG .GE. 1) THEN
CFC      WRITE(IODBUG,602)IBOS,IBOS1,NPOS,IBOS4,IBOS14,
CFC     1  IBK,NPKQ,IBK1,ITA,XITA,X2,Y1,S2ODT,MEANQ
      WRITE(MESSAGESTRING,602)IBOS,IBOS1,NPOS,IBOS4,IBOS14,
     1  IBK,NPKQ,IBK1,ITA,XITA,X2,Y1,S2ODT,MEANQ      
      call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
  602 FORMAT(1H0,10X,47HINITIAL VALUES FOR IBOS,IBOS1,NPOS,IBOS4,IBOS14
     1   ,45H,IBK,NPKQ,IBK1,ITA,XITA,X2,Y1,S2ODT,MEANQ ARE/11X,9I10,
     2    G10.4/11X,3G10.4,2X,L4)
       
CFC      WRITE(IODBUG,611)(QB(I),I=1,NDT)
C LC just write max of 100 values for debug
      MAXDEBUG=NDT
      IF(MAXDEBUG.GT.100)THEN
         MAXDEBUG=100
      ENDIF
      
      WRITE(MESSAGESTRING,611) (QB(I),I=1,MAXDEBUG)
      call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      
  611 FORMAT(1H0,10X,'INFLOW VALUES TO K OPERATION (QB ARRAY)'/
     1  (1X,10G10.4))
      IEOS1=IBOS4-1
      DO 622 I=IBOS1,IEOS1
CFC      WRITE(IODBUG,612)NPOS,(P(I),I=IBOS1,IEOS1)
        WRITE(MESSAGESTRING,612)NPOS,P(I)
        call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
  622 CONTINUE    
  612 FORMAT(1H0,10X,'THE O VS 2*S/DT+O TABLE FOLLOWS - THE NUMBER OF ',
     1  'PAIRS OF VALUES IS ',I3/(11X,8G12.4))
C
      DO 12 I=1,NPKQ
      IF(XITA*2.0.GT.P(IBK+I*2-1))GO TO 15
   12 CONTINUE
C
CFC      WRITE(IODBUG,615)
      WRITE(MESSAGESTRING,615)
      call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
  615 FORMAT(1H0,10X,'THERE IS NO O VS 2*S/(DT/4)+O TABLE')
      GO TO 20
C
   15 NPOS4=P(IBOS4)
      IEOS14=IBOS4+2*NPOS4
      DO 623 I=IBOS14,IEOS14
CFC      WRITE(IODBUG,613)NPOS4,(P(I),I=IBOS14,IEOS14)
      WRITE(MESSAGESTRING,613)NPOS4,P(I)
      call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
  623 CONTINUE
  613 FORMAT(1H0,10X,'THE O VS 2*S/(DT/4)+O TABLE FOLLOWS - THE ',
     1  'NUMBER OF PAIRS OF VALUES IS ',I3/(11X,8G12.3))
CFC      END IF
C.......................................................................
C
C     BEGIN OVERALL LOOP FOR THIS PASS THROUGH K OPERATION
C.......................................................................
C
   20 IF(NOTCPU)GO TO 702
CFC      CALL URTIMR(LAPSE,ICPUT)
CFC      ELAPSE=LAPSE/100.
CFC      XICPUT=ICPUT/100.
CFC      WRITE(IODBUG,703)ELAPSE,XICPUT
CFC  703 FORMAT(1H0,10X,'**ABOUT TO START DO 10 LOOP -  ELAPSED CPU TIME ',
CFC     1  '= ',F13.2,', TOTAL CPU TIME = ',F13.2)
C
  702 DO 10 I=1,NDT
C
      X1=X2
      X2=QB(I)
      QPREV=Y1
C
      IF(.NOT.MEANQ)VALUE=X1 + X2 + S2ODT - Y1
      IF(MEANQ)VALUE=2.*X2*24./ITA + S2ODT - Y1
C
      IF(VALUE.LT.1.0E-7)VALUE=0.0
C
      Y2=FSERC7(LXXXX,VALUE,NPOS,P(IBOS1))
C
CFC      IF(LBUG.AND.MEANQ)WRITE(IODBUG,614)VALUE
      IF(FEWSDEBUG .GE. 1) THEN
        WRITE(MESSAGESTRING,614)VALUE
        call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      END IF       
  614 FORMAT(11X,'IN DO 10 LOOP, VALUE = ',G10.4)
C
      IF(MEANQ)GO TO 9
C
      IF(CONK)GO TO 56
C
      XK1=FSERC7(LXXXX,Y1,NPKQ,P(IBK1))
C
      XK2=FSERC7(LXXXX,Y2,NPKQ,P(IBK1))
C
CFC   56 IF(LBUG)WRITE(IODBUG,603)X1,X2,VALUE,Y2,XK1,XK2
   56 IF(FEWSDEBUG .GE. 1) THEN
        WRITE(MESSAGESTRING,603)X1,X2,VALUE,Y2,XK1,XK2
        call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      END IF   
  603 FORMAT(11X,40HIN DO 10 LOOP, X1,X2,VALUE,Y2,XK1,XK2 = ,6G10.4)
C
      IF(XK1.GE.XITA*2.0.AND.XK2.GE.XITA*2.0)GO TO 9
C
      IF(XK1.GT.XITA/2.0.OR.XK2.GT.XITA/2.0)GO TO 4
C
C.......................................................................
C
C     GET HERE IF K FOR Y1 AND K FOR Y2 ARE BOTH LT DT/2
C     SET OUTFLOW = MINIMUM OF (INFLOW,VALUE)
C.......................................................................
C
      Y2=X2
      IF(VALUE.LT.Y2)Y2=VALUE
CFC      IF(LBUG)WRITE(IODBUG,604)Y2
      IF(FEWSDEBUG .GE. 1) THEN
        WRITE(MESSAGESTRING,604)Y2
        call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      END IF      
  604 FORMAT(11X,17HK LT DT/2 - Y2 = ,G10.4)
C
      GO TO 9
C
C.......................................................................
C
C     GET HERE IF K FOR Y1 GT DT/2 AND K FOR Y2 LE DT/2 OR VICE VERSA
C     SOLVE EQUATIONS IN THIS LOOP WITH DT=ORIGINAL DT/4
C.......................................................................
C
    4 NPOS4=P(IBOS4)
      S2ODT=S2ODT*4.
      IF(S2ODT.LT.-0.5)IPWARN=IPWARN+1
      DX=X2-X1
C
CFC      IF(LBUG)WRITE(IODBUG,605)NPOS4,S2ODT,DX
      IF(FEWSDEBUG .GE. 1) THEN
        WRITE(MESSAGESTRING,605)NPOS4,S2ODT,DX
        call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      END IF      
  605 FORMAT(11X,41HABOUT TO ENTER DO 5 LOOP - TIME INTERVAL ,
     1   06H= DT/4/11X,17HNPOS4,S2ODT,DX = ,I10,2G10.4)
C
      DXOVR4=DX/4.
      XITAO2=XITA/2.
C
      DO 5 J=1,4
      X14=X1+(J-1)*DXOVR4
      X2=X14+DXOVR4
C
      VALUE=X14 + X2 + S2ODT - Y1
C
      Y2=FSERC7(LXXXX,VALUE,NPOS4,P(IBOS14))
C
      IF(CONK)GO TO 50
      XK14=FSERC7(LXXXX,Y1,NPKQ,P(IBK1))
      XK24=FSERC7(LXXXX,Y2,NPKQ,P(IBK1))
   50 IF(XK14.LT.XITAO2.OR.XK24.LT.XITAO2)GO TO 55
      GO TO 6
C
C.......................................................................
C
C     IF EITHER K FOR Y1 OR K FOR Y2 IS STILL LT NEW DT/2
C     (I.E. ORIGINAL DT/8) SET OUTFLOW=MIN(INFLOW,VALUE) AND CONTINUE IN
C     QUARTER PERIOD LOOP
C.......................................................................
C
CFC   55 IF(LBUG)WRITE(IODBUG,609)Y1,Y2,XK14,XK24,X2
   55 IF(FEWSDEBUG .GE. 1) THEN
        WRITE(MESSAGESTRING,609)Y1,Y2,XK14,XK24,X2
        call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      END IF   
  609 FORMAT(11X,45HK LT DT/8 IN DO 5 LOOP - SET Y2=MIN(X2,VALUE)/
     1   11X,21HY1,Y2,XK14,XK24,X2 = ,5G10.4)
      Y2=X2
      IF(VALUE.LT.Y2)Y2=VALUE
C
    6 S2ODT=VALUE - Y2
      IF(S2ODT.LT.-0.5)IPWARN=IPWARN+1
C
CFC      IF(LBUG)WRITE(IODBUG,606)J,X14,X2,VALUE,Y2,S2ODT
      IF(FEWSDEBUG .GE. 1) THEN
        WRITE(MESSAGESTRING,606)J,X14,X2,VALUE,Y2,S2ODT
        call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      END IF       
  606 FORMAT(11X,41HIN DO 5 LOOP - J,X14,X2,VALUE,Y2,S2ODT = ,
     1   I10,5G10.4)
C
      Y1=Y2
C
    5 CONTINUE
C
      FACT=4.
C
 9    QB(I)=Y2
C
      S2ODT=(VALUE - Y2)/FACT
C
      IF(S2ODT.LT.-0.5)IPWARN=IPWARN+1
      FACT=1.
C
CFC      IF(LBUG)WRITE(IODBUG,607)Y2,S2ODT
      IF(FEWSDEBUG .GE. 1) THEN
        WRITE(MESSAGESTRING,607)Y2,S2ODT
        call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      END IF      
  607 FORMAT(11X,27HIN DO 10 LOOP - Y2,S2ODT = ,2G10.4)
C
      Y1=Y2
C
   10 CONTINUE
C.......................................................................
C
C     STORE CARRYOVER - IF C(4), CURRENT STORAGE, IS LT ZERO
C     BECAUSE OF ROUNDOFF ERROR SET TO ZERO
C.......................................................................
C
      C(2)=X2
      C(3)=Y2
      C(4)=S2ODT*ITA/2.
C
      IF(C(4).LT.0.0)C(4)=0.0
C
CFC      IF(LBUG)WRITE(IODBUG,608)C(2),C(3),C(4)
      IF(FEWSDEBUG .GE. 1) THEN
        WRITE(MESSAGESTRING,608)C(2),C(3),C(4)
        call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      END IF      
  608 FORMAT(11X,36HAFTER DO 10 LOOP - C(2),C(3),C(4) = ,
     1   3G10.4)
C
CFC      IF(LBUG)WRITE(IODBUG,601)(QB(I),I=1,NDT)
C LC just write max of 100 values for debug
      IF(FEWSDEBUG .GE. 1) THEN       
         MAXDEBUG=NDT
         IF(MAXDEBUG.GT.100)THEN
            MAXDEBUG=100
         ENDIF
      
         WRITE(MESSAGESTRING,601)(QB(I),I=1,MAXDEBUG)
         call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      END IF      
  601 FORMAT(11X,14HIN FKA7 - QB =/(10X,14F8.2))
C
CFC      IF(NOTCPU)GO TO 704
CFC      CALL URTIMR(LAPSE,ICPUT)
CFC      ELAPSE=LAPSE/100.
CFC      XICPUT=ICPUT/100.
CFC      WRITE(IODBUG,705)ELAPSE,XICPUT
CFC  705 FORMAT(1H0,10X,'**LEAVING FKA7 - ELAPSED CPU TIME = ',F13.2,
CFC     1  ', TOTAL CPU TIME = ',F13.2)
C
  704 IF(IPWARN.LE.0)RETURN
C
CFC      WRITE(IPR,610)IPWARN
      WRITE(MESSAGESTRING,610)IPWARN
      call logfromfortran(WARNING_LEVEL,MESSAGESTRING)
  610 FORMAT(1H0,10X,46H**WARNING**  IN SUBROUTINE FKA7, THE VALUE OF
     1   ,26HS20DT HAS GONE BELOW -0.5 ,I10,07H TIMES./
     2  24X,56HTHIS INDICATES A POSSIBLE ERROR IN THE LAG/K PARAMETERS.)
CFC      CALL WARN
C
      RETURN
      END
