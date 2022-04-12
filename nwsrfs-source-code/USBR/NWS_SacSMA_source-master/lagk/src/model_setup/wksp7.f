      SUBROUTINE WKSP7(PO, CO, LWORK1, LWORK2)
C
      INCLUDE 'flogm'

      DIMENSION PO(1), CO(1)
      LOGICAL FOP7
      
      IF ( FEWSDEBUG.GE.4 ) THEN
         WRITE(MESSAGESTRING,*) '*** ENTER WKSP7 ***'
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
      END IF

      LWORK=0
      L1=0
      ITA=PO(5)        !inflow time interval
      ITB=PO(9)        !outflow time interval
C
C  COMPUTE NUMBER OF TIME STEPS
C
      call numberoftimesteps(ITA, L1)
      LWORK1 = L1
C
C  WORKING SPACE NEEDED FOR CHANGING TIME INTERVAL.
C
      IF((ITB.GT.0).AND.(ITA.NE.ITB)) GO TO 105
      LDT=0
      GO TO 110
 105  call numberoftimesteps(ITB, LDT)
C
C  COMPUTE TOTAL LENGTH OF WORKING SPACE NEEDED.
C
  110 LC=CO(1)
      IP18 = PO(18)
      IP19 = IP18+1
      IF (FOP7(PO(19),PO(20)).OR.FOP7(IP18,IP19)) THEN
         GO TO 115
      ENDIF
C
C     ONLY K PART USED.
C
      L2=LC
      IF(LDT.GT.L2) L2=LDT
      LWORK2=L1+L2

      IF ( FEWSDEBUG.GE.4 ) THEN
        WRITE(MESSAGESTRING, *) 'Only K part used LWORK1= ',LWORK1,
     >                          ' LWORK2= ',LWORK2
        call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
      END IF
      
      GO TO 120
C
C  BOTH LAG AND K OR ONLY LAG USED.
C
  115 LCLAG=CO(5)
      L2=LC+2*(LCLAG+L1)+3
      LWORK2=L1+L2
      IF ( FEWSDEBUG.GE.4 ) THEN
        WRITE(MESSAGESTRING, *) 'Both LAG and K or only LAG used: ',
     >                          'LWORK1= ',LWORK1,' LWORK2= ',LWORK2
        call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
      END IF

  120 RETURN
      END
