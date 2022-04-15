C MEMBER COX51
C--------------------------------------------------------     
C
C@PROCESS LVL(77)
C
C DESC CARRYOVER TRANSFER FOR OPERATION 51 - SSARRESV.
C
      SUBROUTINE COX51(POLD,COLD,PNEW,CNEW)
C
C  CARRYOVER TRANSFER ROUTINE FOR OPERATION 51 - SSARRESV.
C
C--------------------------------------------------
C  WRITTEN BY -  KSHSU - HRL - OCTOBER 1994
C---------------------------------------------------
C
C      COMMON BLOCKS
      
      COMMON/IONUM/IN,IPR,IPU
      INCLUDE 'flogm'
      REAL*8 SUBNAM
      LOGICAL CONV3
      DIMENSION POLD(*),COLD(*),PNEW(*),CNEW(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox51.f,v $
     . $',                                                             '
     .$Id: cox51.f,v 1.2 2006/03/16 18:55:14 xfan Exp $
     . $' /
C    ===================================================================
C
C
      DATA SUBNAM/8HCOX51   /
C
C----------------------------
C  SET DEBUG AND TRACE LEVELS
C      
      IF (FEWSDEBUG.GE.2) THEN
         WRITE(MESSAGESTRING,900)
         call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
  900    FORMAT(' *** ENTER COX51 ***')
      END IF
      IUNTO=POLD(8)
      IUNTN=PNEW(8)
      NCTO=POLD(15)
      NCTN=PNEW(15)

C dws    The following 4 statements were replace below to avoid
C dws     compiler warnings ... 2006-01-23

C     IRUO=POLD(POLD(11))
C     IRUN=PNEW(PNEW(11))
C     IRDO=POLD(POLD(10))
C     IRDN=PNEW(PNEW(10))

        IDXO10=POLD(10)
        IDXO11=POLD(11)
        IDXN10=PNEW(10)
        IDXN11=PNEW(11)
      IRUO=POLD(IDXO11)
      IRUN=PNEW(IDXN11)
      IRDO=POLD(IDXO10)
      IRDN=PNEW(IDXN10)
      
      IF(FEWSDEBUG.EQ.0) GO TO 410
      WRITE(MESSAGESTRING,902) (COLD(I),I=1,NCTO)
      call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
  902 FORMAT(5X,'OLD CARRYOVER ARRAY:',
     & /5X,F10.0,2(F10.0,F11.2,F10.0,F10.0))
      WRITE(MESSAGESTRING,904) (CNEW(I),I=1,NCTN)
      call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
 904  FORMAT(5X,'CARRYOVER ARRAY FROM SEGMENT REDEFITION:',
     & /5X,F10.0,2(F10.0,F11.2,F10.0,F10.0))
  410 CONTINUE
      IF(IUNTO.EQ.IUNTN .AND. NCTN.EQ.NCTO
     & .AND. IRUO.EQ.IRUN .AND. IRDO.EQ.IRDN) GO TO 44
      WRITE(MESSAGESTRING,800) 
      call logfromfortran(WARNING_LEVEL,MESSAGESTRING)
 800  FORMAT(
     & /5X,'**WARNING** FOR SSARRESV OPERATION')
      IF(IUNTN.NE.IUNTO) then        
        WRITE(MESSAGESTRING,805)
        call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
 805    FORMAT(10X,'INPUT UNITS HAS BEEN CHANGED')
      endif
      IF(NCTN.NE.NCTO) then
      WRITE(MESSAGESTRING,806)
      call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
 806  FORMAT(10X,'NO OF RESERVOIRS/STATIONS HAS BEEN CHANGED')
      endif
      IF(IRUN.NE.IRUO) then 
      WRITE(MESSAGESTRING,807)
      call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
 807  FORMAT(10X,'UPSTREAM RESERVOIRS/STATIONS TYPE HAS BEEN CHANGED')
      endif
      IF(IRDN.NE.IRDO) then
      WRITE(MESSAGESTRING,808)
      call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
 808  FORMAT(10X,'UPSTREAM RESERVOIRS/STATIONS TYPE HAS BEEN CHANGED')
      endif
      WRITE(MESSAGESTRING,810)
      call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
 810  FORMAT(
     3  10X,'CARRYOVER VALUES INPUT DURING THE SEGMENT REDEFINITION ',
     4  'WILL BE USED.')
      GO TO 510
C
C-----------------------------------------
C  TRANSFER INFLOW CARRYOVER
C
   44 CONTINUE
      DO 50 I=1,NCTN
 50   CNEW(I) = COLD(I)
C
C
 500  CONTINUE
      IF(FEWSDEBUG.EQ.0) GO TO 510
      WRITE(MESSAGESTRING,906) (CNEW(I),I=1,NCTN)
      call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
 906  FORMAT(5X,'FINAL CARRYOVER ARRAY AFTER CARRYOVER TRANSFER:',
     & /5X,F11.1,2(F11.1,F11.2,F11.0))
  510 CONTINUE
      RETURN
      END
