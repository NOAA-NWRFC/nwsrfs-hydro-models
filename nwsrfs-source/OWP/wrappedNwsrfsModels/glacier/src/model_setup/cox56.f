      SUBROUTINE COX56(POLD,COLD,PONEW,CONEW)
C............................................
C     CARRYOVER TRANSFER ROUTINE
C............................................
      DIMENSION POLD(*),COLD(*),PONEW(*),CONEW(*)
      CHARACTER*8  SNAME
      REAL KG1,KG2,KGNEW,KGOLD
C
cav      INCLUDE 'common/fdbug'
      INCLUDE 'flogm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox56.f,v $
     . $',                                                             '
     .$Id: cox56.f,v 1.2 2000/12/19 14:47:10 jgofus Exp $
     . $' /
C    ===================================================================
C
C     DATA STATEMENTS
      DATA  SNAME / 'COX56   ' /
C.............................................
C     TRACEL LEVEL =1, DEBUG SWITCH = IBUG
cav      CALL FPRBUG(SNAME,1,56,IBUG)
      IF (FEWSDEBUG.GE.1) THEN 
         WRITE(MESSAGESTRING,10)
	 call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
  10     FORMAT(1H0,16H** COX56 ENTERED)
      ENDIF
C.............................................
C     IF DEBUG ON, PRINT OLD CORRYOVER IF ANY EXISTS
cav      IF (IBUG.EQ.0) GOTO 100
      IF (FEWSDEBUG.GE.1) THEN 
         WRITE(MESSAGESTRING,900)COLD(1),COLD(2)
	 call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
 900     FORMAT(1H0,'GLACIER OPER -- OLD CARRYOVER=',F8.2,2x
     &,F8.2)
      ENDIF
C.............................................
C     CONTROL STATEMENTS
 100  CG1OLD=POLD(9)
      CG2OLD=POLD(10)
      CG1NEW=PONEW(9)
      CG2NEW=PONEW(10)
      KG1=POLD(12)
      KG2=POLD(13)
      AFINEW=CONEW(2)
      AFIOLD=COLD(2)
C.............................................
C     TRANSFER CARRYOVER -- ADJUST STORAGE - CO(1)
C     CALCULATE KGOLD
      NUM=EXP(CG1OLD+CG2OLD*AFIOLD)
      FAFI=NUM/(1+NUM)
      KGOLD=KG1+(KG2-KG1)*FAFI
C     CALCULATE KGNEW
      NUM=EXP(CG1NEW+CG2NEW*AFIOLD)
      FAFI=NUM/(1+NUM)
      KGNEW=KG1+(KG2-KG1)*FAFI
C     CALCULATE ADJUSTED CONEW(1)
      CONEW(1)=(KGOLD/KGNEW)*COLD(1)
C
C     TRANSFER CARRYOVER -- NO ADJUSTMENT - CO(2)
      CONEW(2)=COLD(2)
C     IF DEBUG IS ON PRINT NEW CARRYOVER
cav      IF (IBUG.EQ.0) GOTO 190
      IF (FEWSDEBUG.GE.1) THEN 
        WRITE(MESSAGESTRING,901)CONEW(1),CONEW(2)
        call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
 901    FORMAT(1H0,'GLACIER OPER -- NEW CARRYOVER',F8.2,2X,
     &F8.2)
      ENDIF
C...............................................
cav 190  IF(ITRACE.GE.1) WRITE (IODBUG,902)
      WRITE (MESSAGESTRING,902)
      call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
 902  FORMAT(1H0,'**EXIT COX56')
      
      END

