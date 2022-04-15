C MODULE PIN56
C
cav      SUBROUTINE PIN56(PO,LEFTP,IUSEP,CO,LEFTC,IUSEC)
      SUBROUTINE PIN56(PO,CO,FILENAME)
C......................................................
C  THIS IS THE INPUT SUBROUTINE FOR THE GLACIER STORAGE
C  MODEL.
C.......................................................
      DIMENSION PO(*),CO(*)
      DIMENSION GOID(2),RMID(2),AFID(2)
      CHARACTER*8  SNAME
      REAL KG1,KG2
C
cav      INCLUDE 'common/fdbug'
cav      INCLUDE 'common/ionum'
      INCLUDE 'flogm'
      
      CHARACTER*512 FILENAME 
      CHARACTER*5 IDIM1
      CHARACTER*5 IUNIT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/pin56.f,v $
     . $',                                                             '
     .$Id: pin56.f,v 1.5 2003/08/15 13:16:19 gzhou Exp $
     . $' /
C    ===================================================================
C
C
      DATA  SNAME / 'PIN56   ' /
      DATA  BLANK /4H    /
C
C  TRACE LEVEL FOR SUBROUTINE =1, DEBUG SWITCH = IBUG
cav      CALL FPRBUG(SNAME,1,56,IBUG)
      IF (FEWSDEBUG.GE.1) THEN 
         WRITE(MESSAGESTRING,900)
	 call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
  900    FORMAT(1H0,16H** PIN56 ENTERED)
      ENDIF
C  CONTROL VARIABLES
      IVER=2
cav      IUSEP=0
cav      IUSEC=0
      IUSEP=16
      IUSEC=2
      LPO=16
      LCO=2
cav      NOFILL=0
      
      IN=1
      OPEN(IN, FILE=TRIM(FILENAME), STATUS='OLD')
C........................................................
C     READ INPUT CARDS
cav      IERR=0
C
C     CARD 1
C
      READ(IN,500)RMID,RMTYPE,IDT,GOID,GOTYPE,AFID,AFTYPE
      WRITE(MESSAGESTRING,500) QIN, QOUT
      call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
 500  FORMAT(2X,2A4,1X,A4,3X,I2,2X,2A4,1X,A4,2X,2A4,1X,A4)
cav      CALL CHEKTS(RMID,RMTYPE,IDT,1,'L   ',0,1,IERR)
cav      IF (IERR.EQ.1) GOTO 900
      CALL getDimensionAndUnitInFortran(RMTYPE, IDIM1, IUNIT)
      WRITE(ITYPE, 510) RMTYPE
  510 FORMAT(A4)
      IF ((IDIM1.NE.'L   ').OR.(IUNIT.NE.'MM'))THEN
         WRITE(MESSAGESTRING, *)
     +      'FOR RAIN+MELT TS ', ITYPE,
     +      ' L    IS EXPECTED INSTEAD OF ', IDIM1,
     +      ' MM IS EXPECTED INSTEAD OF ', IUNIT
            call logfromfortran(FATAL_LEVEL,MESSAGESTRING)
      ENDIF
      
      
cav      CALL CHEKTS(GOID,GOTYPE,IDT,1,'L   ',0,1,IERR)
cav      IF (IERR.EQ.1) GOTO 900
      CALL getDimensionAndUnitInFortran(GOTYPE, IDIM1, IUNIT)
      WRITE(ITYPE, 520) GOTYPE
  520 FORMAT(A4)
      IF ((IDIM1.NE.'L   ') .OR. (IUNIT.NE.'MM')) THEN
         WRITE(MESSAGESTRING, *)
     +      'FOR GOUT TS ', ITYPE,
     +      ' L    IS EXPECTED INSTEAD OF ', IDIM1,
     +      ' MM IS EXPECTED INSTEAD OF ', IUNIT
            call logfromfortran(FATAL_LEVEL,MESSAGESTRING)
      ENDIF
      IF((AFID(1).EQ.BLANK).AND.(AFID(2).EQ.BLANK))IVER=1
C      
C     CARD2
C
      READ(IN,800)CG1,CG2,CG3,KG1,KG2
 800  FORMAT(2X,F6.1,2X,F4.2,2X,F4.2,2X,F4.2,2X,F4.2)
C
C     CARD3
C
      READ(IN,850)STORAGE,AFI
 850  FORMAT(2X,F6.1,2X,F6.1)
C.........................................................
C     CHECK INITIAL VALUES
cav   added checking CG2 routing parameter 2 value
      IF (CG2.LT.0.0.OR.CG2.GT.1.0) THEN
      WRITE(MESSAGESTRING,851)
      call logfromfortran(FATAL_LEVEL,MESSAGESTRING)
 851  FORMAT(1H0,10X,'ERROR: THE ROUTING PARAMETER #2 MUST BE LESS THAN 
     &OR EQUAL TO ONE.')
      ENDIF
      IF (CG3.LT.0.0.OR.CG3.GT.1.0) THEN
      WRITE(MESSAGESTRING,852)
      call logfromfortran(FATAL_LEVEL,MESSAGESTRING)
 852  FORMAT(1H0,10X,'ERROR: THE AFI DECAY PARAMETER MUST BE GREATER 
     &THAN OR EQUAL TO ZERO AND LESS THAN OR EQUAL TO ONE.')
cav      CALL ERROR
cav      IERR=1.0
      ENDIF
      IF((KG1.LT.0.OR.KG1.GT.1.0).OR.(KG2.LT.0.0.OR.KG2.GT.1.0)) THEN
      WRITE(MESSAGESTRING,854)
      call logfromfortran(FATAL_LEVEL,MESSAGESTRING)
 854  FORMAT(1H0,10X,'ERROR: OUTFLOW COEFS MUST BE GREATER THAN OR 
     &EQUAL TO ZERO AND LESS THAN OR EQUAL TO ONE.')
cav      IERR=1.0
cav      CALL ERROR
      ENDIF
      IF(KG1.GT.KG2) THEN
      WRITE(MESSAGESTRING,856)
      call logfromfortran(FATAL_LEVEL,MESSAGESTRING)
 856  FORMAT(1HO,10X,'ERROR MAX OUTFLOW COEF MUST BE GREATER THAN OR 
     &EQUAL TO MIN COEF')
cav      CALL ERROR
cav      IERR=1.0
      ENDIF
      IF(AFI.LT.0.0) THEN
      WRITE(MESSAGESTRING,858)
      call logfromfortran(FATAL_LEVEL,MESSAGESTRING)
 858  FORMAT(1HO,10X,'ERROR: ANTECEDENT FLOW INDEX CAN NOT BE LESS 
     &THAN ZERO')
cav      CALL ERROR
cav      IERR=1.0
      ENDIF
      IF(STORAGE.LT.0.0) THEN
      WRITE(MESSAGESTRING,860)
      call logfromfortran(FATAL_LEVEL,MESSAGESTRING)
 860  FORMAT(1HO,10X,'STORAGE CAN NOT BE LESS THAN ZERO')
cav      CALL ERROR
cav      IERR=1.0
      ENDIF
C.........................................................
C     CHECK IF OKAY TO LOAD VALUES IN PO().
cav      CALL CHECKP(LPO,LEFTP,IERR)
C     CHECK IF OKAY TO LOAD VALUES IN CO().
cav      CALL CHECKC(LCO,LEFTP,IERR)
cav 900  IF (IERR.EQ.1) NOFILL=1
cav      IF (NOFILL.EQ.1) THEN
cav      WRITE(MESSAGESTRING,902)
cav      call logfromfortran(FATAL_LEVEL,MESSAGESTRING)
cav 902  FORMAT(1H0,10X,'**GLACIER OPERATION WILL BE IGNORED',
cav     1' BECAUSE OF THE PRECEEDING ERRORS.')
cav      CALL WARN
cav      RETURN
cav      ENDIF
C.........................................................
C     LOAD INFORMATION INTO PO().
      PO(1)=IVER+0.01
      PO(2)=RMID(1)
      PO(3)=RMID(2)
      PO(4)=RMTYPE
      PO(5)=GOID(1)
      PO(6)=GOID(2)
      PO(7)=GOTYPE
      PO(8)=IDT+0.01
      PO(9)=CG1
      PO(10)=CG2
      PO(11)=CG3
      PO(12)=KG1
      PO(13)=KG2
C     ONLY LOAD AFI IF IVER = 2
      IF (IVER.EQ.2) THEN
      PO(14)=AFID(1)
      PO(15)=AFID(2)
      PO(16)=AFTYPE
      ELSE
      LPO=13
      ENDIF
      CO(1)=STORAGE
      CO(2)=AFI
      IUSEP=LPO
      IUSEC=LCO

C................................................
C     CHECK FOR DEBUG OUTPUT
cav      IF (IBUG.EQ.0) GOTO 908
C................................................
C     DEBUG OUTPUT
      WRITE(MESSAGESTRING,904)
      call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
 904  FORMAT(1H0,'GLACIER DEBUG--CONTENTS OF PO ARRAY.')
      WRITE(MESSAGESTRING,905)  (PO(I),I=1,IUSEP)
      call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
 905  FORMAT(1H0,13(1X,F8.2))
      WRITE(MESSAGESTRING,906) (PO(I),I=1,IUSEP)
      call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
 906  FORMAT(1H0,13(4X,A4))
      WRITE(MESSAGESTRING,907) (CO(I),I=1,IUSEC)
      call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
 907  FORMAT(1H0,'CARRYOVER=',2(1X,F8.2))
C.................................................
      WRITE(MESSAGESTRING,909)
 909  FORMAT(1H0,'**PIN 56 EXITED')  
      call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      
      CLOSE(IN)
      RETURN
      END
