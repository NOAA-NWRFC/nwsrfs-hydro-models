C MODULE COX24
C-----------------------------------------------------------------------
C
      SUBROUTINE COX24(POLD,COLD,PONEW,CONEW)
C
C****************************************************
C   THIS IS THE CARRYOVER TRANSFER SUBROUTINE FOR
C   THE API-CONT OPERATION
C****************************************************
C INITIALLY WRITTEN BY - ERIC ANDERSON - HRL, JUNE 1990
C****************************************************
C
      DIMENSION POLD(1),COLD(1),PONEW(1),CONEW(1)
cCP   DIMENSION SNAME(2)
C
C****************************************************
C   COMMON BLOCKS
C****************************************************
C
cCP   INCLUDE 'common/ionum'
cCP   INCLUDE 'common/fdbug'
      INCLUDE 'flogm'

cCP   COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox24.f,v $
     . $',                                                             '
     .$Id: cox24.f,v 1.2 1999/07/06 14:53:30 page Exp $
     . $' /
C    ===================================================================
C
cCP   DATA SNAME/4HCOX2,4H4   /
C
C
cCP   CALL FPRBUG(SNAME,1,24,IBUG)
      WRITE(MESSAGESTRING, *) 'API-CONT CARRYOVER TRANSFER: '
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)

C
C   GET CONTROL VARIABLES
      IVER=PONEW(1)
      LPOLD=POLD(28)
      LPONEW=PONEW(28)
      LCO=7
      IVOPTO=POLD(14)
      IVOPTN=PONEW(14)
      IFRZEO=POLD(24)
      IFRZEN=PONEW(24)
      LRGPMO=POLD(27)
      LRGPMN=PONEW(27)
      BFPKO=POLD(LRGPMO)
      BFPKN=PONEW(LRGPMN)
      BFIMO=POLD(LRGPMO+2)
      BFIMN=PONEW(LRGPMN+2)
      LRSPMN=PONEW(26)
      SMIXN=PONEW(LRSPMN+6)
      APIXN=PONEW(LRSPMN+9)
      LRSPMO=POLD(26)
      SMIXO=POLD(LRSPMO+6)
C
C*****************************************************
C   IF DEBUG ON, PRINT  POLD,COLD AND PONEW
C*****************************************************
C
cCP   IF (IBUG.EQ.0) GO TO 100
      IF (FEWSDEBUG.EQ.0) GO TO 100
cCP   WRITE(IODBUG,900) LPOLD,LCO
      WRITE(MESSAGESTRING,900) LPOLD
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
  900 FORMAT(' CONTENTS OF OLD PO ARRAYS FOR API-CONT',
     -  5X,'NUMBER OF VALUES--PO=',I3)

C
cCP   WRITE(IODBUG,901) (POLD(I),I=1,LPOLD)
cCP   WRITE(IODBUG,901) (COLD(I),I=1,LCO)
      WRITE(MESSAGESTRING,901) (POLD(I),I=1,LPOLD)
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
C
      WRITE(MESSAGESTRING,906) (COLD(I),I=1,LCO)
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
   
  906 FORMAT(' CONTENTS OF OLD CO ARRAY',/6X,7F8.3)
  901 FORMAT(1H0,15F8.3)

C
cCP   WRITE(IODBUG,902) LPONEW
      WRITE(MESSAGESTRING,902) LPONEW
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
  902 FORMAT(' CONTENTS OF NEW PO ARRAY',5X,'NUMBER OF ',
     -'VALUES=',I3)
C
cCP   WRITE(IODBUG,901) (PONEW(I),I=1,LPONEW)
      WRITE(MESSAGESTRING,901) (PONEW(I),I=1,LPONEW)
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
C
C****************************************************
C  BEGIN CARRYOVER TRANSFER
C****************************************************
C
  100 CONEW(1)=COLD(1)
      IF (CONEW(1).GT.APIXN) CONEW(1)=APIXN
      CONEW(2)=SMIXN-(SMIXO-COLD(2))
      IF (CONEW(2).LT.0.0) CONEW(2)=0.0
      CONEW(3)=(((1.0-BFPKO)*(1.0+BFIMO*COLD(4)))/
     -  ((1.0-BFPKN)*(1.0+BFIMN*COLD(4))))*COLD(3)
      CONEW(4)=COLD(4)
C
C*****************************************************
C  FIRST QUADRANT VARIATION OPTION
C*****************************************************
C
      IF (IVOPTN.GT.0) GO TO 110
      CONEW(5)=0.0
      GO TO 150
C
  110 IF (IVOPTN.NE.IVOPTO) GO TO 120
      NADD=0
      IF (IVER.EQ.2) NADD=1
      CONEW(5)=COLD(5)
      AETIX=PONEW(LRSPMN+17+NADD)
      AETIN=PONEW(LRSPMN+18+NADD)
      IF (CONEW(5).GT.AETIX) THEN
cCP      WRITE (IPR,905) CONEW(5),'GREATER THAN',AETIX
         WRITE(MESSAGESTRING,905) CONEW(5),'GREATER THAN',AETIX
	 call logfromfortran(WARNING_LEVEL, MESSAGESTRING)
905   FORMAT ('0**WARNING** IN COX24 - ',
     *   'VALUE OF THE NEW CARRYOVER VALUE (',F5.2,') ',
     *   'IS ',A,' THE MAXIMUM VALUE AND WILL BE SET TO ',F5.2,'.')
cCP      CALL WARN
         CONEW(5)=AETIX
         ENDIF
      IF (CONEW(5).LT.AETIN) THEN
cCP      WRITE (IPR,905) CONEW(5),'LESS THAN',AETIN
cCP      CALL WARN
         WRITE(MESSAGESTRING,905) CONEW(5),'LESS THAN',AETIN
         call logfromfortran(WARNING_LEVEL, MESSAGESTRING)
         CONEW(5)=AETIN
         ENDIF
      GO TO 150
C
cCP   120 WRITE(IPR,903)
  120 WRITE(MESSAGESTRING,903)
      call logfromfortran(WARNING_LEVEL, MESSAGESTRING)
  903 FORMAT('0**WARNING** NO CARRYOVER TRANSFER FOR ',
     - 'AEI OR ATI WHEN CHANGING FIRST QUADRANT OPTION. ',
     - 'USER INPUT VALUE IS USED.')
cCP   CALL WARN
C
C*****************************************************
C  FROZEN GROUND
C*****************************************************
C
  150 IF (IFRZEN.GT.0) GO TO 160
      CONEW(6)=32.0
      CONEW(7)=0.0
      GO TO 190
C
  160 IF (IFRZEO.EQ.0) GO TO 190
      CONEW(6)=COLD(6)
      CONEW(7)=COLD(7)
C
C*****************************************************
C  CHECK FOR DEBUG OUTPUT
C*****************************************************
C
cCP   190 IF (IBUG.EQ.0) GO TO 195
  190 IF (FEWSDEBUG.EQ.0) GO TO 195
cCP   WRITE(IODBUG,904) (CONEW(I),I=1,LCO)
      WRITE(MESSAGESTRING,904) (CONEW(I),I=1,LCO)
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
  904 FORMAT(' CONTENTS OF NEW CO ARRAY',/6X,7F8.3)
C
  195 RETURN
      END
