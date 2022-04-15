cav      SUBROUTINE EX56(PO,CO,RM,GO,AF, LD3)
      SUBROUTINE EX56(PO,CO,RM,GO,AF)
C...................................................
C     THIS IS THE EXCUTION ROUTINE FOR THE GLACIER
C     RUNOFF MODEL
C     Inputs:
C     PO
C     CO
C     RM: 	input  - Rain melt
C     LD3: 	input -  
C     GO: 	output - Glacier output
C     AF: 	output - Antecedent Flow 
C...................................................
C
      DIMENSION PO(*),CO(*),RM(*),GO(*),CT(2),AF(*)
      CHARACTER*8  SNAME
      REAL STORAGE,DSTORE,INFLOW,AFI,NUM,KG1,KG2,KG
cav      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
cav      COMMON /FCTIME/IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,NOW(5),
cav     &LOCAL,NOUTZ,NOUTDS,NLSTZ,IDA,IHR,LDA,LHR,IDADAT
cav      COMMON /FCARY/ IFILLC,NCSTOR,ICDAY(20),ICHOUR(20)
      
      INCLUDE 'flogm'
      INCLUDE 'fcary'
      INCLUDE 'fctime'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob90/ohd/ofs/src/fcst_ex/RCS/ex56.f,v $
     . $',                                                             '
     .$Id: ex56.f,v 1.4 2008/11/06 15:30:48 shens Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA  SNAME / 'EX56    ' /
C....................................................
      IF (FEWSDEBUG.GE.1) THEN  
         WRITE(MESSAGESTRING,10)
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 10      FORMAT(1H0,'**ENTER EX56')
      ENDIF
C     TRACE LEVEL=1,DEBUG FLAG=IBUG
cav      CALL FPRBUG(SNAME,1,56,IBUG)
C....................................................
C     CONTROL STATEMENTS
      IVER=PO(1)
      IDT = PO(8)
      RIDT=IDT
      IJH=(IDA-1)*24+IHR
      LJH=(LDA-1)*24+LHR
      NPD=((LJH-IJH)/IDT)+1
      IHD=(IDADAT-1)*24+IDT
cav      IOFF=(IJH-IHD)/IDT
cav NO offset needed for intial start time
      IOFF = 0
      CG1=PO(9)
      CG2=PO(10)
      CG3=PO(11)
      KG1=PO(12)
      KG2=PO(13)   
      IF(IVER.EQ.1)IAF=O
      IF(IVER.EQ.2)IAF=1
      STORAGE=CO(1)
      AFI=CO(2)
      IC=1
      LCO=2
C....................................................
C     ADJUST PARAMETERS FOR TIME STEP
      KG1=1-((1.0-KG1)**(RIDT/24))
      KG2=1-((1.0-KG2)**(RIDT/24))
      CG3=CG3**(RIDT/24) 
C....................................................
C     BEGIN MAIN COMPUTATION LOOP

      DO 200 N=1,NPD
        INFLOW=RM(IOFF+N)
        AFI=CG3*AFI+RM(IOFF+N)
        NUM=EXP(CG1+CG2*AFI)
	
        FAFI=NUM/(1+NUM)
        KG=KG1+(KG2-KG1)*FAFI	
        IF(IAF.EQ.1)AF(IOFF+N)=FAFI
CAV   LD3 will not be passed in from input (CHPS version)
CAV   ignore checking for ioff+n limit
CAV        IF((IAF.EQ.1) .and. ((IOFF+N) .LT. LD3)) AF(IOFF+N)=FAFI
        GO(IOFF+N)=KG*STORAGE	
        DSTORE=INFLOW-GO(IOFF+N)
        STORAGE=STORAGE+DSTORE
        CT(1)=STORAGE
        CT(2)=AFI
C       SAVE CARRYOVER IF REQUESTED
cav        IF(IFILLC.EQ.0)GOTO 200
        IF(NCSTOR.EQ.0) GOTO 200
        IF (IC.GT.NCSTOR) GOTO 200
        KJH=IJH+(N-1)*IDT
        KDA=((KJH-1)/24)+1
        KHR=KJH-((KDA-1)*24)
        IF ((KDA.EQ.ICDAY(IC)).AND.(KHR.EQ.ICHOUR(IC))) GOTO 170

        GOTO 200
cav 170    CALL FCWTCO(KDA,KHR,CT,LCO)
cav double check these ! comment out fcwtco to fix compiling err
 170       IC=IC+1
 200  CONTINUE
C     END OF COMPUTATIONAL LOOP
C...................................
C     UPDATE CO ARRAY IF REQUESTED
cav      IF (IFILLC.EQ.0) GOTO 290
      CO(1)=STORAGE
      CO(2)=AFI
C...................................
C     DEBUG OUTPUT
cav 290  IF (IBUG.EQ.0) GOTO 295
      I1=IOFF+1
      I2=IOFF+NPD
      if(I2 > 100) I2 = 100
      IF (FEWSDEBUG.GE.1) THEN   
      WRITE(MESSAGESTRING,901)
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 901  FORMAT(1H0,'RAIN MELT INPUT TIME SERIES')
      WRITE(MESSAGESTRING,903) (RM(I),I=I1,I2)
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 903  FORMAT(1H0,15F8.2)
      WRITE(MESSAGESTRING,905)
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 905  FORMAT(1H0,'GLACIER OUTPUT TIME SERIES')
      WRITE(MESSAGESTRING,907) (GO(I),I=I1,I2)
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 907  FORMAT(1H0,15F8.2)
       if(IAF .EQ.1 ) THEN
         WRITE(MESSAGESTRING,906)
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 906     FORMAT(1H0,'FAFI OUTPUT TIME SERIES')
         WRITE(MESSAGESTRING,908) (AF(I),I=I1,I2)
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 908     FORMAT(1H0,15F8.2)
       ENDIF
      ENDIF

C................................................
cav 295  IF (ITRACE.GE.1) 
      IF (FEWSDEBUG.GE.1) THEN  
         WRITE(MESSAGESTRING,909)
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 909     FORMAT(1H0,'**EXIT EX56')
      ENDIF
      RETURN
      END
