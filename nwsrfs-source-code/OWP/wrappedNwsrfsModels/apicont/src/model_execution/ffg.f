C MEMBER EX32
C  (from old member FCEX32)
C VERSION 1.13
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/08/95.14:08:22 BY $WC20SV
C
C  PGM:  EX32 (PO,PS,CS,PR,CR,TASN,TARR,RSTS,FFG)
C
C   IN: PO     .... PARAMETRIC DATA FROM P ARRAY
C   IN: PS     .... PARAMETRIC DATA FOR SNOW OPERATION FROM P ARRAY
C   IN: CS     .... CARRYOVER DATA FOR SNOW OPERATION FROM C ARRAY
C   IN: PR     .... PARAMETRIC DATA FROM RAINFALL-RUNOFF OPERATION
C                     FROM P ARRAY
C   IN: CR     .... CARRYOVER DATA FOR RAINFALL-RUNOFF OPERATION
C                     FROM C ARRAY
C   IN: TASN   .... SNOW MODEL AIR TEMPERATURE ARRAY FROM D ARRAY
C   IN: TARR   .... RAINFALL-RUNOFF MODEL AIR TEMPERATURE ARRAY
C                     FROM D ARRAY
CEA IN: RSTS   .... RAIN-SNOW ELEVATION ARRAY FROM D ARRAY
C   IN: FFG    .... FLASH FLOOD GUIDANCE PARAMETER ARRAY (INTERNAL)
C  =====================================================================
C @PROCESS LVL(77)
C
CP      SUBROUTINE EX32 (PO,PS,CS,PR,CR,TASN,TARR,RSTS,FFG)
      SUBROUTINE FFG (PR, CR, RAIMLT, TARR, IDT, ROFF)
C
C.......................................................................
C  THIS ROUTINE EXECUTES THE FFG OPERATION
C
C.......................................................................
C  INITIALY WRITTEN BY
C        JANICE LEWIS, HYDROLOGIC RESEARCH LAB, OCT 1991
C
C    EXPANDED FOR USE WITH API RAINFALL-RUNOFF OPERATIONS (CONTINUOUS
C    AND EVENT).
C        TIM SWEENEY, HRL                  JAN 1995      VERSION 1.10
C
C  Corrected logic for air temp calculation in snow model when 
c  additional temperature parameters not used, i.e. elevation of temp
c  data same as elevation of area.
c        Tim Sweeney, HRL                  Dec 1996      Version 1.11
C
C  CHANGED SNOW MODEL PORTION TO TAKE INTO ACCOUNT ADDITIONS THAT HAVE
C    BEEN MADE TO SNOW-17 RELATED TO SNOW DEPTH AND SNOWPACK
c    TEMPERATURE.  ALSO MADE SOME CORRECTIONS AND COMMENTS RELATED
C    TO THE SNOW MODEL.
C        Eric Anderson                     Sept 2006
C.......................................................................
C  PRINCIPLE VARIABLES...
C
C  FOR DEFINITION OF VARIABLES IN COMMON BLOCKS, SEE SECTION IX.3.3C
C  OF THE NWSRFS USER'S MANUAL.
C
C     CAESC
C     CR(1)       CURRENT CARRYOVER DATA FROM RAINFALL-RUNOFF MODEL
C     CS(1)       CURRENT CARRYOVER DATA FROM SNOW MODEL
C     CWE
C     DT
C     EPDIST
C     FFG         FLASH FLOOD GUIDANCE PARAMETER ARRAY
C     FFGDSC      FLASH FLOOD GUIDANCE AREA DESCRIPTION
C     IBUG        DEBUG OUTPUT SWITCH, 0 = OFF, 1 = ON
C     ICLOC       STARTING LOCATION OF SNOW MODEL INFO IN FFG ARRAY
C     IDBB        ID OF BASIN BOUNDARY
C     IDFFG       FLASH FLOOD GUIDANCE AREA IDENTIFIER
C     IDAY
C     IDEL
C     IDN
C     IDT
C     IDUR        DURATION FLAG, 0 = 1-, 3-, AND 6-HOUR DURATIONS ONLY,
C                                1 = 12-HOUR DURATION ALSO,
C                                2 = 12- AND 24-HOUR DURATIONS ALSO.
C     IFATAL      FATAL ERROR FLAG, 0 = NO FATAL ERRORS, 1 = ERRORS
C     IFFG        FLASH FLOOD GUIDANCE SWITCH: 0=NO FFG, 1=FFG ONLY,
C                    2=FFG AND REGULAR RUN
C     IFRZE
C     IFUT
C     IHOUR
C     IMN
C     IOUT
C     IPRINT
C     IPTR        STARTING LOCATION OF BASIN PARAMETERS IN PREPROCESSOR
C                   PARAMETRIC DATA BASE
C     IPTRNX      POINTER TO NEXT PARAMETER RECORD OF 'FFG' DATA TYPE
C     IRLOC       STARTING LOCATION OF RAINFALL-RUNOFF MODEL INFO IN FFG
C                   ARRAY
C     ISTAT       BASIN BOUNDARY STATUS CODE, 0 = SUCCESSFUL READ,
C                   2 = RECORD NOT FOUND, ELSE NOT 0 OR 2 UNABLE TO
C                   READ RECORD
C     ITP
C     ITYFFG      FLASH FLOOD GUIDANCE AREA TYPE (FFG)
C     IVOPT
C     ITPE
C     ITSC
C     ITWE
C     KHR         SPECIFIED HOUR FOR TODAY'S DATE, RANGE 1-24
C     KMO,KDA,KYR TODAY'S DATE (MONTH, DAY, YEAR, 2 DIGITS EACH)
C     LTA
C     MFFG        MAXIMUM SIZE OF FLASH FLOOD GUIDANCE PARAMETER ARRAY
C     NCORR       NUMBER OF RAINFALL-RUNOFF MODEL CARRYOVER VALUES
C     NCOSN       NUMBER OF SNOW MODEL CARRYOVER VALUES
CEA                 NOTE: THIS IS THE NUMBER OF SNOW MODEL CARRYOVER
CEA                  VALUES RETAINED IN THE 'FFG' PARAMETER ARRAY IN
CEA                  THE PPPDB - NOT THE NUMBER OF CARRYOVER VALUES
CEA                  ACTUALLY USED BY THE SNOW-17 OPERATION.
C     NDT         NUMBER OF TIME INCREMENTS NEEDED TO AVERAGE THE
C                   TEMPERATURE
CEA               NOTE: NDT IS ACTUALLY THE NUMBER OF PRECIPITATION
CEA                TIME INTERVALS PER TEMPERATURE TIME INTERVAL FOR
CEA                THE SNOW MODEL - IT IS ALWAYS EQUAL TO 1 FOR FFG.
C     NFILL       NUMBER OF FULL WORDS FILL IN FFG ARRAY
C     NOP         NUMBER OF THIS OPERATION (32)
C     NOPRR       NUMBER OF OPERATION ASSIGNED TO RAINFALL-RUNOFF
C                   OPERATION
C     NOPSN       NUMBER OF OPERATION ASSIGNED TO SNOW OPERATION
C     OPNARR2)    OPERATION NAME OF RAINFALL-RUNOFF MODEL
C     OPNASN(2)   OPERATION NAME OF SNOW MODEL
C     OPTYRR(2)   OPERATION TYPE OF RAINFALL-RUNOFF MODEL
C     OPTYSN(2)   OPERATION TYPE OF SNOW MODEL
C     PCOVER
C     PEADJ
C     PGM
C     PO(1)       INPUT PARAMETRIC DATA FROM P ARRAY
C     POSC
C     POWE
C     PPCTS
C     PPX         PRECIPITATION FOR THE CURRENT TIME
C     PR(1)       PARAMETRIC DATA FROM RAINFALL-RUNOFF MODEL
C     PS(1)       PARAMETRIC DATA FROM SNOW MODEL
C     PTA         AIR TEMPERATURE FOR THE CURRENT TIME
C     RO          RUNOFF CORRESPONDING TO PRECIPITATION PPX
C     SNAME(2)    ROUTINE NAME
C     TMPSN       TEMPERATURE FOR THE SNOW OPERATION
C     TSTADT      TIME INTERVAL OF AIR TEMPERATURE TIME SERIES FOR
C                   SNOW MODEL
C     TWE         SIMULATED WATER EQUIVALENT
C     VERS        VERSION NUMBER
C
C
C.......................................................................
C
      CHARACTER*4 SNAME(2)
C.......................................................................
C
C  COMMON BLOCKS
       INCLUDE 'fctime'
       INCLUDE 'flogm'
C.......................................................................
C
      DIMENSION PR(*),CR(*),TARR(*)
      
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob90/ohd/ofs/src/fcst_ffg/RCS/ex32.f,v $
     . $',                                                             '
     .$Id: ex32.f,v 1.6 2006/10/04 12:08:45 hsu Exp $
     . $' /
C    ===================================================================
C
C.......................................................................
C
       DATA SNAME/'FFG','    '/,NOP/32/

C  CONVERSION FACTOR
       CONV = 25.4

C-----------------------------------------------------------------------
C  CHECK TO SEE IF SNOW MODEL IS AVAILABLE
      AESC  = 0.
      TWE   = 0.
      TMPRS = -999.0
C-----------------------------------------------------------------------
C  GET THE RAINFALL-RUNOFF MODEL PARAMETER/CARRYOVER DATA
C-----------------------------------------------------------------------
C.......................................................................
C  API-CONT MODEL (NO. 24)
  140 CALL RCON32(PR,CR,AIADJ,IVOPT,IFRZE,LWE,LSC,IPRINT,ITWE,
     +            ITSC,LTA,ITTA,TAVG)
      PE = 0.
      TMPRR = 0.
      IF (FEWSDEBUG.GT.3) THEN
         WRITE(MESSAGESTRING,*)'IDT: ',IDT,' LHRCPD: ',LHRCPD,
     >                         ' LDACPD: ',LDACPD 
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
      END IF

      FIDT = IDT
      NXHR = LHRCPD + IDT
      NXDA = LDACPD
      IF(NXHR.GT.24) NXDA = NXDA + 1
      IF(NXHR.GT.24) NXHR = NXHR - 24
      CALL MDYH1(NXDA,NXHR,KMO,KDA,KYR,KHR,NOUTZ,NOUTDS,TZCODE)
      
      IF (FEWSDEBUG.GT.3) THEN
         WRITE(MESSAGESTRING,*)'run month ',KMO
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)

         WRITE(MESSAGESTRING,*)'run day ',KDA
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)

         WRITE(MESSAGESTRING,*)'run yr ',KYR
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)

         WRITE(MESSAGESTRING,*)'run hr ',KHR
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)

         WRITE(MESSAGESTRING,*)'run time step ',idt
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)

      END IF
C     
C-----------------------------------------------------------------------
C  API-CONT MODEL (NO. 24)
C.......................................................................
C  FIND AVERAGE AIR TEMPERATURE FOR API-CONT MODEL DURING THIS PERIOD
      IF(LTA.EQ.0) GO TO 325
      IHR1=(LHRCPD/ITTA)*ITTA+ITTA
      IHR2=((LHRCPD+IDT)/ITTA)*ITTA+ITTA
      IS=LHRCPD
      I2=LHRCPD+IDT
      TMPRR=0.
      DO 322 I=IHR1,IHR2,ITTA
        LLTA=NDHR+I/ITTA
        IEND=I
        IF(IEND.GT.I2) IEND=I2
        TMPRR=TMPRR+((IEND-IS)/FIDT)*(TARR(LLTA)*1.8+32.0)
        IS=I
  322 CONTINUE
C.......................................................................
C  COMPUTE PED AND Y FOR THIS PERIOD
C.......................................................................
  325 CALL PEDY32(KMO,KDA,IVOPT,PE,TAVG)
C
      WE = TWE/CONV
      RAIMLT = RAIMLT / CONV !convert precip in MM to INCH

      IF (FEWSDEBUG.GT.3) THEN

      WRITE(MESSAGESTRING,*) '--> RAIMLT = ', RAIMLT
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)

      WRITE(MESSAGESTRING,*)
     >'API-CONT - RAIMLT,RS,PFRS,AI,RG,ROFF,PE,AESC,TMPRR,TAVG,WE,
     >AIADJ: ', RAIMLT,RS,PFRS,AI,RG,ROFF,PE,AESC,TMPRR,TAVG,WE,AIADJ
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)

      WRITE(MESSAGESTRING,*)
     >'       - NXHR,KMO,KDA,KHR,IVOPT,IFRZE,LWE,LSC,IDT: ',
     >NXHR,KMO,KDA,KHR,IVOPT,IFRZE,LWE,LSC,IDT
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)

      END IF

      CALL CAPI32(RAIMLT,RS,PFRS,AI,RG,ROFF,PE,AESC,TMPRR,TAVG,WE,
     +            AIADJ,NXHR,KMO,KDA,KHR,IVOPT,IFRZE,LWE,LSC,IDT)
C
C  CONVERT INCH TO MM
C
      ROFF = ROFF * CONV
      
      WRITE(MESSAGESTRING, 7300)RAIMLT*CONV,ROFF 
      call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
 7300 FORMAT(12X,'RAINE(mm)=',F12.6,5X,'ROFF(mm)=',F12.6,/)

      WRITE(MESSAGESTRING, *)'exit FFG FORTRAN'
      call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)

      RETURN
      END
