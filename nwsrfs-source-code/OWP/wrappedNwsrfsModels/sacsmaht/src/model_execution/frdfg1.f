CP 03/09subroutine frdfg1(idt,supm,slwm,state,parfrz1,parfrz2,parfrz3)
      subroutine frdfg1(supm,slwm)
C.......................................
C     THIS SUBROUTINE READS 'SAC-SMA ' INPUT CARDS RELATED TO FROZEN
C     GROUND.

      INCLUDE 'flogm'

cc      PARAMETER (ZBOT = -3.0)
      PARAMETER (T0 = 273.16)
      
      real zsoil(8) !CP, state(6)

C     COMMON BLOCKS
      dimension taid(2),weid(2),fiid(2),shid(2)
      common/pass10/par(20),adj(2),state(6)
      common/fpmfg1/ itta,fgpm(15),iver,ifrze,ixrc,nsoillayer
      common/fsmco1/ uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,fgco(6),rsm(7),
     +               ppx,ppsc,pta,pwe,psh,tsoil(8),smc(8),sh2o(8)
cv07     +               ppx,ppsc,pta,pwe,psh,tsoil(8)     
      common/finfg0/ nsinp,zs(8),ts(8),fgco0(6),pta0,pwe0,psh0
      COMMON/FRDSTFG/SMAX,PSISAT,BRT,SWLT,QUARTZ,STYPE,NUPL,
     +               NSAC,RTUP,RTLW,DZUP,DZLOW
cc      COMMON/FRZCNST/ FRST_FACT,CKSOIL,ZBOT
      COMMON/FRZCNST/ FRST_FACT,ZBOT
      COMMON/TSLINT/DSINT(10),TSINT(10),NDSL,SWINT(10),SWHINT(10) !,NINTW 
      COMMON/IONUM/IN,IPR,IPU

c Initialize few frozen ground constant to pass through common statement:
c FRST_FACT is a factor to reduce frozen soil moisture to match closer
c              Eric's temperature index;
c CKSOIL is a Kulik's parameter of frozen ground conductivity reduction
c ZBOT is a depth of the bottom bound of the soil profile 
cc      DATA FRST_FACT/5.0/, CKSOIL/8.0/, ZBOT/-3.0/
cc      DATA FRST_FACT/5.0/, ZBOT/-3.0/: tested Root with -2.0 
      DATA FRST_FACT/5.0/, ZBOT/-2.5/      
      
      NSL=8
      NSOIL=0
        
CVK  DEFAULT STATE VALUES
      DO I=1,NSL
       ZSOIL(I)=-999.
       TSOIL(I)=-999.
      ENDDO
        
cVK 06/07 Skip frozen ground, but simulate soil moisture
CP      if(iver .gt. 1) then
C.......................................
C     READ FROZEN GROUND CARD NUMBER 1.
CP      READ (IN,900) TAID,TATYPE,ITTA,WEID,WETYPE,ITWE,FIID,ITFI
CP  900 FORMAT (2X,2A4,1X,A4,3X,I2,17X,2A4,1X,A4,3X,I2,2X,2A4,3X,I2)
CP      IF ((ITTA/IDT)*IDT .NE. ITTA) then
CP       WRITE (66,901) TAID,TATYPE,ITTA,IDT
CP  901  FORMAT (1H0,10X,49H**ERROR** THE TIME INTERVAL OF TIME SERIES (I.D
CP     +.=,2A4,2X,5HTYPE=,A4,2X,I2,1X,6HHOURS),/16X,
CP     +39HIS NOT A MUTIPLE OF THE TIME INTERVAL (,I2,1X,
CP     +29HHOURS) OF THE RAIN+MELT DATA.)
CP      endif

cVK 06/07      IF(IVER .GT. 1) THEN
CP       READ(IN,900) SHID,SHTYPE,ITSH
CP  109  IF ((ITSH/IDT)*IDT .NE. ITSH) then
CP        WRITE (66,901) SHID,SHTYPE,ITSH,IDT
CP        stop
CP       endif 
cVK 06/07      ENDIF
CP       endif 

C     READ FROZEN GROUND CARD NUMBER 2.
CVK  ARRAY FGPM IS INCREASD BY 5 VARIABLES
      READ (IN,903) (FGPM(I),I=1,10)
  903 FORMAT (10F5.0)
      
C     READ FROZEN GROUND CARD NUMBER 3 --INITIAL CARRYOVER.
c07      READ (IN,903) FGCO,PTA,PWE,PSH
      READ (IN,904) FGCO,PTA,PWE,PSH      

cvk 11/07
cvk 11/07 assign default positive air T just to initialize smc
      if(iver .lt. 1) PTA = 10.0
cvk 11/07      

CVK  SET DEFAULT SNOW DEPTH ASSUMING DENSITY =0.2
      IF(PSH .EQ. 0.) PSH=0.1*PWE/0.2
      
CVK.................................................
cVK 06/07      IF(IVER .GT. 1) THEN
CP 03/09 NSINP=FGPM(7)+0.01
c07       IF(NSINP .LT. 0 .OR. NSINP .GT. 8) THEN

      IF(NSINP .LT. 0 .OR. NSINP .GT. 7) THEN       
        WRITE(MESSAGESTRING,906) NSINP
  906   FORMAT(39H **ERROR** MORE THAT 7 SOIL LAYERS, NS=,I4) 
        call logfromfortran(FATAL_LEVEL, MESSAGESTRING)
CP 03/09stop
      ENDIF 

cvk  to perform initialization of soil moisture and temperaturee at soil layers
      tsoil(1) = -999.
      smc(1) = -1.
       
CVK  READ CARD NUMBER 2A (LAYER DEPTHS) IF NSINP NE 0.
      IF(NSINP .GT. 0) THEN
        READ(IN,904) (ZS(I),I=1,NSINP)
CP        WRITE(66,904) (ZS(I),I=1,NSINP)

CVK  READ CARD NUMBER 3A (SOIL TEMP.) IF NSINP NE 0
        READ(IN,904) (TS(I),I=1,NSINP)
cv07  input smc & sh2o states if desired; smc(1)=-1. means no states in input
        read(IN,904) (smc(i),i=1,nsinp)
        read(IN,904) (sh2o(i),i=1,nsinp)
      ENDIF 
c07  904 FORMAT(10F6.3)
  904 FORMAT(16F10.3)

cCP  READ CARD NUMBER 4 (USER DEFINED NUMBER SOIL LAYER)
      read(IN,905) NDSL
  905 FORMAT(3x,I2)    
cCP  READ CARD NUMBER 4A (USER DEFINED DEPTHS) IF NDSL NE 0  
      IF ( NDSL .gt. 0 ) then
        read(IN,903) (dsint(i), i=1, ndsl)
      END IF 
           
CVK---------------------------------------------------------------------------
CVK  SET DEFAULT NEW FROZEN GROUND PARAMETERS 
CVK  SOIL TEXTURE
       IF(FGPM(1) .LT. 0.) FGPM(1)=4.0
CVK  BOTTOM BOUNDARY (3m) SOIL TEMPERATURE (ASSUMED = CLIMATE ANNUAL AIR TEMP)
       IF(FGPM(2) .LT. 0.) THEN
        FGPM(2)=12.5+T0
       ELSE
        FGPM(2)=FGPM(2)+T0
       ENDIF  
CVK  RESIDUE LAYER POROSITY
       IF(FGPM(3) .LT. 0.) FGPM(3)=0.58
CVK  PARAMETER THAT ACCOUNTS FOR ICE EFFECT ON PERC./INFILTR. USUALLY CONST=8
       IF(FGPM(4) .LT. 0.) FGPM(4)=8.
CVK  THERE ARE TWO OTHER POTENTIAL PARAMETERS THAT ACCOUNT FOR 
CVK  STATISTICAL PROPERTY OF ICE DISTRIBUTION INDUCED BY IMPERMEABLE SOIL LAYER: 
CVK   CRITICAL ICE CONTENT ABOVE WHICH SOIL BECOMES IMPERMEABLE,
CVK   ICE DISTRIBUTION PARAMETER (ASSUMES GAMMA DISTRIBUTION)
CVK  USUALLY THEY NOT NEEDED. DEFAULT OPTION EXCLUDES THESE EFFECT. 
       IF(FGPM(5) .EQ. 0.) FGPM(5)=-1.
       IF(FGPM(6) .LE. 0.) FGPM(6)=6.
cVK 06/07      ENDIF
c new addition: transfer frz_parameters to SAC parameters array (PAR) 
CP    parfrz1=fgpm(4)
CP    parfrz2=fgpm(5)
CP    parfrz3=fgpm(6)
      par(18)=fgpm(4)
      par(19)=fgpm(5)
      par(20)=fgpm(6)
CVK----------------------------------------------------------------------------

cVK 06/07      IF(IVER .GT. 1) THEN
CVK  DEFINE SOIL LAYERS & STATES
       CALL SOILPAR1(FGPM(1),SUPM,SLWM,SMAX,PSISAT,BRT,SWLT,
     +      QUARTZ,STYPE,NSOIL,NUPL,NSAC,ZSOIL,RTUP,RTLW)
       DZUP=ZSOIL(1)-ZSOIL(NUPL)
       DZLOW=ZSOIL(NUPL)-ZSOIL(NSAC)
       
       IF(NSINP .GT. 0) THEN
CVK  IF STATES ARE IN INPUT CARD ZS(1) SHOULD = -ZSOIL(1),
CVK  AND TS(1) SHOULD = TSOIL(1).
        TSOIL(1)=TS(1)
c vk change07
c07        ZD=ZSOIL(1)-ZSOIL(NSOIL)-ZS(NSINP)

C lc commented: the depths read in (zs) are negative     ZD=-ZSOIL(NSOIL)-ZS(NSINP)         
	   ZD=ZS(NSINP)-ZSOIL(NSOIL)
CVK  IF INPUT SOIL DEPTH LESS SOIL DEFINED DEPTH, USE 
CVK  A BOTTOM BOUNDARY SOIL TEMPERATURE TO INTERPOLATE
        IF(ABS(ZD)-0.001 .GT. 0.) THEN
C lc commented	IF(ZD .GT. 0.) THEN
         TS(NSINP+1)=FGPM(2)-T0
c07         ZS(NSINP+1)=ZBOT
C lc commented, put back original: we expect zs to be negative    ZS(NSINP+1)=-ZBOT 
            ZS(NSINP+1)=ZBOT      
        ENDIF
         
         
	DO I=2,NSOIL
c vk change07
c07         ZX=ZSOIL(1)-ZSOIL(I)
         ZX=-ZSOIL(I)

C lc add code to handle reading in states produced by model
c this is when calculated (zx) should be exactly same as read in (zs are negative)
	  
	  diff=ABS(zs(i)) - ABS(zx)	  
	  IF(diff.lt. 0.001)then
	     TSOIL(I) = TS(i)
	     goto 110
	  endif
	 
c lc code below is not supportd yet (7/09);
c it is for case where temp and depth states read in don't match what was previously saved
c07         DO J=1,NSINP
         DO J=1,NSINP+1         
c vk change07
c07          IF(ZX .LE. ZS(J))
c07     +    TSOIL(I)=TS(J-1)+(TS(J)-TS(J-1))*(ZX-ZS(J-1))/(ZS(J)-ZS(J-1))
           

	  IF(ZX .LE. ZS(J)) then
           TSOIL(I)=TS(J-1)+(TS(J)-TS(J-1))*(ZX-ZS(J-1))/(ZS(J)-ZS(J-1))
           goto 110
          endif
         ENDDO
c vk change07
c07        ENDDO
110     ENDDO
       ELSE
CVK  IF STATES ARE NOT IN INPUT CARD INTERPOLATE BETWEEN AIR
CVK  AND BOTTOM BOUNDARY SOIL TEMPERATURE
CVK  AIR TEMPERATURE AND SNOW WATER EQUIVALENT STATES SHOULD BE
CVK  IN INPUT DECK. 
        ZS(1)=0.
        TS(1)=PTA
CVK  ASSUMED SNOW CONDUCTIVITY IS LESS THAN SOIL CONDUCTIVITY
CVK  BY RATIO OF INVERSE SNOW DENSITY =0.2
        SH=0.01*PSH/0.2
        ZS(2)=-ZBOT+SH
        TS(2)=FGPM(2)-T0
        DO I=1,NSOIL
         IF(I .EQ. 1) THEN
          ZX=-0.5*ZSOIL(1)+SH
         ELSE
          ZX=-0.5*(ZSOIL(I-1)+ZSOIL(I))+SH
         ENDIF
         TSOIL(I)=TS(1)+(TS(2)-TS(1))*ZX/ZS(2)
        ENDDO
      ENDIF
cc       TSOIL(NSOIL+1)=FGPM(2)
       FGPM(7)=NSOIL
       DO I=1,NSOIL
	FGPM(7+I)=ZSOIL(I)
       ENDDO
cc       FGPM(7+NSOIL+1)=ZBOT    

c  save initial frozen ground variables
       do i=1,6
        fgco0(i)=fgco(i)
       enddo
       pta0=pta
       pwe0=pwe
       psh0=psh

c Generate frozen states if not defined 
c in optimization mode fstfg1 will be called in loop from frdfg0
cv07     call fstfg1(fgco,tsoil,fgpm,state,iver)
       call fstfg1(fgco,tsoil,fgpm,state,iver,smc,sh2o)
            
cVK 06/07      ENDIF
cCP 03/09 Close param file
       CLOSE(IN)

      RETURN
      END
