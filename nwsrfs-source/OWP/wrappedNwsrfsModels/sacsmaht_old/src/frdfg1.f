      subroutine frdfg1(idt,supm,slwm,state,parfrz1,parfrz2,parfrz3)
C.......................................
C     THIS SUBROUTINE READS 'SAC-SMA ' INPUT CARDS RELATED TO FROZEN
C     GROUND.

cc      PARAMETER (ZBOT = -3.0)
      PARAMETER (T0 = 273.16)
      
      real zsoil(8),state(6)

C     COMMON BLOCKS
      dimension taid(2),weid(2),fiid(2),shid(2)
      common/fpmfg1/ itta,fgpm(15),iver,ifrze
      common/fsmco1/ uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,fgco(6),rsm(7),
     +               ppx,ppsc,pta,pwe,psh,tsoil(8),smc(8),sh2o(8)
      common/finfg0/ nsinp,zs(8),ts(8),fgco0(6),pta0,pwe0,psh0
      COMMON/FRDSTFG/SMAX,PSISAT,BRT,SWLT,QUARTZ,STYPE,NUPL,
     +               NSAC,RTUP,RTLW,DZUP,DZLOW
cc      COMMON/FRZCNST/ FRST_FACT,CKSOIL,ZBOT
      COMMON/FRZCNST/ FRST_FACT,ZBOT
      
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
        
C.......................................
C     READ FROZEN GROUND CARD NUMBER 1.
      READ (55,900) TAID,TATYPE,ITTA,WEID,WETYPE,ITWE,FIID,ITFI
  900 FORMAT (2X,2A4,1X,A4,3X,I2,17X,2A4,1X,A4,3X,I2,2X,2A4,3X,I2)

      IF(IVER .GT. 1) THEN
       READ(55,900) SHID,SHTYPE,ITSH
  109  IF ((ITSH/IDT)*IDT .NE. ITSH) then
        stop
       endif 
      ENDIF 

C     READ FROZEN GROUND CARD NUMBER 2.
CVK  ARRAY FGPM IS INCREASD BY 5 VARIABLES
      READ (55,903) (FGPM(I),I=1,10)
  903 FORMAT (10F5.0)
C     READ FROZEN GROUND CARD NUMBER 3 --INITIAL CARRYOVER.
c07      READ (55,903) FGCO,PTA,PWE,PSH
      READ (55,904) FGCO,PTA,PWE,PSH
      IF(PSH .EQ. 0.) PSH=0.1*PWE/0.2
CVK.................................................
      IF(IVER .GT. 1) THEN
       NSINP=FGPM(7)+0.01
       IF(NSINP .LT. 0 .OR. NSINP .GT. 7) THEN       
        stop
       ENDIF 
       tsoil(1) = -999.
       smc(1) = -1.
CVK  READ CARD NUMBER 2A (LAYER DEPTHS) IF NSINP NE 0.
       IF(NSINP .GT. 0) THEN
        READ(55,904) (ZS(I),I=1,NSINP)
CVK  READ CARD NUMBER 3A (SOIL TEMP.) IF NSINP NE 0
        READ(55,904) (TS(I),I=1,NSINP)
cv07  input smc & sh2o states if desired; smc(1)=-1. means no states in input
        read(55,904) (smc(i),i=1,nsinp)
        read(55,904) (sh2o(i),i=1,nsinp)
       ENDIF 
c07  904 FORMAT(10F6.3)
  904 FORMAT(16F10.3)
           
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
      ENDIF
c new addition: transfer frz_parameters to SAC parameters array (PAR) 
      parfrz1=fgpm(4)
      parfrz2=fgpm(5)
      parfrz3=fgpm(6)
CVK----------------------------------------------------------------------------

      IF(IVER .GT. 1) THEN
CVK  DEFINE SOIL LAYERS & STATES
       CALL SOILPAR1(FGPM(1),SUPM,SLWM,SMAX,PSISAT,BRT,SWLT,
     +      QUARTZ,STYPE,NSOIL,NUPL,NSAC,ZSOIL,RTUP,RTLW,66)
       DZUP=ZSOIL(1)-ZSOIL(NUPL)
       DZLOW=ZSOIL(NUPL)-ZSOIL(NSAC)
       
       IF(NSINP .GT. 0) THEN
CVK  IF STATES ARE IN INPUT CARD ZS(1) SHOULD = -ZSOIL(1),
CVK  AND TS(1) SHOULD = TSOIL(1).
        TSOIL(1)=TS(1)
c vk change07
c07        ZD=ZSOIL(1)-ZSOIL(NSOIL)-ZS(NSINP)
        ZD=-ZSOIL(NSOIL)-ZS(NSINP)
CVK  IF INPUT SOIL DEPTH LESS SOIL DEFINED DEPTH, USE 
CVK  A BOTTOM BOUNDARY SOIL TEMPERATURE TO INTERPOLATE
        IF(ZD .GT. 0.) THEN
         TS(NSINP+1)=FGPM(2)-T0
c07         ZS(NSINP+1)=ZBOT
         ZS(NSINP+1)=-ZBOT         
        ENDIF

        DO I=2,NSOIL
c vk change07
c07         ZX=ZSOIL(1)-ZSOIL(I)
         ZX=-ZSOIL(I)
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
            
      ENDIF
      RETURN
      END
