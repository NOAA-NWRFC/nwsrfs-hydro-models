      subroutine frdfg0(supm,slwm,state)
C.......................................
C     THIS SUBROUTINE INITIALIZES FROZEN GROUND STATES
C.......................................

      REAL ZS(8),ZSOIL(8),TS(8),state(*)
      common/fpmfg1/ itta,fgpm(15),iver,ifrze
      common/fsmco1/ uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,fgco(6),rsm(7),
     +               ppx,ppsc,pta,pwe,psh,tsoil(8)      
      common/finfg0/ nsinp,zs,ts,fgco0(6),pta0,pwe0,psh0
      COMMON/FRDSTFG/SMAX,PSISAT,BRT,SWLT,QUARTZ,STYPE,NUPL,
     +               NSAC,RTUP,RTLW,DZUP,DZLOW
cc      COMMON/FRZCNST/ FRST_FACT,CKSOIL,ZBOT
      COMMON/FRZCNST/ FRST_FACT,ZBOT
            
      NSOIL=0
CVK  DEFAULT STATE VALUES
      DO I=1,8
       ZSOIL(I)=-999.
       TSOIL(I)=-999.
      ENDDO

      if(iver .gt. 1) then
      

CVK  SET DEFAULT PARAMETER VALUES FOR
CVK  SOIL TEXTURE
cc       IF(FGPM(1) .EQ. 0.) FGPM(1)=4.0
CVK  BOTTOM BOUNDARY (3m) SOIL TEMPERATURE
cc       IF(FGPM(2) .EQ. 0.) FGPM(2)=12.5
CVK  RESIDUE LAYER POROSITY
cc       IF(FGPM(3) .EQ. 0.) FGPM(3)=0.58
cc       TBOT=FGPM(2)

c set initial values of states
      pwe=pwe0
      psh=psh0
CVK  SET DEFAULT SNOW DEPTH ASSUMING DENSITY =0.2
      IF(PSH .EQ. 0.) PSH=0.1*PWE/0.2
      pte=pte0

CVK  DEFINE SOIL LAYERS & STATES
      CALL SOILPAR1(FGPM(1),SUPM,SLWM,SMAX,PSISAT,BRT,SWLT,
cc     +     QUARTZ,STYPE,NSOIL,NUPL,NSAC,ZSOIL,RTUP,RTLW,66)
     +     QUARTZ,STYPE,NSOIL,NUPL,NSAC,ZSOIL,RTUP,RTLW)     
      DZUP=ZSOIL(1)-ZSOIL(NUPL)
      DZLOW=ZSOIL(NUPL)-ZSOIL(NSAC)
cc      ZBOT=FGPM(7+NSOIL+1)
             
      IF(NSINP .GT. 0) THEN
CVK  IF STATES ARE IN INPUT CARD ZS(1) SHOULD =0,
CVK  AND TS(1) SHOULD BE CLOSE TO SOIL SURFACE TEMP.
       TSOIL(1)=TS(1)
       ZD=ZSOIL(1)-ZSOIL(NSOIL)-ZS(NSINP)
CVK  IF INPUT SOIL DEPTH LESS SOIL DEFINED DEPTH, USE 
CVK  A BOTTOM BOUNDARY SOIL TEMPERATURE TO INTERPOLATE
       IF(ZD .GT. 0.) THEN
        TS(NSINP+1)=FGPM(2)
        ZS(NSINP+1)=ZBOT
       ENDIF
       DO I=2,NSOIL
        ZX=ZSOIL(1)-ZSOIL(I)
        DO J=1,NSINP
         IF(ZX .LE. ZS(J))
     +   TSOIL(I)=TS(J-1)+(TS(J)-TS(J-1))*(ZX-ZS(J-1))/(ZS(J)-ZS(J-1))
        ENDDO
       ENDDO
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
       TS(2)=FGPM(2)
       DO I=1,NSOIL
        IF(I .EQ. 1) THEN
         ZX=-0.5*ZSOIL(1)+SH
        ELSE
         ZX=-0.5*(ZSOIL(I-1)+ZSOIL(I))+SH
        ENDIF
        TSOIL(I)=TS(1)+(TS(2)-TS(1))*ZX/ZS(2)
       ENDDO
      ENDIF
cc      TSOIL(NSOIL+1)=FGPM(2)
      FGPM(7)=NSOIL
      DO I=1,NSOIL
       FGPM(7+I)=ZSOIL(I)
      ENDDO
cc      FGPM(7+NSOIL+1)=ZBOT    

      endif
      
c Generate initial states fgco from fgco0 & state
      do i=1,6
       fgco(i)=fgco0(i)
      enddo
      call fstfg1(fgco,tsoil,fgpm,state,iver)
       
      RETURN
      END
