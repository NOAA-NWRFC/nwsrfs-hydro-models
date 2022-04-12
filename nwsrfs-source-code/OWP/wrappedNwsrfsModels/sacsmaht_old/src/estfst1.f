cv07 changed to use smc & sh2o states
CVK
CVK  GENERATE INITIAL UNFROZEN WATER STATES IF NOT INPUTED
CVK
cc      SUBROUTINE ESTFST1(CF,IC,FGCO,TSOIL,FGPM)
cv07      SUBROUTINE ESTFST1(FGCO,TSOIL,FGPM,state)
      SUBROUTINE ESTFST1(FGCO,TSOIL,FGPM,state,smc,sh2o)      
      PARAMETER (T0=273.16)
      
      REAL FGCO(*),TSOIL(*),FGPM(*),state(6),smc(*),sh2o(*)
      REAL ZSOIL(8)

      COMMON/FRDSTFG/SMAX,PSISAT,BRT,SWLT,QUARTZ,STYPE,NUPL,NSAC,
     +               RTUP,RTLW,DZUP,DZLOW 
cc      COMMON/FRZCNST/ FRST_FACT,CKSOIL,ZBOT
      COMMON/FRZCNST/ FRST_FACT,ZBOT      
      NSOIL=FGPM(7)+0.1
      TBOT=FGPM(2)
cc      ZBOT=FGPM(7+NSOIL+1)
      
      L=0
      DO I=1,NSOIL
       IF(TSOIL(I) .LT. 0.) L=1
      ENDDO
      if(smc(1) .eq. -1) then
       SMC(1)=FGPM(3)*0.15
       SH2O(1)=SMC(1)
              
cv07      IF(L .NE. 0) THEN 
C  ESTIMATE UNFROZEN WATER STORAGES
       DO I=1,NSOIL
        ZSOIL(I)=FGPM(I+7)
       ENDDO
       DZUP=ZSOIL(1)-ZSOIL(NUPL)
       DZLOW=ZSOIL(NUPL)-ZSOIL(NSOIL)  
       SUZ=state(1)+state(2)
       SLZ=state(3)+state(4)+state(5)
       SMCUZ=0.001*RTUP*SUZ/DZUP+SWLT
       SMCLZ=0.001*RTLW*SLZ/DZLOW+SWLT
       TBUP=TBND(TSOIL(1)+T0,TSOIL(2)+T0,ZSOIL,ZBOT,1,NSOIL)
       SUP=0.
       SLW=0.

       DO I=2,NSOIL
C  CALCULATE AVERAGE SOIL TEMPERATURE OF I-TH LAYER
        IF(I .NE. NSOIL) THEN
         TBDN=TBND(TSOIL(I)+T0,TSOIL(I+1)+T0,ZSOIL,ZBOT,
     +             I,NSOIL)
        ELSE
         TBDN=TBND(TSOIL(I)+T0,TBOT,ZSOIL,ZBOT,I,NSOIL)
        ENDIF  
        DZ=ZSOIL(I-1)-ZSOIL(I)
        TS=ST_AVG1(TBUP,TSOIL(I)+T0,TBDN,DZ)
        TBUP=TBDN

C  CALCULATE POTENTIAL UNFROZEN WATER CONTENT
        IF(I .LE. NUPL) THEN
         SMC(I)=SMCUZ
         IF(TS .LE. T0) THEN
          SH2O(I)=FRH2O(TS,SMC(I),SMC(I),SMAX,BRT,PSISAT,FGPM(4))
         ELSE
          SH2O(I)=SMC(I)
         ENDIF
         DSW=1000*(SH2O(I)-SWLT)*(ZSOIL(I-1)-ZSOIL(I))/RTUP
         IF(DSW .GT. 0.) SUP=SUP+DSW        
        ELSE
         SMC(I)=SMCLZ
         IF(TS .LE. T0) THEN
          SH2O(I)=FRH2O(TS,SMC(I),SMC(I),SMAX,BRT,PSISAT,FGPM(4))
         ELSE
          SH2O(I)=SMC(I)
         ENDIF
         DSW=1000*(SH2O(I)-SWLT)*(ZSOIL(I-1)-ZSOIL(I))/RTLW
         IF(DSW .GT. 0.) SLW=SLW+DSW
        ENDIF 
       ENDDO

       if(fgco(1) .eq. 999. .and. L .ne. 0) then
        IF(SUP .GT. SUZ) SUP=SUZ
        IF(SLW .GT. SLZ) SLW=SLZ
        ALP=state(1)/SUZ
        FGCO(2)=SUP*ALP
        FGCO(3)=SUP*(1-ALP)
        ALP=state(3)/SLZ
        FGCO(4)=SLW*ALP
        ALP1=state(4)/SLZ
        FGCO(5)=SLW*ALP1
        FGCO(6)=SLW*(1-ALP-ALP1)
        FROST=0.
        DO I=1,5
         FROST=FROST+state(I)-FGCO(I+1)
        ENDDO
        FGCO(1)=-FROST/FRST_FACT
       ELSE
C  ALL WATER UNFROZEN
        DO I=1,5
         FGCO(I+1)=state(I)
        ENDDO
        FGCO(1)=0.
       endif
      endif 
cv07      ENDIF
        
      RETURN
      END

