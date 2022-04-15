C  FUNCTION CALCULATES FROZEN DEPTH.
C based on conversation with victor on 5/7/09 
C we will use this froutine to return the frozen depth
C removed frost, tsoil arguments not used
c returns bottom frozen depth

      FUNCTION FRZIND1(SMC,SH2O,ZSOIL,NSOIL,NUP,NSAC,RTUZ,RTLZ,
     +                 SWLT)
      
      REAL SMC(*),SH2O(*),ZSOIL(*)
      PARAMETER (SICEMN=0.01)
      PARAMETER (T0=273.16)
      PARAMETER (TMIN=-0.01)
cc      COMMON/FRZCNST/ FRST_FACT,ZBOT
               
      DO II=NSOIL,2,-1
       DICE=SMC(II)-SH2O(II)
       IF(DICE .GT. SICEMN) GOTO 10
      ENDDO
      FRZIND1=-999.0
      GOTO 11
   10 DZICE=DICE*(ZSOIL(II-1)-ZSOIL(II))/(SMC(II)-SWLT)
      FRZIND1=100.*(DZICE-ZSOIL(II-1))
   11 RETURN
      END       
C   11 DO JJ=2,NSOIL
C       DICE=SMC(JJ)-SH2O(JJ)
C       IF(DICE .GT. SICEMN) GOTO 20
C      ENDDO  
C      TSOIL(7)=0.
C      GOTO 21
C   20 IF(JJ .LT. II .AND. JJ .GT. 2) THEN
C       DZICE=DICE*(ZSOIL(JJ-1)-ZSOIL(JJ))/(SMC(JJ)-SWLT)
C       TSOIL(7)=100.*(-DZICE-ZSOIL(JJ))
C      ELSE
C       TSOIL(7)=0.
C      ENDIF
C   21 CONTINUE                
        
cc      FROST=0.
cc      FROST1=0.
cc      TSS=0.
     
cc      TBUP=TBND(TSOIL(1)+T0,TSOIL(2)+T0,ZSOIL,ZBOT,1,NSOIL)      
cc      DO I=2,NSOIL
C  CALCULATE AVERAGE SOIL TEMPERATURE OF I-TH LAYER
cc       IF(I .NE. NSOIL) THEN
cc        TBDN=TBND(TSOIL(I)+T0,TSOIL(I+1)+T0,
cc     +       ZSOIL,ZBOT,I,NSOIL)
cc       ELSE
cc        TBDN=TBND(TSOIL(I)+T0,TSOIL(NSOIL+1)+T0,
cc     +       ZSOIL,ZBOT,I,NSOIL)
cc       ENDIF  
cc       DZ=ZSOIL(I-1)-ZSOIL(I)
cc       TS=ST_AVG1(TBUP,TSOIL(I)+T0,TBDN,DZ)-T0
cc       TBUP=TBDN

cc       FR=1000.*(SMC(I)-SH2O(I))*DZ
cc       IF(I .LE. NUP) FR=FR/RTUZ
cc       IF(I .GT. NUP .AND. I .LE. NSAC) FR=FR/RTLZ
cc       FROST=FROST+FR
cc       IF(TS .LT. 0.) THEN
cc        FROST1=FROST1+FR*TS
cc        IF(FR .GT. 0.) TSS=TSS+TS
cc       ELSE
cc        IF(FR .GT. 0.) THEN
cc         FROST1=FROST1+FR*TMIN
cc         TSS=TSS+TMIN
cc        ENDIF
cc       ENDIF
cc      ENDDO

cc      FROST=-FROST/FRST_FACT
c      IF(TSS .LT. 0.) THEN
c       FROST1=FROST1/TSS
c      ELSE
c       FROST1=0.
c      ENDIF
cc      FRZIND1=FROST1/10.

C      FRZIND1=0.
C      RETURN
C      END
