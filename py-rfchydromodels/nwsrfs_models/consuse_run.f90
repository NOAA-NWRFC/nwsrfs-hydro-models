subroutine consuse(NDT_in,AREA_in,EFF_in,MFLOW_in, &
    IRFSTOR_in,ACCUM_in,DECAY_in, &
    ETD_in,QNAT_in, &
    QADJ_out,QDIV_out,QRFIN_out,QRFOUT_out, &
    QOL_out,QCD_out,CE_out)

! !     CONSUMPTIVE USE OPERATION FLOWCHART
! !     -----------------------------------

! !        1.  READ IN TIME-SERIES

! !            Provided :  NATURAL SIMULATED FLOW (24 HOUR)
! !                        EVAPORATION DEMAND (24 HOUR)

! !             Noted Difference: the CHPS version input is potential evatporation
! !             CALCULATE CROP ET  (CE = K * PE)
! !             Evaporation Demand is calculated internally

! !        2.  DETERMINE CROP DEMAND  (QCD = CE * Area)

! !        3.  CALCULATE DIVERSION (QDIV = QCD / Efficiency)

! !        4.  CALCULATE RETURN FLOW OUT (QRFOUT = RFstor(0) * Decay)

! !        5.  IF (QNAT + QRFOUT) >= (QDIV + MFLOW)

!             a)  THEN, QDIV = QDIV
! !            b)  ELSE, IS (MFLOW) > (QNAT + QRFOUT)

! !                     i)  THEN, QDIV = 0.0
! !                     ii) ELSE, QDIV = (QNAT + QRFOUT - MFLOW)

! !                QCD = QDIV * e,
! !                CE = QCD / Area

! !        6. CALCULATE ADJUSTED FLOW  (QADJ = (QNAT + QRFOUT) - QDIV)

! !        7. CALCULATE RETURN FLOW IN  (QRFIN = QDIV * ACCUM)

! !        8. CALCULATE OTHER LOSSES  (QOL = QDIV - QCD - QRFIN)

! !        9. CALCULATE RETURN FLOW STORAGE
! !                          RFstor(1) = RFstor(0) + QRFIN - QRFOUT

! !        1         2         3         4         5         6         7

! !     UNITS CONVERSION

! !     1 INCH = 25.4 MM
! !     1 MM*KM*KM / DAY = 0.011574 CMSD
! !     1 CFSD to 0.0283168 CMSD
! !     1 CMSD to 35.3147 CFSD

! !        1         2         3         4         5         6         7

! !     VARIABLE DEFINITIONS

! !     OPTION    - ET ESTIMATION OPTION: TEMPERATURE OR POTENTIAL ET (ALWAYS 1)

! !     ETD(*)     - ET DEMAND TIME SERIES (MM)
! !     QNAT(*)   - NATURAL FLOW TIME SERIES (CFSD)
! !     QADJ(*)   - ADJUSTED FLOW TIMER SERIES (CFSD)
! !     QDIV(*)   - DIVERSION FLOW TIME SERIES (CFSD)
! !     QRFIN(*)  - RETURN FLOW IN TIME SERIES (CFSD)
! !     QRFOUT(*) - RETURN FLOW OUT TIME SERIES (CFSD)
! !     QOL(*)    - OTHER LOSSES TIME SERIES (CFSD)
! !     QCD(*)    - CROP DEMAND TIME SERIES (CFSD)
! !     CE(*)     - CROP EVAPOTRANSPIRATION (MM)

! !     A         - CONVERSION FACTOR FROM (MM*KM^2/DAY) TO CMSD
! !     AREA      - IRRIGATED AREA (KM^2)
! !     EFF       - IRRIGATION EFFICIENCY (NONDIMENSIONAL)
! !     ACCUM     - RETURN FLOW ACCUMULATION RATE (NONDIMENSIONAL)
! !     DECAY     - RETURN FLOW DECAY RATE (NONDIMENSIONAL)
! !     QSUM      - SUM OF NATURAL FLOW AND RETURN FLOW OUT
! !     QADD      - SUM OF DIVERSION FLOW AND MINIMUM STREAMFLOW
! !     MFLOW     - MINIMUM FLOW (CFSD)
! !     RFSTOR    - RETURN FLOW STORAGE

  ! ! inputs
  integer, intent(in):: NDT_in
  double precision, intent(in):: AREA_in,EFF_in,MFLOW_in
  double precision, intent(in):: IRFSTOR_in,ACCUM_in,DECAY_in
  double precision, dimension(NDT_in), intent(in):: ETD_in,QNAT_in  
  
  ! ! local varible
  integer:: NDT
  real:: AREA,EFF,MFLOW
  real:: IRFSTOR,ACCUM,DECAY
  real, dimension(NDT_in):: ETD,QNAT
  real, dimension(NDT_in):: QADJ,QDIV,QRFIN,QRFOUT
  real, dimension(NDT_in):: QOL,QCD,CE
  
  ! ! output 
  double precision, dimension(NDT_in), intent(out):: QADJ_out,QDIV_out
  double precision, dimension(NDT_in), intent(out):: QRFIN_out,QRFOUT_out
  double precision, dimension(NDT_in), intent(out):: QOL_out,QCD_out,CE_out
  
  ! ! Convert double precision to single precision 
  NDT=int(NDT_in)
  AREA=real(AREA_in)
  EFF=real(EFF_in) 
  MFLOW=real(MFLOW_in)*0.0283168
  IRFSTOR=real(IRFSTOR_in)
  ACCUM=real(ACCUM_in)
  DECAY=real(DECAY_in)
  
  ETD=real(ETD_in)
  QNAT=real(QNAT_in)*0.0283168
  
  ! ! Run Conuse subroutine
  call EX57 (NDT,AREA,EFF,MFLOW,IRFSTOR,ACCUM,DECAY, &
     ETD,QNAT,QADJ,QDIV,QRFIN,QRFOUT,QOL,QCD,CE)
     
  ! ! Convert Output to double precision
  QADJ_out=dble(QADJ)*35.3147
  QDIV_out=dble(QDIV)*35.3147
  QRFIN_out=dble(QRFIN)*35.3147
  QRFOUT_out=dble(QRFOUT)*35.3147
  QOL_out=dble(QOL)*35.3147
  QCD_out=dble(QCD)*35.3147
  CE_out=dble(CE)*35.3147
end subroutine