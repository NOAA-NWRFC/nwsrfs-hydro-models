subroutine consuse(sim_length, year, month, day, &
    AREA_in,EFF_in,MFLOW_in, &
    ACCUM_in,DECAY_in, peadj_m, peadj, &
    PET_in,QNAT_in, &
    QADJ_out,QDIV_out,QRFIN_out,QRFOUT_out, &
    QOL_out,QCD_out,CE_out,RFSTOR_out)

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

! !     PET(*)     - PET TIME SERIES (MM)
! !     QNAT(*)   - NATURAL FLOW TIME SERIES (CFSD)
! !     QADJ(*)   - ADJUSTED FLOW TIMER SERIES (CFSD)
! !     QDIV(*)   - DIVERSION FLOW TIME SERIES (CFSD)
! !     QRFIN(*)  - RETURN FLOW IN TIME SERIES (CFSD)
! !     QRFOUT(*) - RETURN FLOW OUT TIME SERIES (CFSD)
! !     QOL(*)    - OTHER LOSSES TIME SERIES (CFSD)
! !     QCD(*)    - CROP DEMAND TIME SERIES (CFSD)
! !     CE(*)     - CROP EVAPOTRANSPIRATION (MM)
! !     RFSTOR(*) - RETURN FLOW STORAGE ARRAY

! !     ETD(*)     - ET DEMAND TIME SERIES (MM)
! !     A         - CONVERSION FACTOR FROM (MM*KM^2/DAY) TO CMSD
! !     AREA      - IRRIGATED AREA (KM^2)
! !     EFF       - IRRIGATION EFFICIENCY (NONDIMENSIONAL)
! !     ACCUM     - RETURN FLOW ACCUMULATION RATE (NONDIMENSIONAL)
! !     DECAY     - RETURN FLOW DECAY RATE (NONDIMENSIONAL)
! !     QSUM      - SUM OF NATURAL FLOW AND RETURN FLOW OUT
! !     QADD      - SUM OF DIVERSION FLOW AND MINIMUM STREAMFLOW
! !     MFLOW     - MINIMUM FLOW (CFSD)

! !     peadj_m   - Monthly PEadj table
! !     peadj     - PET scale factor


  ! ! inputs
  integer, intent(in):: sim_length
  integer, dimension(sim_length), intent(in):: year, month, day
  double precision, intent(in):: AREA_in,EFF_in,MFLOW_in
  double precision, intent(in):: ACCUM_in,DECAY_in, peadj
  double precision, dimension(sim_length), intent(in):: PET_in,QNAT_in  
  double precision, dimension(12), intent(in):: peadj_m
    
  ! ! local varible
  integer:: NDT, mo, i
  real:: AREA,EFF,MFLOW
  real:: IRFSTOR,ACCUM,DECAY
  double precision:: interp_day, dayn, dayi, d,peadj_step
  integer, dimension(12) :: mdays, mdays_prev
  real, dimension(sim_length):: ETD,QNAT
  real, dimension(sim_length):: QADJ,QDIV,QRFIN,QRFOUT
  real, dimension(sim_length):: QOL,QCD,CE,RFSTOR
  double precision, dimension(12):: peadj_m_prev, peadj_m_next

  ! ! local spin-up varibles
  integer, parameter:: NDT_su = 365
  double precision:: IRFSTOR_su_start, IRFSTOR_su_end, pdiff
  real :: IRFSTOR_su
  real, dimension(NDT_su):: ETD_su,QNAT_su
  real, dimension(NDT_su):: QADJ_su,QDIV_su,QRFIN_su,QRFOUT_su
  real, dimension(NDT_su):: QOL_su,QCD_su,CE_su,RFSTOR_su
  integer:: spin_up_counter, spin_up_max_iter
  
  ! ! output 
  double precision, dimension(sim_length), intent(out):: QADJ_out,QDIV_out
  double precision, dimension(sim_length), intent(out):: QRFIN_out,QRFOUT_out
  double precision, dimension(sim_length), intent(out):: QOL_out,QCD_out,CE_out
  double precision, dimension(sim_length), intent(out):: RFSTOR_out
  ! ! temp
! !  double precision, dimension(sim_length), intent(out):: ETD_out
  
  ! ! Lookup Tables
  mdays =      (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /) 
  mdays_prev = (/ 31, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30 /) 
  
  ! ! Convert double precision to single precision 
  NDT=int(sim_length)
  AREA=real(AREA_in)
  EFF=real(EFF_in) 
  MFLOW=real(MFLOW_in)*0.0283168
  ACCUM=real(ACCUM_in)
  DECAY=real(DECAY_in)
  
  QNAT=real(QNAT_in)*0.0283168
  
  ! ! Calculate daily PE_adj from monthly values

  peadj_m_prev(1) = peadj_m(12)
  peadj_m_prev(2:12) = peadj_m(1:11)
  peadj_m_next(12) = peadj_m(1)
  peadj_m_next(1:11) = peadj_m(2:12)
  
  interp_day = 16
  
  do i = 1,sim_length,1
      
      ! adjust days in february if the year is a leap year
      if(mod(year(i),100) .ne. 0 .and. mod(year(i),4) .eq. 0) then
        mdays(2) = 29 ! leap year
      else if(mod(year(i),400).eq.0) then
        mdays(2) = 29 ! leap year
      else
        mdays(2) = 28 ! not leap year
      endif

      interp_day = 16.
      ! current day and month
      d = dble(day(i))
      mo = month(i)
      !write(*,*)mdays, mdays(mo), day(i), hour(i)
      if(d >= interp_day)then
        dayn = dble(mdays(mo))
        dayi = d - interp_day 
        peadj_step = peadj_m(mo) + dayi/dayn*(peadj_m_next(mo)-peadj_m(mo))
        
      else 
        dayn = dble(mdays_prev(mo))
        dayi = d - interp_day + mdays_prev(mo) 
        peadj_step = peadj_m_prev(mo) + dayi/dayn*(peadj_m(mo)-peadj_m_prev(mo))
      end if 

      ETD(i)=real(PET_in(i)*peadj_step*peadj)
      
  end do

  ! ! IRFSTOR spin-up proceedure

    ! starting values
    IRFSTOR_su_start = dble(1) ! avoid divide by zero 
    IRFSTOR_su_end = dble(0) 
    pdiff = dble(1.0)
    QNAT_su=QNAT(:NDT_su)
    ETD_su=ETD(:NDT_su)
    spin_up_counter = 0
    spin_up_max_iter = 50
    
    ! ! Continue searching for IRFSTOR until starting storage is within 1% of ending storage
    do while (pdiff > 0.01 .and. spin_up_counter < spin_up_max_iter)

      spin_up_counter = spin_up_counter + 1

      ! put the ending states from the previous iteration as the starting states 
      IRFSTOR_su = real(IRFSTOR_su_end)

      ! Run Consuse for the NTD_su time with the initial rfstor 
      call EX57 (int(NDT_su),AREA,EFF,MFLOW,IRFSTOR_su,ACCUM,DECAY, &
         ETD_su,QNAT_su,QADJ_su,QDIV_su,QRFIN_su,QRFOUT_su,QOL_su,QCD_su,CE_su,RFSTOR_su)

      IRFSTOR_su_end = dble(RFSTOR_su(NDT_su))

      ! ! Check the difference between starting RFSTOR and ending RFSTOR
      pdiff = abs(IRFSTOR_su_start-IRFSTOR_su_end)/IRFSTOR_su_start
    
      ! write(*,'(3f10.3)')pdiff, IRFSTOR_su_start, IRFSTOR_su_end
      IRFSTOR_su_start = IRFSTOR_su_end
  
    end do
    
  ! ! set the initial rfstate to the final spin up values
  IRFSTOR = real(IRFSTOR_su_end)
  
  ! ! Run Conuse subroutine
  call EX57 (NDT,AREA,EFF,MFLOW,IRFSTOR,ACCUM,DECAY, &
     ETD,QNAT,QADJ,QDIV,QRFIN,QRFOUT,QOL,QCD,CE,RFSTOR)
     
  ! ! Convert Output to double precision
  QADJ_out=dble(QADJ)*35.3147
  QDIV_out=dble(QDIV)*35.3147
  QRFIN_out=dble(QRFIN)*35.3147
  QRFOUT_out=dble(QRFOUT)*35.3147
  QOL_out=dble(QOL)*35.3147
  QCD_out=dble(QCD)*35.3147
  CE_out=dble(CE)
  RFSTOR_out=dble(RFSTOR)
  
end subroutine