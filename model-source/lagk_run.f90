subroutine lagk(n_hrus, ita, itb, &
    lagtbl_a_in, lagtbl_b_in, lagtbl_c_in, lagtbl_d_in,&
    ktbl_a_in, ktbl_b_in, ktbl_c_in, ktbl_d_in, &
    lagk_lagmax_in, lagk_kmax_in, lagk_qmax_in, &
    lagk_lagmin_in, lagk_kmin_in, lagk_qmin_in, &
    ico_in, iinfl_in, ioutfl_in, istor_in, &
    qa_in, sim_length,lagk_out)
    
    ! !There are three subroutines to execute:  pin7, flag7, fka7
    ! !subroutines should be ran in the order presented
    ! !
    ! !pin7:  Formats inputs (via a P and C array) run LagK operations
    ! !
    ! !         pin7(p,c,ita,itb,jlag,jk,meteng,lagtbl,ktbl,ico,iinfl,ioutfl,istor)
    ! !          p:  output from pin7 subroutine.  Contains lag/q, k/q, and 
    ! !              2*S/(DT/4)+O, O tables.  Also specifies the timestep of 
    ! !              Input output
    ! !          c:   output from pin7 subroutine. Contains initial, inflow, 
    ! !               utflow, storage, and carryover values
    ! !         ita:  Data time interval of the inflow time series (HR)
    ! !         itb:  Data time interval of the outflow time series (HR)
    ! !         jlag: If > 0 - number of pairs of Lag and Q values used to define the
    ! !               variable Lag vs Q curve.  If = 0 - constant Lag will be used
    ! !         jk:   If > 0 - number of pairs of K and Q values used to define the
    ! !               variable K vs Q curve.  If = 0 - constant K will be used
    ! !         meteng:  Code specifying whether units of q, lag, k parameters and initial
    ! !                 values are English or metric:  'ENGL' = enter flow in CFS and 
    ! !                 volume in CFSD,'METR' = enter flow in CMS and volume in CMSD, 
    ! !                 Default is metric.  Note output:  P and C output is ALWAYS converted
    ! !                 to metric
    ! !         lagtbl:  If jlag=0, constant Lag value.  If jlag>0, lag and q pairs, in
    ! !                   that order, in a single column array.  Must be in ascending order
    ! !                   of q (example: [6, 0, 4, 10000, 3.5, 20000]).
    ! !         ktbl:  If jk=0, constant K value.  If jk>0, K and q pairs, in
    ! !                   that order, in a single column array.  Must be in ascending order
    ! !                   of q (example: [1, 100, 1, 40000, 3, 100000]).
    ! !         ico:  Initial carry over 
    ! !         iinfl:  Initial inflow
    ! !         ioutfl:  Initial outflow
    ! !         istor:  Initial storage
    ! !
    ! !  !!Notes: 
    ! !           1) This routine handles all the variables which would be optimized:  
    ! !              lagtbl, ktble, ico, iinfl, ioutfl, istor
    ! !           2) %EDIT% This wrapper Fortran code uses parameters:  a, b c, d to develop 
    ! !              lagtbl and ktbl.  The equation is lag/k_table_entry=a*(Q-d)**2+b*Q+c
    ! !              Q is the flow table entry
    ! !           3) The pin7.f subroutine was edited to start with a empty c/p array
    ! !              far larger than which should be needed [p(500),c(100)].  Below is 
    ! !              python code I used to chop the unused lines after executing the
    ! !              subroutine. This is not necessary for flagk, and fka subroutines to
    ! !              properly run.  I used this document, pg 1-3, as reference:
    ! !              https://www.weather.gov/media/owp/oh/hrl/docs/833lagk.pdf
    ! !               k_start=int(p[17])
    ! !               k_len=int(p[k_start-1])
    ! !               pina7_len=int(p[k_start+2*k_len])
    ! !               p_end=k_start+2*(k_len+pina7_len)+1
    ! !               p=p[:p_end]
    ! !               c=c[:int(c[0])]
    ! !
    ! !flag7:  Controls the Lag Operation
    ! !             flag7(p,c,qa,qb[ndt])
    ! !
    ! !            qb: downstream streamflow values (single column array) with only
    ! !                lag applied, time step is assumed to correspond to itb.  
    ! !            p:  output from pin7 subroutine.  Contains lag/q, k/q, and 
    ! !                2*S/(DT/4)+O, O tables.  Also specifies the timestep of 
    ! !                Input output
    ! !            c:  output from pin7 subroutine. Contains initial, inflow, 
    ! !                outflow, storage, and carryover values.  !!NOTE!! use a copy of
    ! !                 the original c array as it get edited during the subroutine
    ! !            qa: Upstream streamflow values (single column array), time step is
    ! !                assumed to correspond to ita
    ! !            ntd:  Optional variable, total number to time steps to process.
    ! !                  if less than full qa array is desired 
    ! !
    ! !fka7:    Perform the attenuation (K) computations 
    ! !             flag7(p,c,qb,qc [ndt])
    ! !
    ! !            qc: downstream streamflow values (single column array) with both
    ! !                lag and attenuation applied, time step is assumed to correspond
    ! !                to itb.  
    ! !            p:  output from pin7 subroutine.  Contains lag/q, k/q, and 
    ! !                2*S/(DT/4)+O, O tables.  Also specifies the timestep of 
    ! !                Input output
    ! !            c:  output from pin7 subroutine. Contains initial, inflow, 
    ! !                outflow, storage, and carryover values
    ! !            qb: downstream streamflow values (single column array) with only
    ! !                lag applied, time step is assumed to correspond to itb
    ! !            ntd: Optional variable, total number to time steps to process.
    ! !                  if less than full qa array is desired 

    ! !            UNITS CONVERSION
    ! !             1 CFS to 0.0283168 CMS
    ! !             1 CMS to 35.3147 CFS
    ! !             1 CFD to 0.0283168 CMD

  implicit none

  ! ! inputs
  integer, intent(in):: n_hrus, ita, itb, sim_length
  character(len = 4), parameter:: meteng = 'METR'
  !character(len = 4), parameter:: meteng = 'ENGL'
  double precision, dimension(n_hrus), intent(in):: ico_in, iinfl_in, ioutfl_in, istor_in
  double precision, dimension(n_hrus), intent(in):: lagtbl_a_in, lagtbl_b_in, lagtbl_c_in, lagtbl_d_in  
  double precision, dimension(n_hrus), intent(in):: ktbl_a_in, ktbl_b_in, ktbl_c_in, ktbl_d_in
  double precision, dimension(n_hrus), intent(in):: lagk_kmax_in, lagk_lagmax_in, lagk_qmax_in
  double precision, dimension(n_hrus), intent(in):: lagk_kmin_in, lagk_lagmin_in, lagk_qmin_in
  double precision, dimension(sim_length, n_hrus), intent(in):: qa_in
  
  ! ! local varible
  real, dimension(n_hrus):: ico, iinfl, ioutfl, istor
  real, dimension(n_hrus):: lagtbl_a, lagtbl_b, lagtbl_c, lagtbl_d    
  real, dimension(n_hrus):: ktbl_a, ktbl_b, ktbl_c, ktbl_d
  real, dimension(n_hrus):: lagk_kmax, lagk_lagmax, lagk_qmax
  real, dimension(n_hrus):: lagk_kmin, lagk_lagmin, lagk_qmin
  real, dimension(22, n_hrus):: lagtbl, ktbl
  real, dimension(sim_length, n_hrus):: qa 
  real, dimension(500,n_hrus):: p
  real, dimension(100,n_hrus):: c
  real, dimension(100):: c_cpy
  real, dimension(sim_length ,n_hrus):: qb, qc
  integer, dimension(n_hrus):: jlag, jk
  integer:: nh, i
  real::  ndq, lag_entry, k_entry
  
  ! ! output 
  double precision, dimension(sim_length ,n_hrus), intent(out):: lagk_out

  ! ! Convert double precision to single precision.
  !NEED TO COMMENT OUT UNIT CONVERSION BELOW IF USING ENGL
  ico=real(ico_in)*0.0283168e0
  iinfl=real(iinfl_in)*0.0283168e0
  ioutfl=real(ioutfl_in)*0.0283168e0
  istor=real(istor_in)*0.0283168e0
  
  lagtbl_a=real(lagtbl_a_in)
  lagtbl_b=real(lagtbl_b_in)
  lagtbl_c=real(lagtbl_c_in)
  lagtbl_d=real(lagtbl_d_in) 
  ktbl_a=real(ktbl_a_in) 
  ktbl_b=real(ktbl_b_in) 
  ktbl_c=real(ktbl_c_in) 
  ktbl_d=real(ktbl_d_in)
  lagk_kmax=real(lagk_kmax_in)
  lagk_lagmax=real(lagk_lagmax_in)
  !NEED TO COMMENT OUT UNIT CONVERSION BELOW IF USING ENGL
  lagk_qmax=real(lagk_qmax_in)*0.0283168e0
  lagk_kmin=real(lagk_kmin_in)
  lagk_lagmin=real(lagk_lagmin_in) 
  !NEED TO COMMENT OUT UNIT CONVERSION BELOW IF USING ENGL
  lagk_qmin=real(lagk_qmin_in)*0.0283168e0
  
  qa=real(qa_in)*0.0283168e0

  ! write(*,*)'qa_in'
  ! do i=1,100
  !   write(*,*)qa_in(i,:)
  ! end do

  lagk_out = 0
  lagtbl = 0 
  ktbl = 0
  lag_entry = 0
  k_entry = 0
  p = 0 
  c = 0
  c_cpy = 0
  qb = 0 
  qc = 0

  ! write(*,*) 'n_hrus',n_hrus
  ! write(*,*) 'ita',ita
  ! write(*,*) 'itb',itb
  ! write(*,*) 'lagtbl_a_in',lagtbl_a_in
  ! write(*,*) 'lagtbl_b_in',lagtbl_b_in
  ! write(*,*) 'lagtbl_c_in',lagtbl_c_in
  ! write(*,*) 'lagtbl_d_in',lagtbl_d_in
  ! write(*,*) 'ktbl_a_in',ktbl_a_in
  ! write(*,*) 'ktbl_b_in',ktbl_b_in
  ! write(*,*) 'ktbl_c_in',ktbl_c_in
  ! write(*,*) 'ktbl_d_in',ktbl_d_in
  ! write(*,*) 'lagk_lagmax_in',lagk_lagmax_in
  ! write(*,*) 'lagk_kmax_in',lagk_kmax_in
  ! write(*,*) 'lagk_qmax_in',lagk_qmax_in
  ! write(*,*) 'ico_in',ico_in
  ! write(*,*) 'iinfl_in',iinfl_in
  ! write(*,*) 'ioutfl_in',ioutfl_in
  ! write(*,*) 'istor_in',istor_in
  ! write(*,*) 'sim_length',sim_length
  
  ! ! Populate Lag and K tables  
  do nh=1,n_hrus  
   ndq=0
   do i=1,11

    lagtbl(i*2,nh)=ndq*(lagk_qmax(nh)-lagk_qmin(nh))+lagk_qmin(nh)
    ktbl(i*2,nh)=ndq*(lagk_qmax(nh)-lagk_qmin(nh))+lagk_qmin(nh)
   
    lag_entry=lagtbl_a(nh)*(ndq-lagtbl_d(nh))**2+lagtbl_b(nh)*ndq+lagtbl_c(nh)
    k_entry=ktbl_a(nh)*(ndq-ktbl_d(nh))**2+ktbl_b(nh)*ndq+ktbl_c(nh)
   
    if (lag_entry > 0 .AND. lag_entry < 1) then
     lagtbl(i*2-1,nh)=lag_entry*(lagk_lagmax(nh)-lagk_lagmin(nh))+lagk_lagmin(nh)
    else if (lag_entry >= 1) then
     lagtbl(i*2-1,nh)=lagk_lagmax(nh)
    else
     lagtbl(i*2-1,nh)=lagk_lagmin(nh)
    end if
    
    if (k_entry > 0 .AND. k_entry < 1) then
     ktbl(i*2-1,nh)=k_entry*(lagk_kmax(nh)-lagk_kmin(nh))+lagk_kmin(nh)
    else if (k_entry >= 1) then
     ktbl(i*2-1,nh)=lagk_kmax(nh)
    else
     ktbl(i*2-1,nh)=lagk_kmin(nh)
    end if
   
    ndq=ndq+.1
   end do
  end do

!   write(*,*)'lagtbl'
!   do i=1,22
!     write(*,*)lagtbl(i,:)
!   end do

!   write(*,*)'ktbl'
!   do i=1,22
!     write(*,*)ktbl(i,:)
!   end do

  ! return
  
  ! ! Get length of K and Lag Table.  If the lag or k table max value equals its min value,
  ! ! then pass a table size of 0 specifiying constant lag, otherwise get the true size of the table
  ! ! (this check is to prevent a bug in flag7.f from occuring:  passing a all zero lag table causes a return ts of zeros)
  do nh=1,n_hrus  
    if (MAXVAL(lagtbl(::2,nh))==MINVAL(lagtbl(::2,nh))) then
      jlag(nh)=0
    else
      jlag(nh)=size(lagtbl,1)/2
    end if
    if (MAXVAL(ktbl(::2,nh))==MINVAL(ktbl(::2,nh))) then
      jk(nh)=0
    else
      jk(nh)=size(ktbl,1)/2
    end if
  end do
  
!  write(*,*)'jlag'
!  write(*,*)jlag

!  write(*,*)'jk'
!  write(*,*)jk


  ! ! Loop through each reach and calculate lag 
  do nh=1,n_hrus
    
    call pin7(p(:,nh),c(:,nh),int(ita,4),int(itb,4),jlag(nh),jk(nh),meteng,lagtbl(:,nh), &
       ktbl(:,nh),ico(nh),iinfl(nh),ioutfl(nh),istor(nh))

    ! write(*,*)'p'
    ! do i=1,500
    !   if(p(i,nh) .ne. 0) write(*,*) p(i,nh)
    ! end do

    ! write(*,*)'c'
    ! do i=1,100
    !   if(c(i,nh) .ne. 0) write(*,*) c(i,nh)
    ! end do
    
    c_cpy=c(:,nh)

    ! write(*,*)'qa'
    ! do i=1,100
    !   write(*,*)qa(i,:)
    ! end do

    call flag7(p(:,nh),c_cpy,qa(:,nh),qb(:,nh),int(sim_length,4))

    ! write(*,*)'qb'
    ! do i=1,100
    !   write(*,*)qb(i,:)
    ! end do
    
    call fka7(p(:,nh),c_cpy,qb(:,nh),qc(:,nh),int(sim_length,4))
    
  end do
  
  lagk_out=dble(qc)*35.3147d0
  
end subroutine