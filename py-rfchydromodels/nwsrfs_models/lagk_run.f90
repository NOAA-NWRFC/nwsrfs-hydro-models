subroutine lagk(n_hrus, ita, itb, meteng, &
    lagtbl_in, &
    ktbl_in, &
    ico_in, iinfl_in, ioutfl_in, istor_in, &
    sim_length, qa_in, lagk_out)
    
    ! !There are three subroutines to execute:  pin7, flag7, fka7
    ! !subroutines should be ran in the order presented
    ! !
    ! !pin7:  Formats inputs (via a P and C array) run LagK operations
    ! !
    ! !   p,c = pin7(ita,itb,jlag,jk,meteng,lagtbl,ktbl,ico,iinfl,ioutfl,istor)
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
    ! !           2) %EDIT% This wrapper Fortran code uses parameters:  X, Y, Z to develop lagtbl and ktbl
    ! !              See comment below %EDIT%
    ! !           3) The pin7.f subroutine was edited to start with a empty c/p array
    ! !              far larger than which should be needed [p(500),c(100)].  Below is 
    ! !              python code I used to chop the unused lines after executing the
    ! !              subroutine. This is not necessary for flagk, and fka subroutines to
    ! !              properly run.  I used this document, pg 1-3, as reference:
    ! !              https://www.nws.noaa.gov/ohd/hrl/nwsrfs/users_manual/part8/_pdf/833lagk.pdf
    ! !               k_start=int(p[17])
    ! !               k_len=int(p[k_start-1])SAKW1
    ! !               pina7_len=int(p[k_start+2*k_len])
    ! !               p_end=k_start+2*(k_len+pina7_len)+1
    ! !               p=p[:p_end]
    ! !               c=c[:int(c[0])]
    ! !
    ! !flag7:  Controls the Lag Operation
    ! !    qb = flag7(p,c,qa,[ndt])
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
    ! !    qc = flag7(p,c,qb,[ndt])
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

  implicit none

  ! ! inputs
  integer, intent(in):: n_hrus, ita, itb, sim_length
  character(len = 4), intent(in):: meteng
  double precision, dimension(n_hrus), intent(in):: ico_in, iinfl_in, ioutfl_in, istor_in
  double precision, dimension(10, n_hrus), intent(in):: lagtbl_in, ktbl_in
  double precision, dimension(sim_length, n_hrus), intent(in):: qa_in
  
  ! ! local varible
  real, dimension(n_hrus):: ico, iinfl, ioutfl, istor
  real, dimension(10, n_hrus):: lagtbl, ktbl
  real, dimension(sim_length, n_hrus):: qa 
  real, dimension(500,n_hrus):: p
  real, dimension(100,n_hrus):: c
  real, dimension(100):: c_cpy
  real, dimension(sim_length ,n_hrus):: qb, qc
  integer, dimension(n_hrus):: jlag, jk
  integer:: nh
  
  ! ! output 
  double precision, dimension(sim_length ,n_hrus), intent(out):: lagk_out

  ! ! Convert data types
  ico=real(ico_in)
  iinfl=real(iinfl_in)
  ioutfl=real(ioutfl_in)
  istor=real(istor_in)
  
  lagtbl=real(lagtbl_in)
  ktbl=real(ktbl_in)
  
  qa=real(qa_in)

  do nh=1,n_hrus  
   jlag(nh)=size(lagtbl(:, nh),1)/2
   jk(nh)=size(ktbl(:,nh),1)/2
  end do
      
  do nh=1,n_hrus
    
    call pin7(p(:,nh),c(:,nh),ita,itb,jlag(nh),jk(nh),meteng,lagtbl(:,nh), &
       ktbl(:,nh),ico(nh),iinfl(nh),ioutfl(nh),istor(nh))
    
    c_cpy=c(:,nh)
 
! !   write(*,*) c_cpy
! !   write(*,*) "BREAK"
! !   write(*,*) p(:,nh)
    write(*,*) qb(1:10,nh)
    write(*,*) sim_length
    
    call flag7(p(:,nh),c_cpy,qa(:,nh),qb(:,nh),sim_length)
    
    call fka7(p(:,nh),c_cpy,qb(:,nh),qc(:,nh),sim_length)
    
  end do
  
  lagk_out=dble(qc) 
  
end subroutine