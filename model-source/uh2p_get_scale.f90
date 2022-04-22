subroutine uh2p_get_scale_root(shape, toc, dt_hours, scale)

  use uh_optim

  implicit none

  double precision, intent(in):: shape, toc, dt_hours

  double precision:: machine_eps, t, a, b
  double precision, intent(out):: scale
  integer, parameter:: max_len = 1000

  scale = 0
  ! write(*,*)shape, toc, dt_hours, max_len

  ! tolerances
  machine_eps = epsilon(machine_eps)
  t = sqrt(machine_eps)

  ! lower and upper limits
  a = 0.02d0
  b = scale_uplimit(shape, dt_hours, max_len)

  ! write(*,*)a, b, machine_eps, t, scale, shape, toc, dt_hours
  scale = zero_uh2p(a, b, machine_eps, t, shape, toc, dt_hours)

end subroutine

subroutine uh2p_get_scale_optim(shape, toc, dt_hours, scale)

  use uh_optim

  implicit none

  double precision, intent(in):: shape, toc, dt_hours

  double precision:: machine_eps, e, t, a, b, c, m, fx
  double precision, intent(out):: scale
  integer, parameter:: max_len = 1000

  scale = 0
  ! write(*,*)shape, toc, dt_hours, max_len

  ! tolerances
  machine_eps = epsilon(machine_eps)
  e = sqrt(machine_eps)
  t = sqrt(machine_eps)

  ! lower and upper limits
  a = 0.02d0
  b = scale_uplimit(shape, dt_hours, max_len)

  ! initial guess
  c = ( a + b ) / 4d0

  ! upper limit for second derivative, objective function is basically linear
  ! so dont need this value to be very big
  m = 10d0

  ! write(*,*)machine_eps, e, t, a, b, c, m

  ! write(*,*)a, b, c, m, machine_eps, e, t, scale, shape, toc, dt_hours
  fx = glomin_uh2p(a, b, c, m, machine_eps, e, t, scale, shape, toc, dt_hours)

end subroutine

subroutine uh2p_len_obj_root_test(scale, shape, toc, dt_hours, obj)

  use uh_optim

  double precision, intent(in):: shape, scale, dt_hours, toc
  double precision:: obj
  integer:: uh_len
  integer, parameter:: m = 1000

  ! write(*,*)shape, scale, dt_hours, toc
  obj = 0
  uh_len = 0

  ! add one to the length because the first ordinate is at time 0
  uh_len = idnint(toc/dt_hours)+1
  ! write(*,*)uh_len, shape, scale, dt_hours
  obj = dble(uh2p_len(shape, scale, dt_hours, m) - uh_len)
end subroutine