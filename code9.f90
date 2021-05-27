program code9
  implicit none
  integer :: i, nsteps
  real :: dt
  real, dimension(0:1000) :: y, t

  dt = 0.1
  nsteps = ceiling(1/dt) 
  print*, 'step size = ', dt
  print*, '# of stemps = ', nsteps


  t(0) = 0.0
  y(0) = 1.0

  do i=0, nsteps-1
    y(i+1) = y(i) + dt*f(y(i))
    t(i+1) = t(i) + dt
  end do

  print*, 'Numerical solution = ', y(nsteps)
  print*, 'This compares to the FORTRAN EXP(x) function result: ', exp(-1.0)
  print*, 'The absolute error is: ', abs(exp(-1.0) -y(nsteps))

  contains
  real function f(y)
  real, intent(in) :: y
    f = -y
  end function
  
end program code9