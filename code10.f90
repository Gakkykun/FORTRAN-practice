program code10
  implicit none
  integer :: i, nsteps
  real :: dx, k1, k2, k3, k4
  real, dimension(0:10) :: x, y

  dx = 0.1
  nsteps = ceiling(1.0/dx)

  x(0) = 1.0
  y(0) = 0.0

  ! Modified Euler method
  do i=0, nsteps-1
    k1 = f(x(i), y(i))
    k2 = f(x(i)+dx, y(i)+dx*k1)

    y(i+1) = y(i) + dx/2.0 *(k1+k2)
    x(i+1) = x(i) + dx

  end do

  print*, 'Numerical solution using Modified Euler method: ', y(nsteps)


  ! Fourth-order Runge-Kutta method
  do i=0, nsteps -1
    k1 = f(x(i), y(i))
    k2 = f(x(i)+0.5*dx, y(i)+0.5*dx*k1)
    k3 = f(x(i)+0.5*dx, y(i)+0.5*dx*k2)
    k4 = f(x(i)+dx, y(i)+dx*k3)

    y(i+1) = y(i) + dx/6.0*(k1 + 2.0*k2 + 2.0*k3 + k4)
    x(i+1) = x(i) + dx
  end do

  print*, 'Numerical solution using Fourth-order Runge-Kutta method: ', y(nsteps)


  contains
  real function f(x, y)
  real, intent(in) :: x, y
    f = x**2 + y**2
  end function f


  
end program code10