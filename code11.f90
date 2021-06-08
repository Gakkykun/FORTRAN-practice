module group_functions
  implicit none
  contains

  real function bod_analytical(distance)
  real, intent(in) :: distance
  real, parameter :: k_B = 0.5, U =27500., Initial_bod = 18.18
    bod_analytical = Initial_bod * exp(-K_B/U*distance)
end function bod_analytical

  real function do_analytical(distance)
  real, intent(in) :: distance
  real, parameter :: k_B= 0.5, U = 27500., K_a = 1.0, DO_S = 10.0, Initial_bod = 18.18, Initial_do = 9.09
    do_analytical =  DO_S - K_B/(K_a - K_B)*Initial_bod*(exp(-K_B/U*distance)-exp(-K_a/U*distance)) &
    - (DO_S - Initial_do)*exp(-K_a/U*distance)
  end function do_analytical

end module group_functions


program code11
use group_functions
  implicit none
  integer :: i, nsteps, j
  real :: dx, l, k1B, k2B, k3B, k4B, k1D, k2D, k3D, k4D, error_bod, error_do, mae_bod, mae_do
  real, dimension(0:2000) :: conc_bod, conc_do, conc_bod_a, conc_do_a
  real, parameter :: k_B = 0.5, U=27500.0, K_a = 1.0, DO_S = 10.0
  real, dimension(1, 2) :: MonitoringData

  ! read input data
  open(unit=99, file='input.csv', status='old')
  read(99, '()') ! exclude the header
  read(99, *) MonitoringData(1,1), MonitoringData(1,2) !BOD, DO

  ! print*, MonitoringData(1,1)
  ! print*, MonitoringData(1,2)

  !step size
  dx = 0.1 ![km]
  nsteps = ceiling(200.0/dx)
  l = dx*1000 !unit conversion: km -> m

  ! ADD INITIAL CONDITIONS HERE
  conc_bod(0) = MonitoringData(1, 1)
  conc_bod_a(0) = MonitoringData(1, 1)
  conc_do(0) = MonitoringData(1, 2)
  conc_do_a(0) = MonitoringData(1, 2)

  error_bod = 0.0
  error_do = 0.0

  do i=0, nsteps-1
    k1B = -K_B/U*conc_bod(i)
    k2B = -K_B/U*(conc_bod(i)+0.5*l*k1B)
    k3B = -K_B/U*(conc_bod(i)+0.5*l*k2B)
    k4B = -K_B/U*(conc_bod(i)+l*k3B)

    k1D = K1B + K_a/U*(DO_S - conc_do(i))
    k2D = K2B + K_a/U*(DO_S - (conc_do(i)+0.5*l*k1D))
    k3D = K3B + K_a/U*(DO_S - (conc_do(i)+0.5*l*k2D))
    k4D = K4B + K_a/U*(DO_S - (conc_do(i)+l*k3D))

    conc_bod(i+1) = conc_bod(i) + l/6.0*(k1B + 2.0*k2B + 2.0*k3B + k4B)
    conc_do(i+1) = conc_do(i) + l/6.0*(k1D + 2.0*k2D + 2.0*k3D + k4D)

    conc_bod_a(i+1) = bod_analytical(l*(i+1))
    conc_do_a(i+1) = do_analytical(l*(i+1))

    error_bod = error_bod + abs(conc_bod_a(i+1) - conc_bod(i+1))
    error_do = error_do + abs(conc_do_a(i+1) - conc_do(i+1))

  end do

  mae_bod = error_bod / nsteps
  mae_do = error_do / nsteps

  print*, 'Mean absolute error for BOD', mae_bod
  print*, 'Mean absolute error for DO', mae_do

  ! writing results into a csv file
  open(unit=10, file='concentration.csv')
  write(10, *) 'x, bod, do'

  do j = 0, nsteps
    if(mod(j, 10) .eq. 0) then
        write(10, 100) j*dx, conc_bod(j), conc_do(j)
    end if
    
  end do
  100 format(F6.2, ',', F6.2, ',', F6.2)

end program code11