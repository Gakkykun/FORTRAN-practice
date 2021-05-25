program code4
  implicit none
  real :: a, b, c

  a=1.0; b=2.0; c=3.0

  open(unit=10, file='result.csv')
  write(10, 100) a, b, c
  100 format(2(F10.4, ','), F10.4)

  
end program code4