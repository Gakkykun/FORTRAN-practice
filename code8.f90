program code8
  implicit none
  integer, parameter :: NX = 3, NY = 3
  integer :: max_iter = 1000, i, j
  real :: x, y
  real, dimension(0:NX+1, 0:NY+1) :: t

  t = 0.0
  t(:, 0) = 100.0; t(0, :) = 100.0

  call tempcal(NX, NY, max_iter, t)

  open(unit=10, file='result2.csv')

  write(10, *) 'x, y, z, t' ! header

  do i = 0, NX+1
    x = real(i)

    do j = 0, NY+1
      y = real(j)
      write(10, 100) x, y, 0.0, t(i, j)
    end do

  end do
  100 format(3(F6.2, ','), F6.2)

  
contains
subroutine tempcal(n1, n2, max_iter, t)
  integer, intent(in) :: n1, n2, max_iter
  real, dimension(0:n1+1, 0:n2+1), intent(inout) :: t

  integer :: i, j, iter

  do iter = 1, max_iter
    do i = 1, n1
      do j = 1, n2
        t(i,j) = 0.25 * (t(i+1, j) + t(i-1, j) + t(i, j+1) + t(i, j-1))
      end do 
    end do
  end do

end subroutine tempcal

  
end program code8