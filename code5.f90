program code5
  implicit none
  real :: discriminant, a, b, c, root1, root2

  print*, 'Enter A: '
  read*, a
  print*, 'Enter B: '
  read*, b
  print*, 'Enter C: '
  read*, c

  discriminant = b**2 - 4.0*a*c

  If(discriminant >= 0) then
    root1 = (-b + sqrt(discriminant))/(2.0*a)
    root2 = (-b - sqrt(discriminant))/(2.0*a)
    print*, 'Root 1 =', root1
    print*, 'Root 2 =', root2
  else
    print*, 'no real roots'
  end if

  
end program code5