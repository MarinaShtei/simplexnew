module print_solution
  implicit none
!public print_solution
contains

SUBROUTINE printSolution()!basis, BIb,c,  m
use globals
IMPLICIT NONE


INTEGER :: i
REAL :: temp
REAL :: sum1



write(*,*) "basis:"

do i=1,m1
	write(*,*) " ", basis(i), " "
end do


write(*,*) "Basic Solution:\n"
do i=1,m1
		write(*,*) " X",(basis(i)+1)," = " ,BIb(i)," "
end do

  temp = (c1(basis(1))* BIb(1))
  sum1 = temp
  write(*,*) c1(basis(1)), " * "  ,BIb(1)," "


do i=2,m1
	temp = (c1(basis(i))* BIb(i))
	sum1 = sum1 + temp	
	write(*,*) "+ ",c1(basis(i)+1)," * " ,BIb(i)
	!write(*,*) "+ " temp " "
end do 
write(*,*) " = ",sum1

RETURN
end SUBROUTINE printSolution
end module