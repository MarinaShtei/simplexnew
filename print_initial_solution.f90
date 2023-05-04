module print_initial_solution
  implicit none
!public print_initial_solution
contains
SUBROUTINE printInitialSolution()!Initial_basis, Initial_BIb, m
use globals
IMPLICIT NONE
INTEGER :: i



write(*,*) "Initial basis:"

do i=1,m1
	write(*,*)" ", Initial_basis(i)," "
end do


write(*,*) "Basic Solution:"
do i=1,m1
		write(*,*)" X",Initial_basis(i),  " = ", Initial_BIb(i)
end do
RETURN
end SUBROUTINE printInitialSolution
end module