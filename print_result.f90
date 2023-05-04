module print_result
  implicit none
!public print_result
contains
SUBROUTINE printResult(basis, BIb, m,n)
IMPLICIT NONE
INTEGER :: m
INTEGER :: n
INTEGER :: i
INTEGER :: j
INTEGER :: k
INTEGER, INTENT(inout) :: basis(n)
REAL, INTENT(inout) :: BIb(n)
REAL :: sum1
m=m
n=n

write(*,*) "Optimal Basis:"

do i=1,m
	write(*,*) " ",basis(i)," "
end do
  
write(*,*) "Optimal Solution:"
do i=1,m
		write(*,*) " X"
		write(*,*) basis(i)," = " ,BIb(i)," "
end do

RETURN 
end SUBROUTINE printResult
end module