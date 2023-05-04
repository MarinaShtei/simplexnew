module print_original_system
  implicit none
!public print_original_system
contains
SUBROUTINE printOriginalSystem()!A, m, n
use globals
IMPLICIT NONE
!INTEGER :: m
!INTEGER :: n
INTEGER :: i
INTEGER :: j
!REAL, dimension(m,n),INTENT(inout) ::A


write(*,*) "Original System:"
do i=1,m1
	do j=1,n1
		write(*,*) " ",A(i,j)," "
	end do
end do
return
end SUBROUTINE printOriginalSystem
end module