module copy_to_initial_matrix
  implicit none
!public copy_to_initial_matrix
contains
SUBROUTINE copyToInitialMatrix()!Initial_A, Initial_A_aux, A, m, n
use globals
IMPLICIT NONE
INTEGER :: k
INTEGER :: j
INTEGER :: i
!INTEGER :: m
!INTEGER :: n
!REAL, INTENT(inout) :: Initial_A_aux(m,n)
!REAL, INTENT(inout) :: Initial_A(m,n)
!REAL, INTENT(inout) :: A(m,n)
!m=m
!n=n
do i = 1,m1
	do j = 1,n1
		Initial_A(i,j) = A(i,j)
		Initial_A_aux(i,j) = A(i,j)
	END DO
END DO

do i = 1,m1
	do j = n1+1,n1+m1
		IF(i == (j-n1)) THEN
		Initial_A(i,j) = 1.0
		Initial_A_aux(i,j) = 1.0
		ELSE
		Initial_A(i,j) = 0.0
		Initial_A_aux(i,j) = 0.0
		END IF
	END DO
END DO
RETURN
END SUBROUTINE copyToInitialMatrix
end module