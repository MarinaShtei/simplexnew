module set_A_aux
  implicit none
!public set_A_aux
contains
SUBROUTINE setAaux()!A_aux, A, d, n, m
use globals
IMPLICIT NONE
!REAL, INTENT(inout) :: A_aux(n,m)
!REAL, INTENT(inout) :: A(n,m)
!INTEGER, INTENT(inout) :: d(n)
!INTEGER :: m
!INTEGER :: n
INTEGER :: i
INTEGER :: j
INTEGER :: k
!m=m
!n=n

do i = 1, n1
	k = d1(i)
	DO j = 1, m1
	A_aux(j,i) = A(j,k+1)
	END DO
end do
RETURN
end SUBROUTINE setAaux
end module