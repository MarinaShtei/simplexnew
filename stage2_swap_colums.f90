module stage2_swap_colums
  implicit none

contains
SUBROUTINE stage2SwapColums(i, j)!A_aux, A, n, m
use globals
IMPLICIT NONE
!REAL, INTENT(inout) :: A_aux(n,n)
!REAL, INTENT(inout) :: A(n,n)
!INTEGER :: m
!INTEGER :: n
INTEGER :: i
INTEGER :: j
INTEGER :: k
!m=m
!n=n
i=i
j=j
do k=1, m1
	A_aux(k,i) = A(k,j)
     A_aux(k,j) = A(k,i)
end do
RETURN
end SUBROUTINE stage2SwapColums
end module