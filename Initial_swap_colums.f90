module Initial_swap_colums
  implicit none

contains
SUBROUTINE InitialSwapColums(i, j)! m, Initial_A_aux, Initial_A ,n
use globals
IMPLICIT NONE
INTEGER :: j
INTEGER :: i
!INTEGER :: m
!INTEGER :: n
INTEGER :: k
!REAL, INTENT(inout) :: Initial_A_aux(n,n)
!REAL, INTENT(inout) :: Initial_A(n,n)
!m=m
i=i
j=j

do k=1,m1
	Initial_A_aux(k,i) = Initial_A(k,j)
     Initial_A_aux(k,j) = Initial_A(k,i)
end do
RETURN
END SUBROUTINE InitialSwapColums
end module