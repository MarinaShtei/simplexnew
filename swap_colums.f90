module swap_colums
  implicit none
!public swap_colums
contains
SUBROUTINE swapColums(A, i, j, m, n) !(double A[][N], int i, int j, int m, int n)
IMPLICIT NONE
REAL, INTENT(inout) :: A(n,n)
INTEGER :: m
INTEGER :: n
INTEGER :: i
INTEGER :: j
INTEGER :: k
REAL :: temp
m=m
n=n
i=i
j=j

do k=1, m
	temp = A(k,i)
	A(k,i) = A(k,j)
	A(k,j)=temp
end do
RETURN
end SUBROUTINE swapColums
end module