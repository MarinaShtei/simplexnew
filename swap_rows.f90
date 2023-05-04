module swap_rows
  implicit none
!public swap_rows
contains
SUBROUTINE swaprows(W, n, m1, m2) !double W[][N], int n, int m1, int m2
IMPLICIT NONE
REAL, INTENT(inout) :: W(n,n)
INTEGER :: m1
INTEGER :: n
INTEGER :: i
INTEGER :: m2
REAL :: temp
m1=m1
n=n
m2=m2

do i=1,2*n
	temp = W(m1,i)
	W(m1,i) = W(m2,i)
	W(m2,i) = temp
end do
RETURN
end SUBROUTINE swaprows
end module