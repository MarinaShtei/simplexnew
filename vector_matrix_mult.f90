module vector_matrix_mult
  implicit none
contains
SUBROUTINE vectorMatrixMult(c, b, A, n, m, t, g) !double c[], double b[], double A[][N],   int n, int m
IMPLICIT NONE
REAL, INTENT(inout) :: A(t,t)
REAL, INTENT(inout) :: b(t)
REAL, INTENT(inout) :: c(g)
INTEGER :: m
INTEGER :: n
INTEGER :: i
INTEGER :: j
INTEGER :: k
INTEGER :: t
INTEGER :: g
REAL :: sum1 
m=m
n=n
t=t
g=g
do i=1,m
	sum1 = 0.0
	do k=1,n
		sum1 = sum1 + A(k,i)*b(k)
	end do
	c(i)=sum1
end do
RETURN
end SUBROUTINE vectorMatrixMult
end module