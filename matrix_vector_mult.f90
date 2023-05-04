module matrix_vector_mult
  implicit none
!public matrix_vector_mult
contains
SUBROUTINE matrixVectorMult(c, A, b, n, m ,t)
INTEGER :: m
INTEGER :: n
INTEGER :: i
INTEGER :: k
INTEGER :: t
REAL, INTENT(inout) :: b(t)
REAL, INTENT(inout) :: c(t)
REAL, INTENT(inout) :: A(t,t)
REAL :: sum1
m=m
n=n
t=t
do i = 1, n
	sum1 = 0.0
	do k = 1, m
		sum1 = sum1 + (A(i,k) * b(k))
	end do
	c(i) = sum1
end do
RETURN
end SUBROUTINE matrixVectorMult
end module