module matrix_mult
  implicit none
contains
SUBROUTINE matrixMult(C, A, B, n, m, p,t)
IMPLICIT NONE
REAL, INTENT(inout) ::A(t,t)
REAL, INTENT(inout) :: B(t,t)
REAL, INTENT(inout) :: C(t,t)
REAL :: sum1
INTEGER :: n
INTEGER :: m
INTEGER :: k
INTEGER :: i
INTEGER :: j
INTEGER :: p
INTEGER :: t
p=p
m=m
n=n
t=t
do i=1,n
	do j=1,p
		sum1 = 0.0;
		do k=1, m
			sum1 = sum1 + A(i,k)* B(k,j)
		end do
		C(i,j) = sum1
	end do
end do
RETURN
end SUBROUTINE matrixMult
end module