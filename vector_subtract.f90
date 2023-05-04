module vector_subtract
  implicit none
!public vector_subtract
contains
SUBROUTINE vectorSubtract(result_v, v1, v2, n,t,g)!double result_v[], double v1[], double v2[], int n)
IMPLICIT NONE
REAL, INTENT(inout) :: result_v(t)
REAL, INTENT(inout) :: v1(t)
REAL, INTENT(inout) :: v2(g)
INTEGER :: n
INTEGER :: i
INTEGER :: t
INTEGER :: g
n=n
t=t
g=g
do i=1,n
	result_v(i)= v1(i)- v2(i)
end do 
RETURN
end SUBROUTINE vectorSubtract
end module