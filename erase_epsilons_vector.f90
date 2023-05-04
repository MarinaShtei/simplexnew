module erase_epsilons_vector
  implicit none
!public erase_epsilons_vector
contains
SUBROUTINE eraseEpsilonsVector(epsilon1, darray, n, t)

IMPLICIT NONE
INTEGER :: i
INTEGER :: n
INTEGER :: t
REAL :: epsilon1
REAL, INTENT(inout) :: darray(t)
n=n
t=t
epsilon1=epsilon1
do i = 1,n
	IF(ABS(darray(i)) < epsilon1) THEN
		darray(i) = 0.0
	END IF
END DO
RETURN
END SUBROUTINE eraseEpsilonsVector
end module