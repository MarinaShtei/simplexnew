module find_most_negative
  implicit none
!public find_most_negative
contains
SUBROUTINE findMostNegative(rd, n, m, d,t)
use Var
IMPLICIT NONE

INTEGER :: n
INTEGER :: m
INTEGER :: t
INTEGER :: i
INTEGER :: most_index
REAL :: temp_value
REAL, INTENT(inout) :: rd(t)
INTEGER, INTENT(inout) :: d(t)
n=n
m=m
t=t
  most_index = d(m+1)
  temp_value = rd(1)
 do i=1,(n - m)
	IF(rd(i) <  temp_value)THEN
		 most_index = d(m+i)
       temp_value = rd(i);
	 END IF
END DO
enter_id = most_index
!RETURN most_index
RETURN
END  SUBROUTINE findMostNegative
end module