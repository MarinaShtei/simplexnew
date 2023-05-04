module find_Initial_most_negative
  implicit none
!public find_Initial_most_negative
contains
SUBROUTINE findInitialMostNegative(Initial_d,Initial_rd,m,n,Initial_n,t)
use Var
IMPLICIT NONE

INTEGER :: m
INTEGER :: n
INTEGER :: t
INTEGER :: i
INTEGER :: Initial_n
INTEGER :: most_index
REAL :: temp_value
REAL, INTENT(inout) :: Initial_rd(t)
INTEGER, INTENT(inout) :: Initial_d(t)
n=n
m=m
t=t
Initial_n = Initial_n
most_index = Initial_d(m+1)
temp_value = Initial_rd(1)
 do i=1,(Initial_n - m)
	IF(Initial_rd(i) <  temp_value)THEN
		 most_index = Initial_d(m+i)
       temp_value = Initial_rd(i)
	 END IF
END DO
enter_id = most_index
RETURN
END SUBROUTINE findInitialMostNegative
end module