module find_min_value
  implicit none
!public find_min_value
contains
 SUBROUTINE findMinValue(rd, n, d,t)
use Var
IMPLICIT NONE
INTEGER :: n
INTEGER :: i
INTEGER :: t
INTEGER :: most_index
REAL :: temp_value
REAL, INTENT(inout) :: rd(t)
INTEGER, INTENT(inout) :: d(t)
n=n
t=t
most_index = d(1)
temp_value = rd(1)
 do i=1,n
	IF(rd(i) <  temp_value)THEN
		 most_index = d(i)
         temp_value = rd(i)
	 END IF
END DO
min_value = temp_value
RETURN
END SUBROUTINE findMinValue
end module