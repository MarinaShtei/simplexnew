! bublesort function for simplex
module bublesort
  implicit none
!public bublesort
contains
SUBROUTINE buble_Sort(arr, n,t)
IMPLICIT NONE
INTEGER :: i
INTEGER :: j
INTEGER :: n
INTEGER :: t
INTEGER, INTENT(inout) :: arr(t)
INTEGER :: limit
INTEGER :: temp
t=t
n = n
do i = 1,n
	limit = n - i
	do j = 1,limit
		IF(arr(j)>arr(j+1)) THEN
			temp = arr(j)
			arr(j) = arr(j+1)
			arr(j+1) = temp
		END IF
	END DO
END DO
RETURN
END SUBROUTINE buble_Sort
end module



	


		