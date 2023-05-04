module bublesort_d
  implicit none

contains
SUBROUTINE Buble_Sort_d(arr_1,arr_2,n,t)!

IMPLICIT NONE
INTEGER :: i
INTEGER :: j  
INTEGER :: n
INTEGER :: t
INTEGER, INTENT(inout) :: arr_1(t)
REAL, INTENT(inout) :: arr_2(t)
INTEGER :: limit
INTEGER :: temp
REAL :: dtemp
n = n
t=t
do i = 1,n
	limit = n - i
	do j = 1,limit
		IF(arr_1(j)>arr_1(j+1)) THEN
			dtemp=arr_2(j)
			arr_2(j) = arr_2(j+1)
            arr_2(j) = dtemp			
			temp = arr_1(j)
			arr_1(j) = arr_1(j+1)
			arr_1(j+1) = temp
		END IF
	END DO
END DO
RETURN
END SUBROUTINE Buble_Sort_d
end module