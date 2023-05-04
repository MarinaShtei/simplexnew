module find_Initial_exiting_id
  implicit none
!public find_Initial_exiting_id
contains
SUBROUTINE findInitialExitingId()!Initial_BIA_aux ,Initial_BIb ,Initial_d1 ,n1 ,m1 ,Initial_n
use Var
use globals
IMPLICIT NONE
INTEGER :: i
INTEGER :: j

INTEGER :: temp_min_index
INTEGER :: init_flag
INTEGER :: q
REAL :: temp_min
REAL :: temp


do i = 1,Initial_n
	IF(Initial_d1(i) == enter_id) THEN
	q = i
	END IF
END DO

init_flag = 0

do i = 1,m1
	write(*,*) "y[", i-1, "][",q-1,"] =  ",Initial_BIA_aux(i,q) ,"x[",i-1,"] = ",Initial_BIb(i)
	write(*,*) "init_flag = " ,init_flag
	IF(Initial_BIA_aux(i,q) > 0.0)THEN
		temp = Initial_BIb(i)/Initial_BIA_aux(i,q)
		write(*,*) "i = " ,i-1
		write(*,*) " temp = " ,temp
		
		IF(init_flag == 0) THEN
			temp_min = temp
			temp_min_index = i-1
			init_flag = 1
		ELSE
			IF(temp < temp_min) THEN
				temp_min = temp
				temp_min_index = i-1
			END IF
		END IF
	END IF
	
	write(*,*)"temp_min_index = ",temp_min_index ,i
	write(*,*) ", temp_min  = ",temp_min
	
END DO
 exiting_id = temp_min_index
 RETURN
END SUBROUTINE findInitialExitingId
end module