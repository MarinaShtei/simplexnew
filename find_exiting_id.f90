module find_exiting_id
  implicit none

contains
SUBROUTINE findExitingId()!BIA_aux,BIb, d1 ,n1, m1
use Var
use globals
IMPLICIT NONE
INTEGER :: i
INTEGER :: j
INTEGER :: temp_min_index
INTEGER :: init_flag
INTEGER :: q
INTEGER :: unbounded_flag
REAL :: temp_min
REAL :: temp


do i = 1,n1
	IF(d1(i) == enter_id) THEN
	q = i
	END IF
END DO

init_flag = 0
unbounded_flag = 1

do i = 1,m1
	write(*,*) "y[",i-1,"][",q-1,"] =  ",BIA_aux(i,q)  
	write(*,*) "x[",i-1,"] = " ,BIb(i)
	write(*,*) "init_flag = ",init_flag  
	IF(BIA_aux(i,q) > 0.0)THEN
	
		unbounded_flag = 0
		temp = BIb(i)/BIA_aux(i,q)
		write(*,*) "i = ",i-1 
		write(*,*) "temp = " ,temp
	
		IF(init_flag == 0) THEN
			temp_min = temp
			temp_min_index = i-1
			init_flag = 1
		ELSE
			IF(temp < temp_min) THEN
				temp_min = temp
				temp_min_index = i-1
			END IF
			write(*,*) "2:i = ",i 
		END IF
	END IF
END DO

write(*,*) "unbounded_flag = " ,unbounded_flag


IF(unbounded_flag == 1)THEN
	write(*,*)"Unbounded linear program!"
	call EXIT(1)
END IF
exiting_id = temp_min_index
RETURN
END SUBROUTINE findExitingId
end module