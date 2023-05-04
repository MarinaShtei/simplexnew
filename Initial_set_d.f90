module Initial_set_d
  implicit none
!public Initial_set_d
contains
SUBROUTINE InitialSetD()!Initial_d,Initial_basis,m,n,Initial_n
use globals
IMPLICIT NONE

!INTEGER :: m
!INTEGER :: n
INTEGER :: i
!INTEGER :: Initial_n
INTEGER :: j
INTEGER :: pos1
INTEGER :: flag
!INTEGER, INTENT(inout) :: Initial_basis(n)
!INTEGER, INTENT(inout) :: Initial_d(n)
!n=n
!m=m
!Initial_n = Initial_n
do i = 1,m1
	Initial_d1(i) = Initial_basis(i)
END DO
 pos1 = m1
do i = 1,Initial_n
	flag=1
	j=0
	do while(flag==1 .and. j<m1)
		if ( i-1 == Initial_basis(j+1))then
			flag = 0
		end if
		j = j+1
	end do 
	if(flag==1) then
		pos1 = pos1+1
		Initial_d1(pos1) = i-1	
	end if
end do
 
RETURN
END SUBROUTINE InitialSetD
end module