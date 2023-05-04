module set_d
  implicit none

contains
SUBROUTINE setD()!d, basis, m, n, n1
use globals
IMPLICIT NONE
!INTEGER, INTENT(inout) :: basis(n)
!INTEGER, INTENT(inout) :: d(n)
!INTEGER :: m
!INTEGER :: n
!INTEGER :: n1
INTEGER :: i
INTEGER :: j
INTEGER :: flag
INTEGER :: pos1
!m=m
!n=n

do i=1,m1
	d1(i)=basis(i)
end do

pos1 = m1

do i = 1, n1
	flag = 1
	j=0
	do while(flag==1 .and. j<m1)
		if ( i-1 == basis(j+1))then
			flag = 0
		end if
		j = j+1
	end do
	if (flag == 1)then
		pos1 = pos1 + 1
		d1(pos1) = i-1			
	end if
			
end do
RETURN
end SUBROUTINE setD
end module