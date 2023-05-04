module set_Initial_A_aux
  implicit none
!public set_Initial_A_aux
contains
SUBROUTINE setInitialAaux()!Initial_A_aux ,Initial_A ,Initial_d ,Initial_n ,n ,m
use globals
IMPLICIT NONE

INTEGER :: i
INTEGER :: j
INTEGER :: k

do i=1, Initial_n
	k = Initial_d1(i)
	do j=1,m1
		Initial_A_aux(j,i) = Initial_A(j,k+1)
	end do
end do
RETURN
end SUBROUTINE setInitialAaux
end module