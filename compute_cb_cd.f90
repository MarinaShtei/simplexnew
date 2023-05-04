module compute_cb_cd
  implicit none

contains
SUBROUTINE computeCbCd()!cb,cd,d,c,m,n,N_num
use globals
IMPLICIT NONE
INTEGER :: i
!INTEGER :: m
!INTEGER :: n
!INTEGER :: N_num
!REAL, INTENT(inout) :: cb(N_num)
!REAL, INTENT(inout) :: cd(N_num)
!INTEGER, INTENT(inout) :: d(N_num)
!REAL, INTENT(inout) :: c(N_num)

!m=m
!n=n
!N_num = N_num

do i=1,m1
	cb(i)=c1(d1(i)+1)
END DO

do i=m1,n1
	cd(i-m1)=c1(d1(i)+1)
END DO

write (*,*) "d:"

do i=1,n1 
	write (*,*) " ",d1(i)," "
END DO

write (*,*) "cb:"
do i=1,m1
	write (*,*) " ",cb(i)," "
END DO

write (*,*) "cd:"

do i=1,n1
	write (*,*) " ",cd(i)," "
END DO
RETURN
END SUBROUTINE computeCbCd
end module