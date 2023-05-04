module compute_Initial_cb_Initial_cd
  implicit none

contains
SUBROUTINE computeInitialCbInitialCd()!Initial_cb,Initial_cd,Initial_d,Initial_c,m,Initial_n,N_num
use globals
IMPLICIT NONE
INTEGER :: i


do i=1,m1
	Initial_cb(i) = Initial_c1(Initial_d1(i)+1)
END DO

do i=m1+1,Initial_n
	Initial_cd(i-m1) = Initial_c1(Initial_d1(i)+1)
END DO

write (*,*) "Initial_d:"

do i=1,Initial_n
	write (*,*) " ",Initial_d1(i)," "
END DO

write (*,*) "Initial_cb:"

do i=1,m1
	write (*,*) " ",Initial_cb(i)," "
END DO


write (*,*) "Initial_cd:"

do i=1,Initial_n - m1 
	write (*,*) " ",Initial_cd(i)," "
END DO

RETURN
END SUBROUTINE computeInitialCbInitialCd
end module