module copy_matrix
  implicit none

contains
SUBROUTINE copyMatrix(Dest, Source, n1, m1, n_num)

IMPLICIT NONE
INTEGER :: n1
INTEGER :: m1
INTEGER :: i
INTEGER :: j
INTEGER :: n_num
REAL, INTENT(inout) :: Dest(n_num,n_num)
REAL, INTENT(inout) :: Source(n_num,n_num)

m1=m1
n1=n1
n_num = n_num
do i = 1,m1
	do j = 1,n1
		Dest(i,j)=Source(i,j)
	END DO
END DO

RETURN
END SUBROUTINE copyMatrix
end module