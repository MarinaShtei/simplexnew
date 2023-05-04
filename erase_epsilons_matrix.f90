module erase_epsilons_matrix
  implicit none
!public erase_epsilons_matrix
contains
SUBROUTINE eraseEpsilonsMatrix(epsilon1,dmat, m1, n1, n_num, m_num)

IMPLICIT NONE
INTEGER :: n1
INTEGER :: m1
INTEGER :: n_num
INTEGER :: m_num
INTEGER :: i
INTEGER :: j
REAL :: epsilon1
REAL, INTENT(inout) :: dmat(m_num,n_num)

n_num = n_num
m_num = m_num
n1=n1
m1 = m1
epsilon1=epsilon1

do i = 1,m1
	do j = 1,n1
		IF(ABS(dmat(i,j)) < epsilon1) THEN
		dmat(i,j) = 0.0
		END IF
	END DO
END DO
RETURN
END SUBROUTINE eraseEpsilonsMatrix
end module