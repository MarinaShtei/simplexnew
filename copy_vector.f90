module copy_vector
  implicit none
!public copy_vector
contains
SUBROUTINE copyVector(Dest, Source, n)

IMPLICIT NONE
INTEGER :: n
INTEGER :: i
REAL, INTENT(inout) :: Dest(n)
REAL, INTENT(inout) :: Source(n)
n=n

do i = 1,n
	Dest(i)=Source(i)
END DO
RETURN
END SUBROUTINE copyVector
end module