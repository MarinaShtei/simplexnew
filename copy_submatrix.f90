module copy_submatrix
  implicit none
contains
SUBROUTINE copySubmatrix(Dest, Source, istart, depth, jstart, length, N)

IMPLICIT NONE

INTEGER :: istart
INTEGER :: depth
INTEGER :: jstart
INTEGER :: length
INTEGER :: i
INTEGER :: j

INTEGER :: N
REAL, INTENT(out) :: Dest(N,N)
REAL, INTENT(in) :: Source(N,N)
depth = depth
length = length
N=N
jstart = jstart
istart = istart


do i = istart,depth
	do j = jstart,length+jstart-1
		Dest(i-istart+1,j-jstart+1)=Source(i,j)
	END DO
END DO
RETURN
END SUBROUTINE copySubmatrix
end module