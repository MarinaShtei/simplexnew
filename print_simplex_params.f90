module print_simplex_params
  implicit none
!public print_simplex_params
contains
SUBROUTINE printSimplexParams(A, A_aux, c, b1, n, m, B, BID, D, basis, d1, cb, cd)
IMPLICIT NONE
INTEGER :: m
INTEGER :: n
INTEGER :: i
INTEGER :: j
INTEGER  counter
REAL, INTENT(inout) :: A(n,n)
REAL, INTENT(inout) :: A_aux(n,n)
REAL, INTENT(inout) :: BID(n,n)
REAL, INTENT(inout) :: B(n,n)
REAL, INTENT(inout) :: D(n,n)
REAL, INTENT(inout) :: cb(n)
REAL, INTENT(inout) :: cd(n)
REAL, INTENT(inout) :: b1(n)
REAL, INTENT(inout) :: c(n)
INTEGER, INTENT(inout) :: basis(n)
INTEGER, INTENT(inout) :: d1(n)
counter = 0
m=m
n=n

write(*,*) "m = " ,m, ",n = " ,n


write(*,*) "A:"
do i=1,m
	do j=1,n
		write(*,*) " ",A(i,j)," "
	end do
end do

write(*,*) "c:"

do i=1,n
	write(*,*) " ",c(i)," "
end do


write(*,*) "b:"

do i=1,m
	write(*,*) " ", b1(i)," "
end do 

write(*,*) "A_aux:"
do i=1,m
	do j=1,n
		write(*,*) " ", A_aux(i,j)," "
	end do
end do

write(*,*) "B:"
do i=1,m
	do j=1,n
		write(*,*) " ",B(i,j)," "
	end do
end do

write(*,*) "basis:"

do i=1,m
	write(*,*) " ",basis(i)," "
end do

counter = counter + 1

 
 if (counter >= 8) then
    call exit(0)
end if
 
RETURN
end SUBROUTINE printSimplexParams
end module