PROGRAM simplex_main
use simplex
use Var
use simplex2
use globals
IMPLICIT NONE
!int i, j, p, n_p_m, itemp;
!char str[80];

INTEGER :: i
INTEGER :: j
INTEGER :: p
INTEGER :: n_p_m
INTEGER :: itemp 
INTEGER :: x
CHARACTER(len=50) :: arg
CHARACTER(len=1) :: str
CHARACTER(len=1) :: str2
CHARACTER(len=1) :: str3
CHARACTER(len =8) :: str4
INTEGER :: info1
INTEGER :: info2
REAL :: info3
REAL, allocatable :: t(:)
REAL, allocatable :: g(:)
REAL, allocatable :: y (:,:)
CHARACTER(LEN=30) :: FMT
FMT = "(f1.2)"
N = 128
M = 64
 DO x = 1, iargc()
    CALL getarg(x, arg)
 END DO
 
 if(x>2)then
	write(*,*)"Usage: a.exe filename\n"
 end if

open(unit = 1, file = arg, status = 'unknown', action='read')

	read(1,*) str, str2!
	
	read(1,*) info1 ,info2

write (*,*) "str = " ,str ," ",str2
m1 = info1
n1 = info2

write(*,*) "n = ",n1, ", m = ",m1

Initial_n = n1 + m1

read(1,*) str3

write (*,*) "str = ", str3

n_p_m = n1+m1 
!read_file()
allocate( t(n1) )
read(1,*) t
do i=1,n1
	c1(i) = t(i)
end do
do i=1,n1
	write(*,*) "c[" , i,"] = ", c1(i)
end do

	read(1,*) str


write (*,*) "str = ", str


write (*,*) "A: str = " ,str

allocate( y(m1,n1) )

do i = 1,m1
	read(1,*) y(i,:)
end do
do i = 1, m1
	do j=1,n1
		A(i,j)=y(i,j)
	end do
end do

do i = 1, m1
	do j=1,n1
		write(*,*) " ",A(i,j), " " 
	end do
end do


	read(1,*) str


write (*,*) "str = ",str

write (*,*) "b: str = " ,str

allocate( g(m1) )
read(1,*) g
do i=1,m1
	b1(i) = g(i)
end do
do i=1,m1
		write(*,*) " ",b1(i) ," " 
	end do

	read(1,*) str4

	write(*,*) str4
	
read(1,FMT)epsilon1


call copyMatrix(A_aux, A, n1, m1, N)
!read_file()
deallocate(t)
deallocate(g)
deallocate(y)
close(1)
write(*,*) "epsilon = " ,epsilon1 

write(*,*) " A:"

call printOriginalSystem()!A, n1, m1
call copyToInitialMatrix()!Initial_A, Initial_A_aux, A, m1, n1

write(*,*)" Initial_A:"
do i=1,m1
	do j=1,n_p_m
		write(*,*)" ",Initial_A(i,j)," "
	end do
end do

do i=1,m1
	 Initial_basis(i) = i-1+n1
end do

do i=1,n1
	Initial_c1(i) = 0.0
end do

do i=n1+1, Initial_n
	Initial_c1(i) = 1.0
end do

do i=1,m1
	Initial_b1(i) = b1(i)
end do

do i=1,m1
	 Initial_b_aux(i) = b1(i)
end do

write(*,*)"Initial_basis:"
do i=1,m1
	write(*,*)" " ,Initial_basis(i)
end do

write(*,*)"Initial_c:"
do i=1,Initial_n
	write(*,*)" " ,Initial_c1(i)
end do


write(*,*)"Initial_b:"
do i=1,m1
	write(*,*)" " ,Initial_b1(i)
end do

call InitialSimplexAlgorithm() 

do i=1,m1
	itemp = Initial_basis(i)
	basis(i)=itemp
	if(itemp >= n1) then
		call printNoSolution()
		call exit(0)
	end if
end do 

call printInitialSolution()!Initial_basis, Initial_BIb, m1

call simplexAlgorithm()

call Buble_Sort_d(basis, BIb, m1,N)      

call printSolution()!basis, BIb,c,  m1

close(1)
 
END PROGRAM simplex_main


