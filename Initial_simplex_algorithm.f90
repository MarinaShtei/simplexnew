module Initial_simplex_algorithm
  implicit none
contains

SUBROUTINE InitialSimplexAlgorithm()

use Var
use simplex
use invert_matrix ! inv_gaussian
use globals
IMPLICIT NONE


INTEGER :: i

INTEGER :: j
INTEGER :: k
INTEGER :: optimal_flag

INTEGER :: itemp
INTEGER :: basis_i
INTEGER :: counter
INTEGER :: tnum2
INTEGER :: tnum
REAL :: dtemp

 M = 64 
 N = 128

counter=0


optimal_flag = 0
 
 write (*,*) " m = " ,m1, ", Initial_n = ",Initial_n

 write(*,*) "Initial_basis:"
 do i = 1,m1
	write(*,*)Initial_basis(i)
end do

 write(*,*) "Initial_A:"
 do i = 1,m1
	do j=1,Initial_n
	write(*,*)Initial_A(i,j)
	end do
end do


do while(optimal_flag == 0)
	call buble_Sort(Initial_basis, m1,N)
	 write(*,*) "Initial_basis:"
	do i = 1,m1
		write(*,*)Initial_basis(i)
	end do
	call InitialsetD()!Initial_d1,Initial_basis,m1,n1,Initial_n
	
	write(*,*) "Initial_d:"
	do i = 1,Initial_n
		write(*,*)Initial_d1(i)
	end do
	
	 write(*,*) "Initial_A:"
 do i = 1,m1
	do j=1,Initial_n
	write(*,*)Initial_A(i,j)
	end do
end do
	
	call setInitialAaux()!Initial_A_aux,Initial_A,Initial_d1,Initial_n, n1, m1
	write(*,*) "Initial_A_aux (B, D):"
	do i = 1,m1
		do j=1,Initial_n
			write(*,*)Initial_A_aux(i,j)
		end do
	end do
	tnum2 = 1
	call copySubmatrix(Initial_B, Initial_A_aux, tnum2, m1, tnum2, m1, N) !Dest, Source, istart, depth, jstart, length,N

	write(*,*) "Initial_B:"
	do i = 1,m1
		do j=1,m1
			write(*,*)Initial_B(i,j)
		end do
	end do
	
	call invertMatrix(Initial_BI, Initial_B,m1) !(A, B, W, n, m, epsilon1)  W, n1, m1, epsilon1
	
	call eraseEpsilonsMatrix(epsilon1,Initial_BI, m1, m1,N,N)
	write(*,*) "Initial_BI:"
	do i = 1,m1
		do j=1,m1
			write(*,*)Initial_BI(i,j)
		end do
	end do
	
	call matrixMult(Initial_BIA_aux, Initial_BI, Initial_A_aux, m1, m1, Initial_n,N)
	call eraseEpsilonsMatrix(epsilon1,Initial_BIA_aux, m1, Initial_n,N,N)
	write(*,*) "Initial_BIA_aux (I, B-1*D):"
	do i = 1,m1
		do j=1,Initial_n
			write(*,*)Initial_BIA_aux(i,j)
		end do
	end do
	
	write(*,*) "Initial_A_aux (B,D):"
	do i = 1,m1
		do j=1,Initial_n
			write(*,*)Initial_A_aux(i,j)
		
		end do
	end do
	
	write(*,*) "Initial_b:"
	do i = 1,m1
		write(*,*)Initial_b1(i)
	end do
	
	call matrixVectorMult(Initial_BIb, Initial_BI, Initial_b1,  m1, m1,N)
	call eraseEpsilonsVector(epsilon1,Initial_BIb, m1,N)
	write(*,*) "Initial_BIb:"
	do i = 1,m1
		write(*,*)Initial_BIb(i)
	end do
	tnum = m1+1
	tnum2 = 1
	call copySubmatrix(Initial_D, Initial_A_aux,tnum2, m1, tnum, Initial_n-m1,N)
	write(*,*) "Initial_D:"
	do i = 1,m1
		do j=1,(Initial_n - m1)
			write(*,*)Initial_D(i,j)
		end do
	end do
	
	call computeInitialCbInitialCd()!Initial_cb,Initial_cd,Initial_d1,Initial_c1,m1,Initial_n,N
	write(*,*) "Initial_cb:"
	do i = 1,m1
		write(*,*)Initial_cb(i)
	end do
	
	write(*,*) "Initial_cd:"
	do i = 1,(Initial_n-m1)
		write(*,*)Initial_cd(i)
	end do
	
	call vectorMatrixMult(Initial_cbBI, Initial_cb, Initial_BI, m1,  m1, N,N)
	call eraseEpsilonsVector(epsilon1,Initial_cbBI,  m1,m1)
	
	write(*,*) "Initial_cbBI:"
	do i = 1,m1
		write(*,*)Initial_cbBI(i)
	end do
	call vectorMatrixMult(Initial_cbBID, Initial_cbBI, Initial_D, m1, Initial_n - m1, N,N) !Initial_cbBID, Initial_cbBI, Initial_D, m, Initial_n - m
	call eraseEpsilonsVector(epsilon1,Initial_cbBID, Initial_n - m1,Initial_n - m1)
	
	write(*,*) "Initial_cbBID:"
	do i = 1,Initial_n - m1
		write(*,*)Initial_cbBID(i)
	end do
	
	call vectorSubtract(Initial_rd,Initial_cd,Initial_cbBID,Initial_n-m1,N,N)
	call eraseEpsilonsVector(epsilon1,Initial_rd, Initial_n - m1,Initial_n - m1)
	
	write(*,*) "Initial_rd( cd - cbBID ):"
	do i = 1,Initial_n-m1
		write(*,*)Initial_rd(i)
	end do
	write(*,*) "Initial_BIA_aux (I, B-1*D):"
	do i = 1,m1
		do j=1,Initial_n
			write(*,*)Initial_BIA_aux(i,j)
		end do
	end do
	CALL  findMinValue(Initial_rd, n1, d1,N)
	 
	 IF(min_value >= 0)THEN
		optimal_flag = 1
	ELSE
		call findInitialMostNegative(Initial_d1,Initial_rd,m1,n1,Initial_n,N)
		call findInitialExitingId()
		write(*,*) "enter_id  = ", enter_id, ", exiting_id = ",exiting_id,",  Initial_d1[exiting_id] = ",Initial_d1(exiting_id+1)
		
		 Initial_basis(exiting_id+1) = enter_id
		 write(*,*) "Initial_basis:"
		 do i = 1,m1
		write(*,*)Initial_basis(i)
		
	end do
    END IF  
end do 
RETURN
END SUBROUTINE InitialSimplexAlgorithm
end module