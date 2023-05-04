module simplex_algorithm
  implicit none
!public simplex_algorithm
contains

SUBROUTINE simplexAlgorithm()

use Var
use simplex
use globals
use invert_matrix ! inv_gaussian
IMPLICIT NONE
INTEGER :: optimal_flag
 INTEGER :: itemp
 INTEGER :: basis_i
 INTEGER :: count1
 INTEGER :: i
 INTEGER :: j
 REAL:: dtemp
 INTEGER :: tnum2
INTEGER :: tnum
 !REAL::min_value

 
  M = 64 
 N = 128
count1=1 
optimal_flag = 0

write (*,*) "m = " ,m1

write(*,*) " n = ",n1 

write (*,*) "basis1:"
do i = 1,m1
	write(*,*) basis(i)
end do


write(*,*)" A:"
do i=1,m1
	do j=1,n1
		write(*,*)" ",A(i,j)," "
	end do
end do

do while(optimal_flag == 0)
	write(*,*)" count = " ,count1
	count1 = count1 + 1
	
	call buble_Sort(basis,m1,N)
	do i = 1,m1
		write(*,*) basis(i)
	end do
	call setD()!d1, basis, m1,N, n1
	
	write (*,*) "d:"
	do i = 1,n1
		write(*,*) d1(i)
	end do
	
	call setAaux()!A_aux, A, d1, n1, m1
	
	write (*,*) "A_aux (B, D):"
	do i = 1,m1
		do j=1,n1
			write(*,*) A_aux(i,j)
		end do
	end do
	tnum2 = 1
	CALL copySubmatrix(B, A_aux, tnum2, m1, tnum2, m1,N) ! Set B
	
	write (*,*) "B:"
	do i = 1,m1
		do j=1,m1
			write(*,*) B(i,j)
		end do
	end do
	
	
	call invertMatrix(BI, B,m1) ! inv_gaussian  invertMatrix(A, B, W, n, m, epsilon1) !W , n1, m1, epsilon1
	call eraseEpsilonsMatrix(epsilon1,BI, m1, m1, N, N)
	
	write (*,*) "BI:"
	do i = 1,m1
		do j=1,m1
			write(*,*) BI(i,j)
		end do
	end do
	 CALL matrixMult(BIA_aux, BI, A_aux, m1, m1, n1,N)
	 call eraseEpsilonsMatrix(epsilon1,BIA_aux, m1, n1, N, N)
	 
	 write (*,*) "BIA_aux (I, B-1*D):"
	do i = 1,m1
		do j=1,n1
			write(*,*) BIA_aux(i,j)
		end do
	end do
	
	write (*,*) "A_aux (I, B-1*D):"
	do i = 1,m1
		do j=1,n1
			write(*,*) A_aux(i,j)
		end do
	end do
	
	write (*,*) "b:"
	do i = 1,m1
		write(*,*) b1(i)
	end do
	
	call matrixVectorMult(BIb, BI, b1, m1, m1,M) ! c, A, b, n, m
	call eraseEpsilonsVector(epsilon1,BIb, m1,N)
	
	write (*,*) "BIb:"
	do i = 1,m1
		write(*,*) BIb(i)
	end do
	tnum = m1 + 1
	tnum2 = 1
	call copySubmatrix(D, A_aux, tnum2, m1, tnum, n1-m1,N) !Set D :D, A_aux, 0, m, m, n-m
	
	write (*,*) "D:"
	do i = 1,m1
		do j=1,n1-m1
			write(*,*) D(i,j)
		end do
	end do
	!END OF FOR DEBUG ONLY
	
	call computeCbCd()!cb,cd,d1,c1,m1,n1,N
	
	write (*,*) "cb:"
	do i = 1,m1
		write(*,*) cb(i)
	end do
	
	write (*,*) "cd:"
	do i = 1,n1-m1
		write(*,*) cd(i)
	end do
	
	
	!cbBI = cb * B-1
	
	call vectorMatrixMult(cbBI, cb, BI, m1, m1, N,N) 
	call eraseEpsilonsVector(epsilon1, cbBI, m1,m1)
	
	write (*,*) "cbBI:"
	do i = 1,m1
		write(*,*) cbBI(i)
	end do
	
	
	call vectorMatrixMult(cbBID, cbBI, D, m1, n1 - m1,N,M) !cbBID, cbBI, D, m, n - m
	call eraseEpsilonsVector(epsilon1,cbBID, n1 - m1,n1 - m1)
	
	write (*,*) "cbBID:"
	do i = 1,n1-m1
		write(*,*) cbBID(i)
	end do
	
	
	call vectorSubtract(rd, cd,  cbBID, n1-m1,N,M)!result_v, v1, v2, n
	call eraseEpsilonsVector(epsilon1,rd, n1 - m1,n1 - m1)
	write (*,*) "rd( cd - cbBID ):"
	do i = 1,n1-m1
		write(*,*) rd(i)
	end do
	
	
	CALL findMinValue(rd, n1, d1,N)
		
	if(min_value>= 0.0)then
		optimal_flag = 1
	else
		CALL findMostNegative(rd, n1, m1, d1,N)
		CALL findExitingId()
		write (*,*) "\nenter_id = " ,enter_id  
		write(*,*) " exiting_id = " ,exiting_id 
		write(*,*) " d[exiting_id] = " ,d1(exiting_id+1) 
		
		write (*,*) "pivot: enter_id = " ,enter_id 
		
		write(*,*)" ,exiting_id = " , d1(exiting_id+1) 
		
		
		basis(exiting_id)=enter_id
		write (*,*) "basis3:"
		do i = 1,m1
			write(*,*) basis(i)
		end do
	
	end if
	
 
 
end do
RETURN
end SUBROUTINE simplexAlgorithm
end module