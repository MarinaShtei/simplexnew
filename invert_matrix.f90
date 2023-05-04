module invert_matrix
  implicit none
!public invert_matrix

contains
SUBROUTINE  invertMatrix(B2, A1, x)! W, n, m, epsilon1
use swap_rows
use globals
IMPLICIT NONE
!inv_gaussian
INTEGER :: x
INTEGER :: k
INTEGER :: i
INTEGER :: j
INTEGER :: p
INTEGER :: itemp
REAL :: RelativeValue
REAL :: MaxValue
REAL :: temp
REAL, INTENT(inout) ::A1(N,N)
REAL, INTENT(inout) :: B2(N,N)
N = 128

do i=1,x
	do j=1,x
		W(i,j) = A1(i,j)
	end do
end do

do i=1,x
	do j=x+1,x*2
		W(i,j) = 0.0
	end do
end do


do i=1,x
	W(i,x+i) = 1.0
end do

write(*,*) "Before loop W:"
do i=1,x
	do j=1,2*x
		write(*,*) W(i,j)," "
	end do
end do

do k=1,x
	
	write(*,*) "k = " ,k	
	p = k
	MaxValue = ABS(W(k,k))
	 
	 do i=k+1,x
		if(ABS(W(i,k)) > MaxValue)then
			p=i
			MaxValue = ABS(W(i,k))
		end if
	 end do
	 write(*,*) "p = " ,p

	 write(*,*)",k= " ,k	 
	 if(p /= k)then
		call swaprows(W,x,k,p)
	 end if
	 RelativeValue = W(k,k)
	 write(*,*) "RelativeValue =" ,RelativeValue 
	 W(k,k)=1.0
	 
	 do j = (k+1),(2*x)
		temp = W(k,j)/RelativeValue
		if(ABS(temp)<epsilon1)then
			W(k,j) = 0.0
		else
			W(k,j)=temp
		end if
	 end do
	 
	 do i = 1, x
		if (i /= k)then
			RelativeValue = W(i,k)
			W(i,k) = 0.0
			do j = k+1, 2*x+1
				temp = (W(i,j)-(RelativeValue*W(k,j)))
				if(ABS(temp)<epsilon1)then
					W(i,j)= 0.0
				else
					W(i,j)=temp
				end if
			end do
		end if
	 end do
	 write(*,*) "W: "
	 do i=1,x
		do j=1,x*2
			write(*,*) W(i,j), " "
		end do
	 end do	
end do

do i=1,x
	do j = 1,x
		B2(j,i) = W(j,i+x)
	end do
end do

write(*,*) "BI:"

do i=1,x
	do j=1,x
		write(*,*) B2(i,j), " "
	end do
	
end do

write(*,*) "W:"
	 
do i=1,x
	do j=1,x*2
		write(*,*) W(i,j)," "
	end do
end do

RETURN  
end SUBROUTINE invertMatrix
end module