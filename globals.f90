module globals
  implicit none
    INTEGER :: N 
 INTEGER :: M 
   REAL , DIMENSION(128,128)::A
 REAL , DIMENSION(128,128):: B
 REAL , DIMENSION(128):: C
 REAL , DIMENSION(128,128):: D
 REAL , DIMENSION(128):: c1
 REAL , DIMENSION(128):: b1
 REAL , DIMENSION(128):: b_aux
 REAL , DIMENSION(128):: cb
 REAL , DIMENSION(128):: cbBI
 REAL , DIMENSION(64):: cbBID
 REAL , DIMENSION(128):: cd
 REAL , DIMENSION(128):: rd
 REAL , DIMENSION(128,128):: BID
 REAL , DIMENSION(128,128):: W
 REAL , DIMENSION(128,128):: BI
 REAL , DIMENSION(128,128):: BIA_aux

REAL , DIMENSION(128,128):: A_aux
REAL , DIMENSION(128):: BIb
REAL :: epsilon1;
  
 INTEGER , DIMENSION(128):: d1
 INTEGER , DIMENSION(128) :: d_aux
 INTEGER , DIMENSION(128):: basis
 INTEGER :: n1
 INTEGER :: m1

 INTEGER :: Initial_n
 REAL , DIMENSION(128,128):: Initial_W
 REAL , DIMENSION(128):: Initial_cb
 REAL , DIMENSION(128):: Initial_cd
 REAL , DIMENSION(128,128):: Initial_A
 REAL , DIMENSION(128,128):: Initial_A_aux
 REAL , DIMENSION(128):: Initial_c1
 REAL , DIMENSION(128):: Initial_c_aux
 INTEGER , DIMENSION(128):: Initial_basis
 REAL , DIMENSION(128,128):: Initial_D
 INTEGER , DIMENSION(128):: Initial_d1
 REAL , DIMENSION(128,128):: Initial_B
 REAL , DIMENSION(128):: Initial_BIb

 REAL , DIMENSION(128,128):: Initial_C
 REAL , DIMENSION(128):: Initial_b1
 REAL , DIMENSION(128):: Initial_b_aux
 REAL , DIMENSION(128):: Initial_rd
 REAL , DIMENSION(128,128):: Initial_BID
 REAL , DIMENSION(128,128):: Initial_BI
 REAL , DIMENSION(128)::Initial_cbBI
 REAL , DIMENSION(128):: Initial_cbBID
 REAL , DIMENSION(128,128):: Initial_BIA_aux
end module globals