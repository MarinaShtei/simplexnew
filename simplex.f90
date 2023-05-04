! simplex.h
MODULE simplex
!IMPLICIT NONE

!CONTAINS

	!ֱֱUSE globals
	use bublesort_d  
	use copy_matrix  
	use print_original_system
	use copy_to_initial_matrix !globals
	use print_no_solution
	use print_initial_solution
	use print_solution
	use bublesort 
use Initial_set_d !globals
use set_Initial_A_aux !globals
use copy_submatrix
use erase_epsilons_matrix
use matrix_mult
use matrix_vector_mult
use erase_epsilons_vector
use compute_Initial_cb_Initial_cd !globals
use vector_subtract
use find_min_value
use find_Initial_most_negative
use find_Initial_exiting_id
use set_d !globals
use set_A_aux   !globals        
use compute_cb_cd !globals
use find_most_negative
use find_exiting_id
use copy_vector
use vector_matrix_mult
use print_simplex_params
use swap_rows
use print_result
use stage2_swap_colums !globals
use swap_colums
use Initial_swap_colums !globals
  

END MODULE