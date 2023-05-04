module print_no_solution
  implicit none
!public print_no_solution
contains
SUBROUTINE printNoSolution

write(*,*)"System A has NO solution"
RETURN
end SUBROUTINE printNoSolution
end module