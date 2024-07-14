! Matrix inversion example: subroutine interface
program example_inverse_subroutine
  use stdlib_linalg_constants, only: dp
  use stdlib_linalg, only: invert,eye
  implicit none

  real(dp) :: A(2,2), Am1(2,2)

  ! Input matrix (NB Fortran is column major! input columns then transpose)
  A = transpose(reshape( [4, 3, &
                          3, 2], [2,2] ))

  ! Invert matrix
  call invert(A,Am1)

  print *, '         |',Am1(1,:),'|' ! | -2  3 |
  print *, ' inv(A)= |',Am1(2,:),'|' ! |  3 -4 |

  ! Final check 
  print *, 'CHECK passed? ',matmul(A,Am1)==eye(2)

end program example_inverse_subroutine
