program test_loadtxt
use iso_fortran_env, only: sp=>real32, dp=>real64 ,qp=>real128
use stdlib_experimental_io, only: loadtxt, savetxt
use stdlib_experimental_error, only: assert
implicit none

{% for type in ["sp", "dp", "qp"] -%}
call test_{{type}}()
{% endfor %}

contains

{% for type in ["sp", "dp", "qp"] -%}
subroutine test_{{type}}()
  real({{type}}) :: d(3, 2), e(2, 3)
  real({{type}}), allocatable :: d2(:, :)
  d = reshape([1, 2, 3, 4, 5, 6], [3, 2])
  call savetxt("tmp.dat", d)
  call loadtxt("tmp.dat", d2)
  call assert(all(shape(d2) == [3, 2]))
  call assert(all(abs(d-d2) < epsilon(1._{{type}})))

  e = reshape([1, 2, 3, 4, 5, 6], [2, 3])
  call savetxt("tmp.dat", e)
  call loadtxt("tmp.dat", d2)
  call assert(all(shape(d2) == [2, 3]))
  call assert(all(abs(e-d2) < epsilon(1._{{type}})))
end subroutine
{% endfor %}

end program
