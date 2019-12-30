program test_loadtxt
use iso_fortran_env, only: sp=>real32, dp=>real64 ,qp=>real128
use stdlib_experimental_io, only: loadtxt
implicit none

{% for type in ["sp", "dp", "qp"] -%}
real({{type}}), allocatable :: array_{{type}}(:, :)
{% endfor %}

{% for type in ["sp", "dp", "qp"] -%}
{% for example in ["1", "2", "3", "4"] -%}
call loadtxt("array{{example}}.dat", array_{{type}})
call print_array(array_{{type}})
{% endfor %}
{% endfor %}

contains

subroutine print_array(a)
class(*),intent(in) :: a(:, :)
integer :: i
print *, "Array, shape=(", size(a, 1), ",", size(a, 2), ")"

 select type(a)
  type is(real(sp))
   do i = 1, size(a, 1)
    print *, a(i, :)
   end do
  type is(real(dp))
   do i = 1, size(a, 1)
    print *, a(i, :)
   end do
  type is(real(qp))
   do i = 1, size(a, 1)
    print *, a(i, :)
   end do
  class default
   write(*,'(a)')'The proposed type is not supported'
   error stop
 end select

end subroutine

end program
