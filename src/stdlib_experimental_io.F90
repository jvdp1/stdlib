!Command line used to auto-generate the module:
! jin2for --generate-for gfortran stdlib_experimental_io.F90

module stdlib_experimental_io
use iso_fortran_env, only: sp=>real32, dp=>real64, qp=>real128
implicit none
private
public :: loadtxt, savetxt

interface loadtxt
{% for type in ["sp", "dp", "qp"] -%}
  module procedure loadtxt_{{type}}
{% endfor %}
end interface

interface savetxt
{% for type in ["sp", "dp", "qp"] -%}
  module procedure savetxt_{{type}}
{% endfor %}
end interface

contains

{% for type in ["sp", "dp", "qp"] -%}
subroutine loadtxt_{{type}}(filename, d)
  ! Loads a 2D array from a text file.
  !
  ! Arguments
  ! ---------
  !
  ! Filename to load the array from
  character(len=*), intent(in) :: filename
  
  ! The array 'd' will be automatically allocated with the correct dimensions
  real({{type}}), allocatable, intent(out) :: d(:,:)
  !
  ! Example
  ! -------
  !
  ! real({{type}}), allocatable :: data(:, :)
  ! call loadtxt("log.txt", data)  ! 'data' will be automatically allocated
  !
  ! Where 'log.txt' contains for example::
  !
  !     1 2 3
  !     2 4 6
  !     8 9 10
  !     11 12 13
  !     ...
  !
  integer :: s
  integer :: nrow,ncol,i
  
  open(newunit=s, file=filename, status="old")
  
  ! determine number of columns
  ncol = number_of_columns(s)
  
  ! determine number or rows
  nrow = number_of_rows_numeric(s)
  
  allocate(d(nrow, ncol))
  do i = 1, nrow
    read(s, *) d(i, :)
  end do
  close(s)

end subroutine

{% endfor %}

{% for type in ["sp", "dp", "qp"] -%}
subroutine savetxt_{{type}}(filename, d)
  ! Saves a 2D array into a textfile.
  !
  ! Arguments
  ! ---------
  !
  character(len=*), intent(in) :: filename  ! File to save the array to
  real({{type}}), intent(in) :: d(:,:)           ! The 2D array to save
  !
  ! Example
  ! -------
  !
  ! real({{type}}) :: data(3, 2)
  ! call savetxt("log.txt", data)
  
  integer :: s, i

  open(newunit=s, file=filename, status="replace")
  do i = 1, size(d, 1)
    write(s, *) d(i, :)
  end do
  close(s)

end subroutine
{% endfor %}

integer function number_of_columns(s)
 ! determine number of columns
 integer,intent(in)::s

 integer :: ios
 character :: c
 logical :: lastwhite

 rewind(s)
 number_of_columns = 0
 lastwhite = .true.
 do
    read(s, '(a)', advance='no', iostat=ios) c
    if (ios /= 0) exit
    if (lastwhite .and. .not. whitechar(c)) number_of_columns = number_of_columns + 1
    lastwhite = whitechar(c)
 end do
 rewind(s)

end function

integer function number_of_rows_numeric(s)
 ! determine number or rows
 integer,intent(in)::s
 integer :: ios
 
 real::r
 
 rewind(s)
 number_of_rows_numeric = 0
 do
    read(s, *, iostat=ios) r
    if (ios /= 0) exit
    number_of_rows_numeric = number_of_rows_numeric + 1
 end do
 
 rewind(s)

end function

logical function whitechar(char) ! white character
! returns .true. if char is space (32) or tab (9), .false. otherwise
character, intent(in) :: char
if (iachar(char) == 32 .or. iachar(char) == 9) then
    whitechar = .true.
else
    whitechar = .false.
end if
end function

end module
