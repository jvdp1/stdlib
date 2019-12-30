module stdlib_experimental_io
use iso_fortran_env, only: int8, int16, int32, int64, sp=>real32, dp=>real64, qp=>real128
implicit none
private
public :: loadtxt, savetxt

interface loadtxt
  module procedure i8loadtxt
  module procedure i16loadtxt
  module procedure i32loadtxt
  module procedure i64loadtxt
  module procedure sloadtxt
  module procedure dloadtxt
  module procedure qloadtxt
end interface

interface savetxt
  module procedure i8loadtxt
  module procedure i16loadtxt
  module procedure i32loadtxt
  module procedure i64loadtxt
  module procedure ssavetxt
  module procedure dsavetxt
  module procedure qsavetxt
end interface

contains

#undef SUB_LOADTXT_
#undef SUB_SAVETXT_
#undef TYPE_
#define SUB_LOADTXT_ i8loadtxt
#define SUB_SAVETXT_ i8savetxt
#define TYPE_ integer(int8)
#include "stdlib_experimental_io.inc"

#undef SUB_LOADTXT_
#undef SUB_SAVETXT_
#undef TYPE_
#define SUB_LOADTXT_ i16loadtxt
#define SUB_SAVETXT_ i16savetxt
#define TYPE_ integer(int16)
#include "stdlib_experimental_io.inc"

#undef SUB_LOADTXT_
#undef SUB_SAVETXT_
#undef TYPE_
#define SUB_LOADTXT_ i32loadtxt
#define SUB_SAVETXT_ i32savetxt
#define TYPE_ integer(int32)
#include "stdlib_experimental_io.inc"

#undef SUB_LOADTXT_
#undef SUB_SAVETXT_
#undef TYPE_
#define SUB_LOADTXT_ i64loadtxt
#define SUB_SAVETXT_ i64savetxt
#define TYPE_ integer(int64)
#include "stdlib_experimental_io.inc"


#undef SUB_LOADTXT_
#undef SUB_SAVETXT_
#undef TYPE_
#define SUB_LOADTXT_ sloadtxt
#define SUB_SAVETXT_ ssavetxt
#define TYPE_ real(sp)
#include "stdlib_experimental_io.inc"

#undef SUB_LOADTXT_
#undef SUB_SAVETXT_
#undef TYPE_
#define SUB_LOADTXT_ dloadtxt
#define SUB_SAVETXT_ dsavetxt
#define TYPE_ real(dp)
#include "stdlib_experimental_io.inc"

#undef SUB_LOADTXT_
#undef SUB_SAVETXT_
#undef TYPE_
#define SUB_LOADTXT_ qloadtxt
#define SUB_SAVETXT_ qsavetxt
#define TYPE_ real(qp)
#include "stdlib_experimental_io.inc"



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
