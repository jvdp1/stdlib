program harvest_kinds
 use iso_fortran_env, only: integer_kinds, real_kinds, logical_kinds

 implicit none

 integer :: i, un
 character(len=:), allocatable :: cdummy

 open(newunit = un, file = 'common_gen.fypp', action = 'write')

 cdummy = ''
 do i =1, size(integer_kinds)-1
  cdummy = cdummy//'"int'//int2char(integer_kinds(i))//'", '
 end do
 cdummy = cdummy//'"int'//int2char(integer_kinds(size(integer_kinds)))//'"'
 cdummy = '#:set INT_KINDS = ['//cdummy//']'

 write(un, '(a)')cdummy

 cdummy = ''
 do i =1, size(real_kinds)-1
  cdummy = cdummy//'"real'//int2char(real_kinds(i))//'", '
 end do
 cdummy = cdummy//'"real'//int2char(real_kinds(i))//'"'
 cdummy = '#:set REAL_KINDS = ['//cdummy//']'

 write(un, '(a)')cdummy

! cdummy = ''
! do i =1, size(logical_kinds)-1
!  cdummy = cdummy//'"lg'//int2char(logical_kinds(i))//'", '
! end do
! cdummy = cdummy//'"lg'//int2char(logical_kinds(i))//'"'
! cdummy = '#:set LOG_KINDS = ['//cdummy//']'
!
! write(un, '(a)')cdummy

 close(un)


 open(newunit = un, file = 'stdlib_kinds.f90', action = 'write')
 write(un,'(a)')'module stdlib_kinds'
 write(un,'(a)')'use iso_fortran_env, only: integer_kinds, real_kinds'
 write(un,'(a)')'use iso_c_binding, only: c_bool'
 write(un,'(a)')'implicit none'
 write(un,'(a)')'private'

 write(un,'(a)')'public :: c_bool'
 write(un,'(a)')'integer, parameter, public :: lk = kind(.true.)'

 do i =1, size(integer_kinds)
  write(un, '(a,i0,a)')'integer, parameter, public :: '//'int'//int2char(integer_kinds(i))&
                  //' = integer_kinds(',i,')'
 end do

 do i =1, size(real_kinds)
  write(un, '(a,i0,a)')'integer, parameter, public :: '//'real'//int2char(real_kinds(i))&
                  //' = real_kinds(',i,')'
 end do

 write(un,'(a)')'integer, parameter, public :: dop = kind(1.0d0) !default_output_real_kind'

 write(un,'(a)')'end module stdlib_kinds'

 close(un)



contains

pure function int2char(value) result(intchar)
    integer, intent(in) :: value
    character(len=:), allocatable:: intchar
    
    character(len=4) :: dummy

    write(dummy,'(1i4)') value

    intchar = trim(adjustl(dummy))

end function

end program
