program main
  use stdlib_string_type !, only: string_type, index, assignment(=)
  use stdlib_stringlist_type !, only: stringlist_type, fidx
  implicit none
  integer :: i
  integer :: stat
  integer :: poslist
  character(256) :: arg
  type(stringlist_type) :: list_args
  character(:), allocatable :: source_file, output_file
  type(string_type) :: output
  character(:), allocatable :: outputc
  type(string_type) :: macros
  type(string_type) :: include_paths


  !Store arguments
  i = 0
  do
    call get_command_argument(i, arg, status = stat)
    if(stat.ne.0) exit
    i = i + 1
    call list_args%insert_at(fidx(i), trim(arg))
  enddo

  !Check for preprocessing files
  call endswith(list_args, '.fypp', source_file, poslist)

  !Run the preprocessor
  if(poslist.gt.0)then
    !find macros
    macros = findargs(list_args, '-D')
 
    !find include paths
    include_paths = findargs(list_args, '-I')
 
    !run fypp
    output = 'fypp -n '//source_file//'.fypp '//source_file//'.f90 '//macros//' '//include_paths
    call move(output, outputc)
    write(*,*)outputc
    call execute_command_line(outputc, exitstat = stat)
    if(stat.ne.0)error stop 'Issue with fypp cmd'
  
  endif

  !Run the actual compiler
  output = 'gfortran'
  do i = 2, list_args%len()
    if(i == poslist)then
      output = output //' '// source_file//'.f90'
    else
      output = output //' '// list_args%get(fidx(i))
    endif
  enddo

  call move(output, outputc)
  write(*,*)outputc

  call execute_command_line(outputc, exitstat = stat)
  if(stat.ne.0)error stop 'Issue with compiler cmd'

contains

subroutine endswith(stringlist, set, val, pos)
  type(stringlist_type), intent(in) :: stringlist
  character(len=*), intent(in) :: set
  character(len=:), allocatable, intent(out) :: val
  integer, intent(out) :: pos

  integer :: i, posc
  type(string_type) :: output

  pos = 0

  do i = 1, stringlist%len()
   output = stringlist%get(fidx(i))
   posc = index(output, set, back=.true.)

   if(posc.gt.0)then
    val = char(output, 1, posc-1)
    pos = i
    exit
   endif

  enddo

end subroutine

function findargs(stringlist, set) result(val)
  type(stringlist_type), intent(in) :: stringlist
  character(len=*), intent(in) :: set
  type(string_type) :: val

  integer :: i, pos
  type(string_type) :: output

  val = ''
  do i = 1, stringlist%len()
   output = stringlist%get(fidx(i))
   if(char(output, 1, len(set)) == set)then
    val = val //' '// output
   endif
  enddo

end function

end program main
