! SPDX-Identifier: MIT
module test_insert_at
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_string_type, only: string_type, operator(//), operator(==)
    use stdlib_stringlist_type, only: stringlist_type, fidx, bidx, list_head, list_tail, operator(==)
    use stdlib_strings, only: to_string
    implicit none
    private
    public :: collect_insert_at

contains

    !> Collect all exported unit tests
    subroutine collect_insert_at(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_insert_at_string_1", test_insert_at_string_1) &
            , new_unittest("test_insert_at_string_2", test_insert_at_string_2) &
            , new_unittest("test_insert_at_string_3", test_insert_at_string_3) &
            , new_unittest("test_insert_at_array", test_insert_at_array) &
            , new_unittest("test_insert_at_list", test_insert_at_list) &
            , new_unittest("test_constructor", test_constructor) &
            ]
    end subroutine collect_insert_at

    subroutine test_insert_at_string_1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(stringlist_type)           :: work_list
        integer                         :: i, current_length
        character(len=:), allocatable   :: string
        integer, parameter              :: first = -100
        integer, parameter              :: last = 1

        work_list = stringlist_type()
        call check(error, work_list%len() == 0, "test_insert_at_string_1: constructor" )
        if (allocated(error)) return

        write (*,*) "test_insert_at_string_1: Starting test case 1!"
        current_length = 0
        do i = first, last
            string = to_string( i )
            call work_list%insert_at( fidx(i), string )
            current_length = current_length + 1

            call check(error, work_list%get( fidx(1) ) == string, "test_insert_at_string_1:&
                                    & get fidx(1) " // string )
            if (allocated(error)) return
            call check(error, work_list%get( list_head ) == string, "test_insert_at_string_1:&
                                    & get list_head " // string )
            if (allocated(error)) return
            call check(error, work_list%get( bidx(current_length) ) == string, "test_insert_at_string_1: get&
                                    & bidx(current_length) " // string )
            if (allocated(error)) return
            call check(error, work_list%get( list_tail ) == to_string(first), "test_insert_at_string_1: get&
                                    & list_tail " // string )
            if (allocated(error)) return

            call check(error, work_list%len() == current_length, "test_insert_at_string_1: length check "&
                                    & // to_string( current_length ) )
            if (allocated(error)) return

        end do

        ! compare work_list with [1, 0, -1, ..., ..., -99, -100]
        call compare_list( work_list, last, first - 1, 1, error)
        if (allocated(error)) return

        call work_list%clear()
        call work_list%clear()
        current_length = 0

        write (*,*) "test_insert_at_string_1: Starting test case 2!"
        do i = first, last
            string = to_string( i )
            call work_list%insert_at( bidx(i), string )
            current_length = current_length + 1

            call check(error, work_list%get( bidx(1) ) == string, "test_insert_at_string_1:&
                                    & get bidx(1) " // string )
            if (allocated(error)) return
            call check(error, work_list%get( list_tail ) == string, "test_insert_at_string_1:&
                                    & get list_tail " // string )
            if (allocated(error)) return
            call check(error, work_list%get( fidx(current_length) ) == string, "test_insert_at_string_1: get&
                                    & fidx(current_length) " // string )
            if (allocated(error)) return
            call check(error, work_list%get( list_head ) == to_string(first), "test_insert_at_string_1: get&
                                    & list_head " // string )
            if (allocated(error)) return

            call check(error, work_list%len() == current_length, "test_insert_at_string_1: length check "&
                                    & // to_string( current_length ) )
            if (allocated(error)) return

        end do

        ! compare work_list with [-100, -99, ..., ..., 0, 1]
        call compare_list( work_list, first, last + 1, 2, error)
        if (allocated(error)) return

    end subroutine test_insert_at_string_1

    subroutine test_insert_at_string_2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(stringlist_type)           :: work_list
        integer                         :: i, current_length
        character(len=:), allocatable   :: string
        integer, parameter              :: first = 2
        integer, parameter              :: last = 200

        write (*,*) "test_insert_at_string_2: Starting test case 1!"

        current_length = 0
        do i = first, last, 2
            string = to_string( i )
            call work_list%insert_at( fidx(i), string )
            current_length = current_length + 1

            call check(error, work_list%get( fidx(current_length) ) == string, "test_insert_at_string_2:&
                                    & get fidx(current_length) " // string )
            if (allocated(error)) return
            call check(error, work_list%get( fidx(1) ) == to_string(first), "test_insert_at_string_2:&
                                    & get fidx(1) " // string )
            if (allocated(error)) return
            call check(error, work_list%get( list_head ) == to_string(first), "test_insert_at_string_2:&
                                    & get list_head " // string )
            if (allocated(error)) return
            call check(error, work_list%get( bidx(1) ) == string, "test_insert_at_string_2:&
                                    & get bidx(1) " // string )
            if (allocated(error)) return
            call check(error, work_list%get( bidx(current_length) ) == to_string(first), "test_insert_at_string_2: get&
                                    & bidx(current_length) " // string )
            if (allocated(error)) return
            call check(error, work_list%get( list_tail ) == string, "test_insert_at_string_2: get&
                                    & list_tail " // string )
            if (allocated(error)) return

            call check(error, work_list%len() == current_length, "test_insert_at_string_2: length check "&
                                    & // to_string( current_length ) )
            if (allocated(error)) return

        end do

        write (*,*) "test_insert_at_string_2: Starting test case 2!"

        do i = first - 1, last - 1, 2
            string = to_string( i )
            call work_list%insert_at( fidx(i), string )
            current_length = current_length + 1

            call check(error, work_list%get( fidx(i) ) == string, "test_insert_at_string_2:&
                                    & get fidx(current_length) " // string )
            if (allocated(error)) return
            call check(error, work_list%get( fidx(1) ) == to_string(first - 1), "test_insert_at_string_2:&
                                    & get fidx(1) " // string )
            if (allocated(error)) return
            call check(error, work_list%get( list_head ) == to_string(first - 1), "test_insert_at_string_2:&
                                    & get list_head " // string )
            if (allocated(error)) return
            call check(error, work_list%get( bidx(1) ) == to_string(last), "test_insert_at_string_2:&
                                    & get bidx(1) " // string )
            if (allocated(error)) return
            call check(error, work_list%get( bidx(current_length) ) == to_string(first - 1), "test_insert_at_string_2: get&
                                    & bidx(current_length) " // string )
            if (allocated(error)) return
            call check(error, work_list%get( list_tail ) == to_string(last), "test_insert_at_string_2: get&
                                    & list_tail " // string )
            if (allocated(error)) return

            call check(error, work_list%len() == current_length, "test_insert_at_string_2: length check "&
                                    & // to_string( current_length ) )
            if (allocated(error)) return

        end do

        ! compare work_list with [1, 2, ..., ..., 199, 200]
        call compare_list( work_list, first - 1, last + 1, 3, error)
        if (allocated(error)) return

    end subroutine test_insert_at_string_2

    subroutine test_insert_at_string_3(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(stringlist_type)           :: work_list
        integer                         :: i, current_length
        character(len=:), allocatable   :: string
        integer, parameter              :: first = 2
        integer, parameter              :: last = 200

        write (*,*) "test_insert_at_string_3: Starting test case 1!"

        current_length = 0
        do i = first, last, 2
            string = to_string( i )
            call work_list%insert_at( bidx(i), string )
            current_length = current_length + 1

            call check(error, work_list%get( bidx(current_length) ) == string, "test_insert_at_string_3:&
                                    & get bidx(current_length) " // string )
            if (allocated(error)) return
            call check(error, work_list%get( bidx(1) ) == to_string(first), "test_insert_at_string_3:&
                                    & get bidx(1) " // string )
            if (allocated(error)) return
            call check(error, work_list%get( list_tail ) == to_string(first), "test_insert_at_string_3:&
                                    & get list_tail " // string )
            if (allocated(error)) return
            call check(error, work_list%get( fidx(1) ) == string, "test_insert_at_string_3:&
                                    & get fidx(1) " // string )
            if (allocated(error)) return
            call check(error, work_list%get( fidx(current_length) ) == to_string(first), "test_insert_at_string_3: get&
                                    & fidx(current_length) " // string )
            if (allocated(error)) return
            call check(error, work_list%get( list_head ) == string, "test_insert_at_string_3: get&
                                    & list_head " // string )
            if (allocated(error)) return

            call check(error, work_list%len() == current_length, "test_insert_at_string_3: length check "&
                                    & // to_string( current_length ) )
            if (allocated(error)) return

        end do

        write (*,*) "test_insert_at_string_3: Starting test case 2!"

        do i = first - 1, last - 1, 2
            string = to_string( i )
            call work_list%insert_at( bidx(i), string )
            current_length = current_length + 1

            call check(error, work_list%get( bidx(i) ) == string, "test_insert_at_string_3:&
                                    & get bidx(current_length) " // string )
            if (allocated(error)) return
            call check(error, work_list%get( bidx(1) ) == to_string(first - 1), "test_insert_at_string_3:&
                                    & get bidx(1) " // string )
            if (allocated(error)) return
            call check(error, work_list%get( list_tail ) == to_string(first - 1), "test_insert_at_string_3:&
                                    & get list_tail " // string )
            if (allocated(error)) return
            call check(error, work_list%get( fidx(1) ) == to_string(last), "test_insert_at_string_3:&
                                    & get fidx(1) " // string )
            if (allocated(error)) return
            call check(error, work_list%get( fidx(current_length) ) == to_string(first - 1), "test_insert_at_string_3: get&
                                    & fidx(current_length) " // string )
            if (allocated(error)) return
            call check(error, work_list%get( list_head ) == to_string(last), "test_insert_at_string_3: get&
                                    & list_head " // string )
            if (allocated(error)) return

            call check(error, work_list%len() == current_length, "test_insert_at_string_3: length check "&
                                    & // to_string( current_length ) )
            if (allocated(error)) return

        end do

        ! compare work_list with [200, 199, ..., ..., 2, 1]
        call compare_list( work_list, last, first - 2, 4, error)
        if (allocated(error)) return

    end subroutine test_insert_at_string_3

    subroutine test_insert_at_array(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(stringlist_type)           :: work_list
        type(stringlist_type)           :: reference_list
        integer                         :: i, j
        integer, parameter              :: first = -100
        integer, parameter              :: last = 100
        integer, parameter              :: stride = 4

        write (*,*) "test_insert_at_array:    Starting work_list!"

        call work_list%insert_at( list_head, &
                        & [ ( string_type( to_string(j) ), j = first, first + stride - 1 ) ] )

        call compare_list( work_list, first, first + stride, 5, error)
        if (allocated(error)) return

        call work_list%insert_at( list_tail, &
                        & [ ( string_type( to_string(j) ), j = last - stride, last - 1 ) ] )

        do i = first + stride, last - stride - 1, stride
            call work_list%insert_at( fidx( i - first + 1 ), &
                        & [ ( string_type( to_string(j) ), j = i, i + stride - 1 ) ] )
        end do

        call work_list%insert_at( list_tail, [ to_string(last) ] )

        call compare_list( work_list, first, last + 1, 6, error )
        if (allocated(error)) return

        write (*,*) "test_insert_at_array:    Starting reference_list!"

        call reference_list%insert_at( list_tail, &
                        & [ ( string_type( to_string(j) ), j = last - stride + 1, last ) ] )

        call compare_list( reference_list, last - stride + 1, last + 1, 7, error )
        if (allocated(error)) return

        call reference_list%insert_at( list_head, &
                        & [ ( string_type( to_string(j) ), j = first + 1, first + stride ) ] )

        do i = last - stride, first + stride + 1, -1 * stride
            call reference_list%insert_at( bidx( last - i + 1 ), &
                        & [ ( string_type( to_string(j) ), j = i - stride + 1, i ) ] )
        end do

        call reference_list%insert_at( list_head, [ to_string(first) ] )

        call compare_list( reference_list, first, last + 1, 8, error )
        if (allocated(error)) return

    end subroutine test_insert_at_array

    subroutine test_insert_at_list(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(stringlist_type)           :: work_list, reference_list
        type(stringlist_type)           :: temp_list
        integer                         :: i, j
        integer, parameter              :: first = -100
        integer, parameter              :: last = 100
        integer, parameter              :: stride = 4

        write (*,*) "test_insert_at_list:     Starting work_list!"

        call temp_list%clear()
        do j = first, first + stride - 1
            call temp_list%insert_at( list_tail, string_type( to_string(j) ) )
        end do

        call work_list%insert_at(list_head, temp_list)
        call compare_list( work_list, first, first + stride, 9, error )
        if (allocated(error)) return

        call temp_list%clear()
        do j = last - 1, last - stride, -1
            call temp_list%insert_at( list_head, string_type( to_string(j) ) )
        end do

        call work_list%insert_at(list_tail, temp_list)

        do i = first + stride, last - stride - 1, stride
            call temp_list%clear()
            do j = i, i + stride - 1
                call temp_list%insert_at( list_tail, to_string(j) )
            end do
            call work_list%insert_at( fidx( i - first + 1 ), temp_list )

        end do

        call temp_list%clear()
        call temp_list%insert_at( list_head, to_string(last) )
        call work_list%insert_at( list_tail, temp_list )

        call compare_list( work_list, first, last + 1, 10, error )
        if (allocated(error)) return

        write (*,*) "test_insert_at_list:     Starting reference_list!"

        call temp_list%clear()
        do j = last - stride + 1, last
            call temp_list%insert_at( list_tail, to_string(j) )
        end do

        call reference_list%insert_at( list_tail, temp_list )
        call compare_list( reference_list, last - stride + 1, last + 1, 11, error )
        if (allocated(error)) return

        call temp_list%clear()
        do j = first + 1, first + stride
            call temp_list%insert_at( list_tail, string_type( to_string(j) ) )
        end do

        call reference_list%insert_at( list_head, temp_list )

        do i = last - stride, first + stride + 1, -1 * stride
            call temp_list%clear()
            do j = i - stride + 1, i
                call temp_list%insert_at( list_tail, to_string(j) )
            end do
            call reference_list%insert_at( bidx( last - i + 1 ), temp_list )

        end do

        call temp_list%clear()
        call temp_list%insert_at( list_tail, to_string(first) )
        call reference_list%insert_at( list_head, temp_list )

        call compare_list( reference_list, first, last + 1, 12, error )
        if (allocated(error)) return

    end subroutine test_insert_at_list

    subroutine test_constructor(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(stringlist_type)           :: work_list
        character(len=4), allocatable   :: carray(:)
        type(string_type), allocatable  :: sarray(:)

        write (*,*) "test_constructor:        Starting test case 1!"
        work_list = stringlist_type()
        allocate( carray(0) )
        allocate( sarray(0) )
        call check(error, work_list == carray, "test_constructor:&
            & test_case 1 work_list == carray" )
        if (allocated(error)) return
        call check(error, work_list == sarray, "test_constructor:&
            & test_case 1 work_list == sarray" )
        if (allocated(error)) return

        write (*,*) "test_constructor:        Starting test case 2!"
        carray = [ '#1', '#2', '#3', '#4' ]
        sarray = [ string_type('#1'), string_type('#2'), string_type('#3'), string_type('#4') ]
        work_list = stringlist_type( carray )
        call check(error, work_list == carray, "test_constructor:&
            & test_case 2 work_list == carray" )
        if (allocated(error)) return
        call check(error, work_list == sarray, "test_constructor:&
            & test_case 2 work_list == sarray" )
        if (allocated(error)) return

        write (*,*) "test_constructor:        Starting test case 3!"
        work_list = stringlist_type( sarray )
        call check(error, work_list == carray, "test_constructor:&
            & test_case 3 work_list == carray" )
        if (allocated(error)) return
        call check(error, work_list == sarray, "test_constructor:&
            & test_case 3 work_list == sarray" )
        if (allocated(error)) return

    end subroutine test_constructor

    ! compares input stringlist 'list' with an array of consecutive integers
    ! array is 'first' inclusive and 'last' exclusive
    subroutine compare_list(list, first, last, call_number, error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(stringlist_type), intent(in)   :: list
        integer, intent(in)                 :: first, last, call_number
        integer                             :: i, j

        call check(error, abs( last - first ) == list%len(), "compare_list: length mis-match&
                                        & call_number " // to_string( call_number ) )
        if (allocated(error)) return

        j = merge(-1, 1, last < first)
        do i = 1, list%len()
            call check(error, list%get( fidx(i) ) == to_string( first + ( ( i - 1 ) * j ) ), &
                                    & "compare_list: call_number " // to_string( call_number ) &
                                    & // " fidx( " // to_string( i ) // " )")
            if (allocated(error)) return
            call check(error, list%get( bidx(i) ) == to_string( last - ( i * j ) ), &
                                    & "compare_list: call_number " // to_string( call_number ) &
                                    & // " bidx( " // to_string( i ) // " )")
            if (allocated(error)) return
        end do

    end subroutine compare_list

end module test_insert_at


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_insert_at, only : collect_insert_at
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("insert_at", collect_insert_at) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program tester
