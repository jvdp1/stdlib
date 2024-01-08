! SPDX-Identifier: MIT
module test_append_prepend
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_string_type, only: string_type, operator(//), operator(==)
    use stdlib_stringlist_type, only: stringlist_type, fidx, bidx, list_head, &
                            & list_tail, operator(//), operator(==), operator(/=)
    use stdlib_strings, only: to_string
    implicit none
    private
    public :: collect_append_prepend

contains

    !> Collect all exported unit tests
    subroutine collect_append_prepend(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_append_prepend_string", test_append_prepend_string) &
            , new_unittest("test_append_prepend_array", test_append_prepend_array) &
            , new_unittest("test_append_prepend_list", test_append_prepend_list) &
            ]
    end subroutine collect_append_prepend

    subroutine test_append_prepend_string(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(stringlist_type)           :: work_list
        type(stringlist_type)           :: reference_list
        integer                         :: i
        integer, parameter              :: first = -100
        integer, parameter              :: last = 100
        character(len=:), allocatable   :: string

        do i = first, last
            string = to_string(i)
            work_list = work_list // string
            call check(error, work_list%get( fidx( i - first + 1 ) ) == string_type( string ), &
                        & "test_append_prepend_string: get fidx( i - first + 1 ) " // string )
            if (allocated(error)) return

        end do

        call compare_list( work_list, first, last + 1, 1, error )
        call check(error, work_list == [ ( string_type( to_string(i) ), i = first, last ) ], &
                        & "test_append_prepend_string: work_list ==&
                        & [ ( string_type( to_string(i) ), i = first, last ) ]" )
        if (allocated(error)) return
        call check(error, [ ( string_type( to_string(i) ), i = first, last ) ] == work_list, &
                        & "test_append_prepend_string: [ ( string_type( to_string(i) ),&
                        & i = first, last ) ] == work_list" )
        if (allocated(error)) return

        do i = last, first, -1
            call check(error, work_list /= reference_list, "test_append_prepend_string:&
                                    & work_list /= reference_list" )
            if (allocated(error)) return
            call check(error, reference_list /= work_list, "test_append_prepend_string:&
                                    & reference_list /= work_list" )
            if (allocated(error)) return

            string = to_string(i)
            reference_list = string_type( string ) // reference_list
            call check(error, reference_list%get( bidx( last - i + 1 ) ) == string, &
                                    & "test_append_prepend_string: get bidx( last - i + 1 ) " // string )
            if (allocated(error)) return

        end do

        call compare_list( reference_list, first, last + 1, 2, error )
        call check(error, reference_list == [ ( string_type( to_string(i) ), i = first, last ) ], "test_append_prepend_string:&
                    & reference_list == [ ( string_type( to_string(i) ), i = first, last ) ]" )
        if (allocated(error)) return
        call check(error, [ ( string_type( to_string(i) ), i = first, last ) ] == reference_list, &
                    & "test_append_prepend_string: [ ( string_type( to_string(i) ), i = first, last ) ] == reference_list" )
        if (allocated(error)) return

        call check(error, work_list == reference_list, "test_append_prepend_string:&
                                    & work_list == reference_list" )
        if (allocated(error)) return
        call check(error, reference_list == work_list, "test_append_prepend_string:&
                                    & reference_list == work_list" )
        if (allocated(error)) return

    end subroutine test_append_prepend_string

    subroutine test_append_prepend_array(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(stringlist_type)           :: work_list
        type(stringlist_type)           :: reference_list
        integer                         :: i, j
        integer, parameter              :: first = -100
        integer, parameter              :: last = 100
        integer, parameter              :: stride = 10

        do i = first, last - 1, stride
            work_list = work_list // [ ( string_type( to_string(j) ), j = i, i + stride - 1) ]
            call check(error, work_list == [ ( string_type( to_string(j) ), j = first, i + stride - 1) ], &
                    & "test_append_prepend_array: work_list ==&
                    & [ ( string_type( to_string(j) ), j = first, i + stride - 1) ]" )
            if (allocated(error)) return

        end do

        work_list = work_list // to_string(last)

        call compare_list( work_list, first, last + 1, 3, error )
        call check(error, work_list == [ ( string_type( to_string(i) ), i = first, last) ], &
                    & "test_append_prepend_array: work_list ==&
                    & [ ( string_type( to_string(i) ), i = first, last) ]" )
        if (allocated(error)) return
        call check(error, [ ( string_type( to_string(i) ), i = first, last) ] == work_list, &
                    & "test_append_prepend_array: [ ( string_type( to_string(i) ), i = first, last) ]&
                    & == work_list" )
        if (allocated(error)) return

        do i = last, first + 1, -1 * stride
            call check(error, work_list /= reference_list, "test_append_prepend_array:&
                                    & work_list /= reference_list" )
            if (allocated(error)) return
            call check(error, reference_list /= work_list, "test_append_prepend_array:&
                                    & reference_list /= work_list" )
            if (allocated(error)) return

            reference_list = [ ( string_type( to_string(j) ), j = i - stride + 1, i ) ] &
                            & // reference_list
            call check(error, reference_list == &
                            & [ ( string_type( to_string(j) ), j = i - stride + 1, last ) ], &
                            & "test_append_prepend_array: reference_list ==&
                            & [ ( string_type( to_string(j) ), j = i - stride + 1, last ) ]" )
            if (allocated(error)) return

        end do

        reference_list = to_string(first) // reference_list

        call compare_list( reference_list, first, last + 1, 4, error )
        call check(error, [ ( string_type( to_string(i) ), i = first, last) ] == reference_list, &
                    & "test_append_prepend_array:&
                    & [ ( string_type( to_string(i) ), i = first, last) ] == reference_list" )
        if (allocated(error)) return
        call check(error, [ ( string_type( to_string(i) ), i = first, last) ] == reference_list, &
                    & "test_append_prepend_array: [ ( string_type( to_string(i) ), i = first, last) ]&
                    & == reference_list" )
        if (allocated(error)) return

        call check(error, work_list == reference_list, "test_append_prepend_array:&
                                    & work_list == reference_list" )
        if (allocated(error)) return
        call check(error, reference_list == work_list, "test_append_prepend_array:&
                                    & reference_list == work_list" )
        if (allocated(error)) return

    end subroutine test_append_prepend_array

    subroutine test_append_prepend_list(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(stringlist_type)           :: work_list, reference_list
        type(stringlist_type)           :: temp_list
        integer                         :: i, j
        integer, parameter              :: first = -100
        integer, parameter              :: last = 100
        integer, parameter              :: stride = 10

        do i = first, last - 1, stride
            call temp_list%clear()
            do j = i, i + stride - 1
                call temp_list%insert_at( list_tail, string_type( to_string(j) ) )
            end do
            work_list = work_list // temp_list
            
            call check(error, work_list == [ ( string_type( to_string(j) ), j = first, i + stride - 1 ) ], &
                    & "test_append_prepend_list: work_list ==&
                    & [ ( to_string(j), j = first, i + stride - 1) ]" )
            if (allocated(error)) return

        end do

        work_list = work_list // to_string(last)

        call compare_list( work_list, first, last + 1, 5, error )
        call check(error, work_list == [ ( string_type( to_string(i) ), i = first, last) ], "test_append_prepend_list:&
                    & work_list == [ ( string_type( to_string(i) ), i = first, last) ]" )
        if (allocated(error)) return
        call check(error, [ ( string_type( to_string(i) ), i = first, last) ] == work_list, &
                    & "test_append_prepend_list: [ ( string_type( to_string(i) ), i = first, last) ]&
                    & == work_list" )
        if (allocated(error)) return

        do i = last, first + 1, -1 * stride
            call check(error, work_list /= reference_list, "test_append_prepend_list:&
                                    & work_list /= reference_list" )
            if (allocated(error)) return
            call check(error, reference_list /= work_list, "test_append_prepend_list:&
                                    & reference_list /= work_list" )
            if (allocated(error)) return

            call temp_list%clear()
            do j = i - stride + 1, i
                call temp_list%insert_at( list_tail, to_string(j) )
            end do
            reference_list = temp_list // reference_list

            call check(error, reference_list == &
                            & [ ( string_type( to_string(j) ), j = i - stride + 1, last ) ], &
                            & "test_append_prepend_list: reference_list ==&
                            & [ ( string_type( to_string(j) ), j = i - stride + 1, last ) ]" )
            if (allocated(error)) return

        end do

        reference_list = to_string(first) // reference_list

        call compare_list( reference_list, first, last + 1, 6, error )
        call check(error, [ ( string_type( to_string(i) ), i = first, last) ] == reference_list, &
                    & "test_append_prepend_list:&
                    & [ ( string_type( to_string(i) ), i = first, last) ] == reference_list" )
        if (allocated(error)) return
        call check(error, [ ( string_type( to_string(i) ), i = first, last) ] == reference_list, &
                    & "test_append_prepend_list: [ ( string_type( to_string(i) ), i = first, last) ]&
                    & == reference_list" )
        if (allocated(error)) return

        call check(error, work_list == reference_list, "test_append_prepend_list:&
                                    & work_list == reference_list" )
        if (allocated(error)) return
        call check(error, reference_list == work_list, "test_append_prepend_list:&
                                    & reference_list == work_list" )
        if (allocated(error)) return

    end subroutine test_append_prepend_list

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

end module test_append_prepend


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_append_prepend, only: collect_append_prepend
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("append_prepend", collect_append_prepend) &
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
