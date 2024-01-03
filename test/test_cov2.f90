module test_cov2
    use,intrinsic :: ieee_arithmetic, only : ieee_is_nan
    use stdlib_kinds, only: sp, dp, int32, int64
    use stdlib_stats, only: cov
    use testdrive, only: new_unittest, unittest_type, error_type, check

    implicit none

    private
    public :: collect_cov2, initialize_test_data

    real(sp), parameter :: sptol = 1000 * epsilon(1._sp)
    real(dp), parameter :: dptol = 1000 * epsilon(1._dp)

    real(dp), parameter :: d1(5) = [1._dp, 2._dp, 3._dp, 4._dp, 5._dp]
    real(dp), parameter :: d(4,3) = reshape( &
        [1._dp, 3._dp, 5._dp, 7._dp, &
         2._dp, 4._dp, 6._dp, 8._dp, &
         9._dp, 10._dp, 11._dp, 12._dp], [4, 3])

    complex(dp) :: c1(5) = [(0.57706_dp, 0.00000_dp), &
                            (0.00000_dp, 1.44065_dp), &
                            (1.26401_dp, 0.00000_dp), &
                            (0.00000_dp, 0.88833_dp), &
                            (1.14352_dp, 0.00000_dp)]
    complex(dp) :: c2(5,3)

    real(sp) :: x1(5) = real(d1, sp)
    real(sp) :: x2(4,3) = real(d, sp)

    real(dp) :: dx1(5) = d1
    real(dp) :: dx2(4,3) = d
    real(dp) :: dy2(4,2) = d(1:4,2:3)

    integer(int32) :: i1(5) = d1
    integer(int32) :: i2(4,3) = d

    integer(int64) :: di1(5) = d1
    integer(int64) :: di2(4,3) = d

contains

    subroutine initialize_test_data()

        c2(:,1) = c1
        c2(:,2) = c1 * 3
        c2(:,3) = c1 * 1.5

    end subroutine initialize_test_data

    !> Collect all exported unit tests
    subroutine collect_cov2(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest('cov2_dp', test_cov2_dp) &
        ]

    end subroutine collect_cov2

    subroutine test_cov2_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1

        print*,'aaa ', cov(dx2, dy2)



        !call check(error, all(abs(cov(dx2, order, 2)) < dptol))
    end subroutine


end module test_cov2


program tester

    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_cov2, only: collect_cov2, initialize_test_data
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("cov2", collect_cov2) &
        ]

    call initialize_test_data()

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if

end program tester
