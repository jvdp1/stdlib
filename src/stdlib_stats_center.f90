submodule (stdlib_stats) stdlib_stats_center

  use, intrinsic:: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
  use stdlib_error, only: error_stop
  use stdlib_optval, only: optval
  implicit none

contains

      module function center_all_1_rsp_rsp (x, mask) result(res)
        real(sp), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(sp) :: res(size(x, 1))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = x - mean(x)

      end function center_all_1_rsp_rsp
      module function center_all_2_rsp_rsp (x, mask) result(res)
        real(sp), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(sp) :: res(size(x, 1), size(x, 2))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = x - mean(x)

      end function center_all_2_rsp_rsp
      module function center_all_3_rsp_rsp (x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(sp) :: res(size(x, 1), size(x, 2), size(x, 3))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = x - mean(x)

      end function center_all_3_rsp_rsp
      module function center_all_4_rsp_rsp (x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(sp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = x - mean(x)

      end function center_all_4_rsp_rsp
      module function center_all_1_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = x - mean(x)

      end function center_all_1_rdp_rdp
      module function center_all_2_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = x - mean(x)

      end function center_all_2_rdp_rdp
      module function center_all_3_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = x - mean(x)

      end function center_all_3_rdp_rdp
      module function center_all_4_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = x - mean(x)

      end function center_all_4_rdp_rdp
      module function center_all_1_csp_csp (x, mask) result(res)
        complex(sp), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        complex(sp) :: res(size(x, 1))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = x - mean(x)

      end function center_all_1_csp_csp
      module function center_all_2_csp_csp (x, mask) result(res)
        complex(sp), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        complex(sp) :: res(size(x, 1), size(x, 2))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = x - mean(x)

      end function center_all_2_csp_csp
      module function center_all_3_csp_csp (x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        complex(sp) :: res(size(x, 1), size(x, 2), size(x, 3))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = x - mean(x)

      end function center_all_3_csp_csp
      module function center_all_4_csp_csp (x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        complex(sp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = x - mean(x)

      end function center_all_4_csp_csp
      module function center_all_1_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        complex(dp) :: res(size(x, 1))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = x - mean(x)

      end function center_all_1_cdp_cdp
      module function center_all_2_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        complex(dp) :: res(size(x, 1), size(x, 2))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = x - mean(x)

      end function center_all_2_cdp_cdp
      module function center_all_3_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        complex(dp) :: res(size(x, 1), size(x, 2), size(x, 3))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = x - mean(x)

      end function center_all_3_cdp_cdp
      module function center_all_4_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        complex(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = x - mean(x)

      end function center_all_4_cdp_cdp

      module function center_all_1_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = real(x, dp) - mean(x)

      end function center_all_1_iint8_dp
      module function center_all_2_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = real(x, dp) - mean(x)

      end function center_all_2_iint8_dp
      module function center_all_3_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = real(x, dp) - mean(x)

      end function center_all_3_iint8_dp
      module function center_all_4_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = real(x, dp) - mean(x)

      end function center_all_4_iint8_dp
      module function center_all_1_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = real(x, dp) - mean(x)

      end function center_all_1_iint16_dp
      module function center_all_2_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = real(x, dp) - mean(x)

      end function center_all_2_iint16_dp
      module function center_all_3_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = real(x, dp) - mean(x)

      end function center_all_3_iint16_dp
      module function center_all_4_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = real(x, dp) - mean(x)

      end function center_all_4_iint16_dp
      module function center_all_1_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = real(x, dp) - mean(x)

      end function center_all_1_iint32_dp
      module function center_all_2_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = real(x, dp) - mean(x)

      end function center_all_2_iint32_dp
      module function center_all_3_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = real(x, dp) - mean(x)

      end function center_all_3_iint32_dp
      module function center_all_4_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = real(x, dp) - mean(x)

      end function center_all_4_iint32_dp
      module function center_all_1_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = real(x, dp) - mean(x)

      end function center_all_1_iint64_dp
      module function center_all_2_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = real(x, dp) - mean(x)

      end function center_all_2_iint64_dp
      module function center_all_3_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = real(x, dp) - mean(x)

      end function center_all_3_iint64_dp
      module function center_all_4_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = real(x, dp) - mean(x)

      end function center_all_4_iint64_dp


      module function center_1_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(size(x, 1))

        integer :: i
        real(sp), allocatable :: mean_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i) = &
                x(i) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_1_rsp_rsp
      module function center_2_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(size(x, 1), size(x, 2))

        integer :: i
        real(sp), allocatable :: mean_(:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :) = &
                x(i, :) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i) = &
                x(:, i) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_2_rsp_rsp
      module function center_3_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(size(x, 1), size(x, 2), size(x, 3))

        integer :: i
        real(sp), allocatable :: mean_(:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :, :) = &
                x(i, :, :) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i, :) = &
                x(:, i, :) - mean_
            end do
          case(3)
            do i = 1, size(x, 3)
              res(:, :, i) = &
                x(:, :, i) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_3_rsp_rsp
      module function center_4_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        integer :: i
        real(sp), allocatable :: mean_(:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :, :, :) = &
                x(i, :, :, :) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i, :, :) = &
                x(:, i, :, :) - mean_
            end do
          case(3)
            do i = 1, size(x, 3)
              res(:, :, i, :) = &
                x(:, :, i, :) - mean_
            end do
          case(4)
            do i = 1, size(x, 4)
              res(:, :, :, i) = &
                x(:, :, :, i) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_4_rsp_rsp
      module function center_1_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1))

        integer :: i
        real(dp), allocatable :: mean_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i) = &
                x(i) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_1_rdp_rdp
      module function center_2_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2))

        integer :: i
        real(dp), allocatable :: mean_(:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :) = &
                x(i, :) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i) = &
                x(:, i) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_2_rdp_rdp
      module function center_3_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3))

        integer :: i
        real(dp), allocatable :: mean_(:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :, :) = &
                x(i, :, :) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i, :) = &
                x(:, i, :) - mean_
            end do
          case(3)
            do i = 1, size(x, 3)
              res(:, :, i) = &
                x(:, :, i) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_3_rdp_rdp
      module function center_4_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        integer :: i
        real(dp), allocatable :: mean_(:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :, :, :) = &
                x(i, :, :, :) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i, :, :) = &
                x(:, i, :, :) - mean_
            end do
          case(3)
            do i = 1, size(x, 3)
              res(:, :, i, :) = &
                x(:, :, i, :) - mean_
            end do
          case(4)
            do i = 1, size(x, 4)
              res(:, :, :, i) = &
                x(:, :, :, i) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_4_rdp_rdp
      module function center_1_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(sp) :: res(size(x, 1))

        integer :: i
        complex(sp), allocatable :: mean_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i) = &
                x(i) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_1_csp_csp
      module function center_2_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(sp) :: res(size(x, 1), size(x, 2))

        integer :: i
        complex(sp), allocatable :: mean_(:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :) = &
                x(i, :) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i) = &
                x(:, i) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_2_csp_csp
      module function center_3_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(sp) :: res(size(x, 1), size(x, 2), size(x, 3))

        integer :: i
        complex(sp), allocatable :: mean_(:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :, :) = &
                x(i, :, :) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i, :) = &
                x(:, i, :) - mean_
            end do
          case(3)
            do i = 1, size(x, 3)
              res(:, :, i) = &
                x(:, :, i) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_3_csp_csp
      module function center_4_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(sp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        integer :: i
        complex(sp), allocatable :: mean_(:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :, :, :) = &
                x(i, :, :, :) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i, :, :) = &
                x(:, i, :, :) - mean_
            end do
          case(3)
            do i = 1, size(x, 3)
              res(:, :, i, :) = &
                x(:, :, i, :) - mean_
            end do
          case(4)
            do i = 1, size(x, 4)
              res(:, :, :, i) = &
                x(:, :, :, i) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_4_csp_csp
      module function center_1_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(size(x, 1))

        integer :: i
        complex(dp), allocatable :: mean_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i) = &
                x(i) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_1_cdp_cdp
      module function center_2_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(size(x, 1), size(x, 2))

        integer :: i
        complex(dp), allocatable :: mean_(:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :) = &
                x(i, :) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i) = &
                x(:, i) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_2_cdp_cdp
      module function center_3_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(size(x, 1), size(x, 2), size(x, 3))

        integer :: i
        complex(dp), allocatable :: mean_(:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :, :) = &
                x(i, :, :) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i, :) = &
                x(:, i, :) - mean_
            end do
          case(3)
            do i = 1, size(x, 3)
              res(:, :, i) = &
                x(:, :, i) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_3_cdp_cdp
      module function center_4_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        integer :: i
        complex(dp), allocatable :: mean_(:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :, :, :) = &
                x(i, :, :, :) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i, :, :) = &
                x(:, i, :, :) - mean_
            end do
          case(3)
            do i = 1, size(x, 3)
              res(:, :, i, :) = &
                x(:, :, i, :) - mean_
            end do
          case(4)
            do i = 1, size(x, 4)
              res(:, :, :, i) = &
                x(:, :, :, i) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_4_cdp_cdp


      module function center_1_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1))


        integer :: i
        real(dp), allocatable :: mean_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i) = &
                real(x(i), dp) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_1_iint8_dp
      module function center_2_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2))


        integer :: i
        real(dp), allocatable :: mean_(:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :) = &
                real(x(i, :), dp) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i) = &
                real(x(:, i), dp) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_2_iint8_dp
      module function center_3_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3))


        integer :: i
        real(dp), allocatable :: mean_(:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :, :) = &
                real(x(i, :, :), dp) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i, :) = &
                real(x(:, i, :), dp) - mean_
            end do
          case(3)
            do i = 1, size(x, 3)
              res(:, :, i) = &
                real(x(:, :, i), dp) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_3_iint8_dp
      module function center_4_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))


        integer :: i
        real(dp), allocatable :: mean_(:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :, :, :) = &
                real(x(i, :, :, :), dp) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i, :, :) = &
                real(x(:, i, :, :), dp) - mean_
            end do
          case(3)
            do i = 1, size(x, 3)
              res(:, :, i, :) = &
                real(x(:, :, i, :), dp) - mean_
            end do
          case(4)
            do i = 1, size(x, 4)
              res(:, :, :, i) = &
                real(x(:, :, :, i), dp) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_4_iint8_dp
      module function center_1_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1))


        integer :: i
        real(dp), allocatable :: mean_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i) = &
                real(x(i), dp) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_1_iint16_dp
      module function center_2_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2))


        integer :: i
        real(dp), allocatable :: mean_(:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :) = &
                real(x(i, :), dp) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i) = &
                real(x(:, i), dp) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_2_iint16_dp
      module function center_3_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3))


        integer :: i
        real(dp), allocatable :: mean_(:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :, :) = &
                real(x(i, :, :), dp) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i, :) = &
                real(x(:, i, :), dp) - mean_
            end do
          case(3)
            do i = 1, size(x, 3)
              res(:, :, i) = &
                real(x(:, :, i), dp) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_3_iint16_dp
      module function center_4_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))


        integer :: i
        real(dp), allocatable :: mean_(:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :, :, :) = &
                real(x(i, :, :, :), dp) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i, :, :) = &
                real(x(:, i, :, :), dp) - mean_
            end do
          case(3)
            do i = 1, size(x, 3)
              res(:, :, i, :) = &
                real(x(:, :, i, :), dp) - mean_
            end do
          case(4)
            do i = 1, size(x, 4)
              res(:, :, :, i) = &
                real(x(:, :, :, i), dp) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_4_iint16_dp
      module function center_1_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1))


        integer :: i
        real(dp), allocatable :: mean_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i) = &
                real(x(i), dp) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_1_iint32_dp
      module function center_2_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2))


        integer :: i
        real(dp), allocatable :: mean_(:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :) = &
                real(x(i, :), dp) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i) = &
                real(x(:, i), dp) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_2_iint32_dp
      module function center_3_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3))


        integer :: i
        real(dp), allocatable :: mean_(:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :, :) = &
                real(x(i, :, :), dp) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i, :) = &
                real(x(:, i, :), dp) - mean_
            end do
          case(3)
            do i = 1, size(x, 3)
              res(:, :, i) = &
                real(x(:, :, i), dp) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_3_iint32_dp
      module function center_4_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))


        integer :: i
        real(dp), allocatable :: mean_(:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :, :, :) = &
                real(x(i, :, :, :), dp) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i, :, :) = &
                real(x(:, i, :, :), dp) - mean_
            end do
          case(3)
            do i = 1, size(x, 3)
              res(:, :, i, :) = &
                real(x(:, :, i, :), dp) - mean_
            end do
          case(4)
            do i = 1, size(x, 4)
              res(:, :, :, i) = &
                real(x(:, :, :, i), dp) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_4_iint32_dp
      module function center_1_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1))


        integer :: i
        real(dp), allocatable :: mean_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i) = &
                real(x(i), dp) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_1_iint64_dp
      module function center_2_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2))


        integer :: i
        real(dp), allocatable :: mean_(:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :) = &
                real(x(i, :), dp) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i) = &
                real(x(:, i), dp) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_2_iint64_dp
      module function center_3_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3))


        integer :: i
        real(dp), allocatable :: mean_(:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :, :) = &
                real(x(i, :, :), dp) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i, :) = &
                real(x(:, i, :), dp) - mean_
            end do
          case(3)
            do i = 1, size(x, 3)
              res(:, :, i) = &
                real(x(:, :, i), dp) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_3_iint64_dp
      module function center_4_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))


        integer :: i
        real(dp), allocatable :: mean_(:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        allocate(mean_, source = mean(x, dim))

        select case(dim)
          case(1)
            do i = 1, size(x, 1)
              res(i, :, :, :) = &
                real(x(i, :, :, :), dp) - mean_
            end do
          case(2)
            do i = 1, size(x, 2)
              res(:, i, :, :) = &
                real(x(:, i, :, :), dp) - mean_
            end do
          case(3)
            do i = 1, size(x, 3)
              res(:, :, i, :) = &
                real(x(:, :, i, :), dp) - mean_
            end do
          case(4)
            do i = 1, size(x, 4)
              res(:, :, :, i) = &
                real(x(:, :, :, i), dp) - mean_
            end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_4_iint64_dp

      module function center_mask_all_1_rsp_rsp(x, mask) result(res)
        real(sp), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(sp) :: res(size(x, 1))

        res = 0
        where(mask) res = x - mean(x, mask)

      end function center_mask_all_1_rsp_rsp
      module function center_mask_all_2_rsp_rsp(x, mask) result(res)
        real(sp), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(sp) :: res(size(x, 1), size(x, 2))

        res = 0
        where(mask) res = x - mean(x, mask)

      end function center_mask_all_2_rsp_rsp
      module function center_mask_all_3_rsp_rsp(x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(sp) :: res(size(x, 1), size(x, 2), size(x, 3))

        res = 0
        where(mask) res = x - mean(x, mask)

      end function center_mask_all_3_rsp_rsp
      module function center_mask_all_4_rsp_rsp(x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(sp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        res = 0
        where(mask) res = x - mean(x, mask)

      end function center_mask_all_4_rsp_rsp
      module function center_mask_all_1_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(dp) :: res(size(x, 1))

        res = 0
        where(mask) res = x - mean(x, mask)

      end function center_mask_all_1_rdp_rdp
      module function center_mask_all_2_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(size(x, 1), size(x, 2))

        res = 0
        where(mask) res = x - mean(x, mask)

      end function center_mask_all_2_rdp_rdp
      module function center_mask_all_3_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3))

        res = 0
        where(mask) res = x - mean(x, mask)

      end function center_mask_all_3_rdp_rdp
      module function center_mask_all_4_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        res = 0
        where(mask) res = x - mean(x, mask)

      end function center_mask_all_4_rdp_rdp
      module function center_mask_all_1_csp_csp(x, mask) result(res)
        complex(sp), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        complex(sp) :: res(size(x, 1))

        res = 0
        where(mask) res = x - mean(x, mask)

      end function center_mask_all_1_csp_csp
      module function center_mask_all_2_csp_csp(x, mask) result(res)
        complex(sp), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        complex(sp) :: res(size(x, 1), size(x, 2))

        res = 0
        where(mask) res = x - mean(x, mask)

      end function center_mask_all_2_csp_csp
      module function center_mask_all_3_csp_csp(x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        complex(sp) :: res(size(x, 1), size(x, 2), size(x, 3))

        res = 0
        where(mask) res = x - mean(x, mask)

      end function center_mask_all_3_csp_csp
      module function center_mask_all_4_csp_csp(x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        complex(sp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        res = 0
        where(mask) res = x - mean(x, mask)

      end function center_mask_all_4_csp_csp
      module function center_mask_all_1_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        complex(dp) :: res(size(x, 1))

        res = 0
        where(mask) res = x - mean(x, mask)

      end function center_mask_all_1_cdp_cdp
      module function center_mask_all_2_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        complex(dp) :: res(size(x, 1), size(x, 2))

        res = 0
        where(mask) res = x - mean(x, mask)

      end function center_mask_all_2_cdp_cdp
      module function center_mask_all_3_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        complex(dp) :: res(size(x, 1), size(x, 2), size(x, 3))

        res = 0
        where(mask) res = x - mean(x, mask)

      end function center_mask_all_3_cdp_cdp
      module function center_mask_all_4_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        complex(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        res = 0
        where(mask) res = x - mean(x, mask)

      end function center_mask_all_4_cdp_cdp


      module function center_mask_all_1_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(dp) :: res(size(x, 1))

        res = 0
        where(mask) res = real(x, dp) - mean(x, mask)

      end function center_mask_all_1_iint8_dp
      module function center_mask_all_2_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(size(x, 1), size(x, 2))

        res = 0
        where(mask) res = real(x, dp) - mean(x, mask)

      end function center_mask_all_2_iint8_dp
      module function center_mask_all_3_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3))

        res = 0
        where(mask) res = real(x, dp) - mean(x, mask)

      end function center_mask_all_3_iint8_dp
      module function center_mask_all_4_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        res = 0
        where(mask) res = real(x, dp) - mean(x, mask)

      end function center_mask_all_4_iint8_dp
      module function center_mask_all_1_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(dp) :: res(size(x, 1))

        res = 0
        where(mask) res = real(x, dp) - mean(x, mask)

      end function center_mask_all_1_iint16_dp
      module function center_mask_all_2_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(size(x, 1), size(x, 2))

        res = 0
        where(mask) res = real(x, dp) - mean(x, mask)

      end function center_mask_all_2_iint16_dp
      module function center_mask_all_3_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3))

        res = 0
        where(mask) res = real(x, dp) - mean(x, mask)

      end function center_mask_all_3_iint16_dp
      module function center_mask_all_4_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        res = 0
        where(mask) res = real(x, dp) - mean(x, mask)

      end function center_mask_all_4_iint16_dp
      module function center_mask_all_1_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(dp) :: res(size(x, 1))

        res = 0
        where(mask) res = real(x, dp) - mean(x, mask)

      end function center_mask_all_1_iint32_dp
      module function center_mask_all_2_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(size(x, 1), size(x, 2))

        res = 0
        where(mask) res = real(x, dp) - mean(x, mask)

      end function center_mask_all_2_iint32_dp
      module function center_mask_all_3_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3))

        res = 0
        where(mask) res = real(x, dp) - mean(x, mask)

      end function center_mask_all_3_iint32_dp
      module function center_mask_all_4_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        res = 0
        where(mask) res = real(x, dp) - mean(x, mask)

      end function center_mask_all_4_iint32_dp
      module function center_mask_all_1_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(dp) :: res(size(x, 1))

        res = 0
        where(mask) res = real(x, dp) - mean(x, mask)

      end function center_mask_all_1_iint64_dp
      module function center_mask_all_2_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(size(x, 1), size(x, 2))

        res = 0
        where(mask) res = real(x, dp) - mean(x, mask)

      end function center_mask_all_2_iint64_dp
      module function center_mask_all_3_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3))

        res = 0
        where(mask) res = real(x, dp) - mean(x, mask)

      end function center_mask_all_3_iint64_dp
      module function center_mask_all_4_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        res = 0
        where(mask) res = real(x, dp) - mean(x, mask)

      end function center_mask_all_4_iint64_dp

      module function  center_mask_1_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(sp) :: res(size(x, 1))

        integer :: i
        real(sp), allocatable :: mean_

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              where(mask)&
                res = x - mean_
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_1_rsp_rsp
      module function  center_mask_2_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(sp) :: res(size(x, 1), size(x, 2))

        integer :: i
        real(sp), allocatable :: mean_(:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :))&
                  res(i, :) = &
                    x(i, :) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i))&
                  res(:, i) = &
                    x(:, i) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_2_rsp_rsp
      module function  center_mask_3_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(sp) :: res(size(x, 1), size(x, 2), size(x, 3))

        integer :: i
        real(sp), allocatable :: mean_(:,:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :, :))&
                  res(i, :, :) = &
                    x(i, :, :) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i, :))&
                  res(:, i, :) = &
                    x(:, i, :) - mean_
              end do
            case(3)
              do i = 1, size(x, 3)
                where(mask(:, :, i))&
                  res(:, :, i) = &
                    x(:, :, i) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_3_rsp_rsp
      module function  center_mask_4_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(sp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        integer :: i
        real(sp), allocatable :: mean_(:,:,:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :, :, :))&
                  res(i, :, :, :) = &
                    x(i, :, :, :) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i, :, :))&
                  res(:, i, :, :) = &
                    x(:, i, :, :) - mean_
              end do
            case(3)
              do i = 1, size(x, 3)
                where(mask(:, :, i, :))&
                  res(:, :, i, :) = &
                    x(:, :, i, :) - mean_
              end do
            case(4)
              do i = 1, size(x, 4)
                where(mask(:, :, :, i))&
                  res(:, :, :, i) = &
                    x(:, :, :, i) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_4_rsp_rsp
      module function  center_mask_1_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res(size(x, 1))

        integer :: i
        real(dp), allocatable :: mean_

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              where(mask)&
                res = x - mean_
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_1_rdp_rdp
      module function  center_mask_2_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(size(x, 1), size(x, 2))

        integer :: i
        real(dp), allocatable :: mean_(:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :))&
                  res(i, :) = &
                    x(i, :) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i))&
                  res(:, i) = &
                    x(:, i) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_2_rdp_rdp
      module function  center_mask_3_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3))

        integer :: i
        real(dp), allocatable :: mean_(:,:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :, :))&
                  res(i, :, :) = &
                    x(i, :, :) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i, :))&
                  res(:, i, :) = &
                    x(:, i, :) - mean_
              end do
            case(3)
              do i = 1, size(x, 3)
                where(mask(:, :, i))&
                  res(:, :, i) = &
                    x(:, :, i) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_3_rdp_rdp
      module function  center_mask_4_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        integer :: i
        real(dp), allocatable :: mean_(:,:,:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :, :, :))&
                  res(i, :, :, :) = &
                    x(i, :, :, :) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i, :, :))&
                  res(:, i, :, :) = &
                    x(:, i, :, :) - mean_
              end do
            case(3)
              do i = 1, size(x, 3)
                where(mask(:, :, i, :))&
                  res(:, :, i, :) = &
                    x(:, :, i, :) - mean_
              end do
            case(4)
              do i = 1, size(x, 4)
                where(mask(:, :, :, i))&
                  res(:, :, :, i) = &
                    x(:, :, :, i) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_4_rdp_rdp
      module function  center_mask_1_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        complex(sp) :: res(size(x, 1))

        integer :: i
        complex(sp), allocatable :: mean_

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              where(mask)&
                res = x - mean_
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_1_csp_csp
      module function  center_mask_2_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        complex(sp) :: res(size(x, 1), size(x, 2))

        integer :: i
        complex(sp), allocatable :: mean_(:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :))&
                  res(i, :) = &
                    x(i, :) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i))&
                  res(:, i) = &
                    x(:, i) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_2_csp_csp
      module function  center_mask_3_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        complex(sp) :: res(size(x, 1), size(x, 2), size(x, 3))

        integer :: i
        complex(sp), allocatable :: mean_(:,:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :, :))&
                  res(i, :, :) = &
                    x(i, :, :) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i, :))&
                  res(:, i, :) = &
                    x(:, i, :) - mean_
              end do
            case(3)
              do i = 1, size(x, 3)
                where(mask(:, :, i))&
                  res(:, :, i) = &
                    x(:, :, i) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_3_csp_csp
      module function  center_mask_4_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        complex(sp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        integer :: i
        complex(sp), allocatable :: mean_(:,:,:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :, :, :))&
                  res(i, :, :, :) = &
                    x(i, :, :, :) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i, :, :))&
                  res(:, i, :, :) = &
                    x(:, i, :, :) - mean_
              end do
            case(3)
              do i = 1, size(x, 3)
                where(mask(:, :, i, :))&
                  res(:, :, i, :) = &
                    x(:, :, i, :) - mean_
              end do
            case(4)
              do i = 1, size(x, 4)
                where(mask(:, :, :, i))&
                  res(:, :, :, i) = &
                    x(:, :, :, i) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_4_csp_csp
      module function  center_mask_1_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        complex(dp) :: res(size(x, 1))

        integer :: i
        complex(dp), allocatable :: mean_

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              where(mask)&
                res = x - mean_
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_1_cdp_cdp
      module function  center_mask_2_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        complex(dp) :: res(size(x, 1), size(x, 2))

        integer :: i
        complex(dp), allocatable :: mean_(:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :))&
                  res(i, :) = &
                    x(i, :) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i))&
                  res(:, i) = &
                    x(:, i) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_2_cdp_cdp
      module function  center_mask_3_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        complex(dp) :: res(size(x, 1), size(x, 2), size(x, 3))

        integer :: i
        complex(dp), allocatable :: mean_(:,:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :, :))&
                  res(i, :, :) = &
                    x(i, :, :) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i, :))&
                  res(:, i, :) = &
                    x(:, i, :) - mean_
              end do
            case(3)
              do i = 1, size(x, 3)
                where(mask(:, :, i))&
                  res(:, :, i) = &
                    x(:, :, i) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_3_cdp_cdp
      module function  center_mask_4_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        complex(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        integer :: i
        complex(dp), allocatable :: mean_(:,:,:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :, :, :))&
                  res(i, :, :, :) = &
                    x(i, :, :, :) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i, :, :))&
                  res(:, i, :, :) = &
                    x(:, i, :, :) - mean_
              end do
            case(3)
              do i = 1, size(x, 3)
                where(mask(:, :, i, :))&
                  res(:, :, i, :) = &
                    x(:, :, i, :) - mean_
              end do
            case(4)
              do i = 1, size(x, 4)
                where(mask(:, :, :, i))&
                  res(:, :, :, i) = &
                    x(:, :, :, i) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_4_cdp_cdp


      module function center_mask_1_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res(size(x, 1))

        integer :: i
        real(dp), allocatable :: mean_

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              where(mask)&
                res = real(x, dp) - mean_
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_1_iint8_dp
      module function center_mask_2_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(size(x, 1), size(x, 2))

        integer :: i
        real(dp), allocatable :: mean_(:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :))&
                  res(i, :) = &
                    real(x(i, :), dp) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i))&
                  res(:, i) = &
                    real(x(:, i), dp) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_2_iint8_dp
      module function center_mask_3_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3))

        integer :: i
        real(dp), allocatable :: mean_(:,:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :, :))&
                  res(i, :, :) = &
                    real(x(i, :, :), dp) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i, :))&
                  res(:, i, :) = &
                    real(x(:, i, :), dp) - mean_
              end do
            case(3)
              do i = 1, size(x, 3)
                where(mask(:, :, i))&
                  res(:, :, i) = &
                    real(x(:, :, i), dp) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_3_iint8_dp
      module function center_mask_4_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        integer :: i
        real(dp), allocatable :: mean_(:,:,:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :, :, :))&
                  res(i, :, :, :) = &
                    real(x(i, :, :, :), dp) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i, :, :))&
                  res(:, i, :, :) = &
                    real(x(:, i, :, :), dp) - mean_
              end do
            case(3)
              do i = 1, size(x, 3)
                where(mask(:, :, i, :))&
                  res(:, :, i, :) = &
                    real(x(:, :, i, :), dp) - mean_
              end do
            case(4)
              do i = 1, size(x, 4)
                where(mask(:, :, :, i))&
                  res(:, :, :, i) = &
                    real(x(:, :, :, i), dp) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_4_iint8_dp
      module function center_mask_1_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res(size(x, 1))

        integer :: i
        real(dp), allocatable :: mean_

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              where(mask)&
                res = real(x, dp) - mean_
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_1_iint16_dp
      module function center_mask_2_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(size(x, 1), size(x, 2))

        integer :: i
        real(dp), allocatable :: mean_(:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :))&
                  res(i, :) = &
                    real(x(i, :), dp) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i))&
                  res(:, i) = &
                    real(x(:, i), dp) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_2_iint16_dp
      module function center_mask_3_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3))

        integer :: i
        real(dp), allocatable :: mean_(:,:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :, :))&
                  res(i, :, :) = &
                    real(x(i, :, :), dp) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i, :))&
                  res(:, i, :) = &
                    real(x(:, i, :), dp) - mean_
              end do
            case(3)
              do i = 1, size(x, 3)
                where(mask(:, :, i))&
                  res(:, :, i) = &
                    real(x(:, :, i), dp) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_3_iint16_dp
      module function center_mask_4_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        integer :: i
        real(dp), allocatable :: mean_(:,:,:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :, :, :))&
                  res(i, :, :, :) = &
                    real(x(i, :, :, :), dp) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i, :, :))&
                  res(:, i, :, :) = &
                    real(x(:, i, :, :), dp) - mean_
              end do
            case(3)
              do i = 1, size(x, 3)
                where(mask(:, :, i, :))&
                  res(:, :, i, :) = &
                    real(x(:, :, i, :), dp) - mean_
              end do
            case(4)
              do i = 1, size(x, 4)
                where(mask(:, :, :, i))&
                  res(:, :, :, i) = &
                    real(x(:, :, :, i), dp) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_4_iint16_dp
      module function center_mask_1_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res(size(x, 1))

        integer :: i
        real(dp), allocatable :: mean_

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              where(mask)&
                res = real(x, dp) - mean_
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_1_iint32_dp
      module function center_mask_2_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(size(x, 1), size(x, 2))

        integer :: i
        real(dp), allocatable :: mean_(:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :))&
                  res(i, :) = &
                    real(x(i, :), dp) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i))&
                  res(:, i) = &
                    real(x(:, i), dp) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_2_iint32_dp
      module function center_mask_3_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3))

        integer :: i
        real(dp), allocatable :: mean_(:,:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :, :))&
                  res(i, :, :) = &
                    real(x(i, :, :), dp) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i, :))&
                  res(:, i, :) = &
                    real(x(:, i, :), dp) - mean_
              end do
            case(3)
              do i = 1, size(x, 3)
                where(mask(:, :, i))&
                  res(:, :, i) = &
                    real(x(:, :, i), dp) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_3_iint32_dp
      module function center_mask_4_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        integer :: i
        real(dp), allocatable :: mean_(:,:,:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :, :, :))&
                  res(i, :, :, :) = &
                    real(x(i, :, :, :), dp) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i, :, :))&
                  res(:, i, :, :) = &
                    real(x(:, i, :, :), dp) - mean_
              end do
            case(3)
              do i = 1, size(x, 3)
                where(mask(:, :, i, :))&
                  res(:, :, i, :) = &
                    real(x(:, :, i, :), dp) - mean_
              end do
            case(4)
              do i = 1, size(x, 4)
                where(mask(:, :, :, i))&
                  res(:, :, :, i) = &
                    real(x(:, :, :, i), dp) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_4_iint32_dp
      module function center_mask_1_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res(size(x, 1))

        integer :: i
        real(dp), allocatable :: mean_

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              where(mask)&
                res = real(x, dp) - mean_
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_1_iint64_dp
      module function center_mask_2_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(size(x, 1), size(x, 2))

        integer :: i
        real(dp), allocatable :: mean_(:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :))&
                  res(i, :) = &
                    real(x(i, :), dp) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i))&
                  res(:, i) = &
                    real(x(:, i), dp) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_2_iint64_dp
      module function center_mask_3_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3))

        integer :: i
        real(dp), allocatable :: mean_(:,:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :, :))&
                  res(i, :, :) = &
                    real(x(i, :, :), dp) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i, :))&
                  res(:, i, :) = &
                    real(x(:, i, :), dp) - mean_
              end do
            case(3)
              do i = 1, size(x, 3)
                where(mask(:, :, i))&
                  res(:, :, i) = &
                    real(x(:, :, i), dp) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_3_iint64_dp
      module function center_mask_4_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(size(x, 1), size(x, 2), size(x, 3), size(x, 4))

        integer :: i
        real(dp), allocatable :: mean_(:,:,:)

        allocate(mean_, source = mean(x, dim, mask))

        res = 0

        select case(dim)
            case(1)
              do i = 1, size(x, 1)
                where(mask(i, :, :, :))&
                  res(i, :, :, :) = &
                    real(x(i, :, :, :), dp) - mean_
              end do
            case(2)
              do i = 1, size(x, 2)
                where(mask(:, i, :, :))&
                  res(:, i, :, :) = &
                    real(x(:, i, :, :), dp) - mean_
              end do
            case(3)
              do i = 1, size(x, 3)
                where(mask(:, :, i, :))&
                  res(:, :, i, :) = &
                    real(x(:, :, i, :), dp) - mean_
              end do
            case(4)
              do i = 1, size(x, 4)
                where(mask(:, :, :, i))&
                  res(:, :, :, i) = &
                    real(x(:, :, :, i), dp) - mean_
              end do
        case default
          call error_stop("ERROR (center): wrong dimension")
        end select

        deallocate(mean_)

      end function center_mask_4_iint64_dp

end submodule
