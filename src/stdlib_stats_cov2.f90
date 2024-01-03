submodule (stdlib_stats) stdlib_stats_cov2

  use, intrinsic:: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
  use stdlib_error, only: error_stop
  use stdlib_optval, only: optval
  implicit none

contains

    module function cov2_2_rsp_rsp(x, y, maskx, masky, corrected) result(res)
      real(sp), intent(in) :: x(:, :)
      real(sp), intent(in) :: y(:, :)
      logical, intent(in), optional :: maskx
      logical, intent(in), optional :: masky
      logical, intent(in), optional :: corrected
      real(sp) :: res(size(x, 2), size(y, 2))

      integer :: i
      real(sp) :: x_mean_(size(x, 2))
      real(sp) :: y_mean_(size(y, 2))
      real(sp) :: x_center(size(x, 1),size(x, 2))
      real(sp) :: y_center(size(y, 1),size(y, 2))

      if(size(x, 1).ne.size(y, 1))then
        call error_stop("ERROR (cov): the number of rows in x and y must be equal")
      endif

      if (.not.optval(maskx, .true.).or..not.optval(masky, .true.)) then
        res = ieee_value(1._sp, ieee_quiet_nan)
        return
      end if

      x_mean_ = mean(x, 1)

      do i = 1, size(x, 1)
        x_center(i, :) = x(i, :) - x_mean_
      end do

      y_mean_ = mean(y, 1)

      do i = 1, size(y, 1)
        y_center(i, :) = y(i, :) - y_mean_
      enddo
            res = matmul( transpose(x_center), y_center)

      res = res / (size(x, 1) - merge(1, 0, optval(corrected, .true.)))

    end function cov2_2_rsp_rsp
    module function cov2_2_rdp_rdp(x, y, maskx, masky, corrected) result(res)
      real(dp), intent(in) :: x(:, :)
      real(dp), intent(in) :: y(:, :)
      logical, intent(in), optional :: maskx
      logical, intent(in), optional :: masky
      logical, intent(in), optional :: corrected
      real(dp) :: res(size(x, 2), size(y, 2))

      integer :: i
      real(dp) :: x_mean_(size(x, 2))
      real(dp) :: y_mean_(size(y, 2))
      real(dp) :: x_center(size(x, 1),size(x, 2))
      real(dp) :: y_center(size(y, 1),size(y, 2))

      if(size(x, 1).ne.size(y, 1))then
        call error_stop("ERROR (cov): the number of rows in x and y must be equal")
      endif

      if (.not.optval(maskx, .true.).or..not.optval(masky, .true.)) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      x_mean_ = mean(x, 1)

      do i = 1, size(x, 1)
        x_center(i, :) = x(i, :) - x_mean_
      end do

      y_mean_ = mean(y, 1)

      do i = 1, size(y, 1)
        y_center(i, :) = y(i, :) - y_mean_
      enddo
            res = matmul( transpose(x_center), y_center)

      res = res / (size(x, 1) - merge(1, 0, optval(corrected, .true.)))

    end function cov2_2_rdp_rdp
    module function cov2_2_csp_csp(x, y, maskx, masky, corrected) result(res)
      complex(sp), intent(in) :: x(:, :)
      complex(sp), intent(in) :: y(:, :)
      logical, intent(in), optional :: maskx
      logical, intent(in), optional :: masky
      logical, intent(in), optional :: corrected
      complex(sp) :: res(size(x, 2), size(y, 2))

      integer :: i
      complex(sp) :: x_mean_(size(x, 2))
      complex(sp) :: y_mean_(size(y, 2))
      complex(sp) :: x_center(size(x, 1),size(x, 2))
      complex(sp) :: y_center(size(y, 1),size(y, 2))

      if(size(x, 1).ne.size(y, 1))then
        call error_stop("ERROR (cov): the number of rows in x and y must be equal")
      endif

      if (.not.optval(maskx, .true.).or..not.optval(masky, .true.)) then
        res = ieee_value(1._sp, ieee_quiet_nan)
        return
      end if

      x_mean_ = mean(x, 1)

      do i = 1, size(x, 1)
        x_center(i, :) = x(i, :) - x_mean_
      end do

      y_mean_ = mean(y, 1)

      do i = 1, size(y, 1)
        y_center(i, :) = y(i, :) - y_mean_
      enddo
            res = matmul( transpose(x_center), y_center)

      res = res / (size(x, 1) - merge(1, 0, optval(corrected, .true.)))

    end function cov2_2_csp_csp
    module function cov2_2_cdp_cdp(x, y, maskx, masky, corrected) result(res)
      complex(dp), intent(in) :: x(:, :)
      complex(dp), intent(in) :: y(:, :)
      logical, intent(in), optional :: maskx
      logical, intent(in), optional :: masky
      logical, intent(in), optional :: corrected
      complex(dp) :: res(size(x, 2), size(y, 2))

      integer :: i
      complex(dp) :: x_mean_(size(x, 2))
      complex(dp) :: y_mean_(size(y, 2))
      complex(dp) :: x_center(size(x, 1),size(x, 2))
      complex(dp) :: y_center(size(y, 1),size(y, 2))

      if(size(x, 1).ne.size(y, 1))then
        call error_stop("ERROR (cov): the number of rows in x and y must be equal")
      endif

      if (.not.optval(maskx, .true.).or..not.optval(masky, .true.)) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      x_mean_ = mean(x, 1)

      do i = 1, size(x, 1)
        x_center(i, :) = x(i, :) - x_mean_
      end do

      y_mean_ = mean(y, 1)

      do i = 1, size(y, 1)
        y_center(i, :) = y(i, :) - y_mean_
      enddo
            res = matmul( transpose(x_center), y_center)

      res = res / (size(x, 1) - merge(1, 0, optval(corrected, .true.)))

    end function cov2_2_cdp_cdp


!  #:for k1, t1 in INT_KINDS_TYPES
!    #:set RName = rname("cov",2, t1, k1, 'dp')
!    module function cov2_2_cdp_cdp(x, dim, mask, corrected) result(res)
!      complex(dp), intent(in) :: x(:, :)
!      integer, intent(in) :: dim
!      logical, intent(in), optional :: mask
!      logical, intent(in), optional :: corrected
!      real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
!                      , merge(size(x, 1), size(x, 2), mask = 1<dim))
!
!      integer :: i
!      real(dp) :: mean_(merge(size(x, 1), size(x, 2), mask = 1<dim))
!      real(dp) :: center(size(x, 1),size(x, 2))
!
!      if (.not.optval(mask, .true.)) then
!        res = ieee_value(1._dp, ieee_quiet_nan)
!        return
!      end if
!
!      mean_ = mean(x, dim)
!      select case(dim)
!        case(1)
!          do i = 1, size(x, 1)
!            center(i, :) = real(x(i, :), dp) - mean_
!          end do
!          res = matmul( transpose(center), center)
!        case(2)
!          do i = 1, size(x, 2)
!            center(:, i) = real(x(:, i), dp) - mean_
!          end do
!          res = matmul( center, transpose(center))
!        case default
!          call error_stop("ERROR (cov): wrong dimension")
!      end select
!      res = res / (size(x, dim) - merge(1, 0, optval(corrected, .true.)))
!
!    end function cov2_2_cdp_cdp
!  #:endfor
!
!
!  #:for k1, t1 in RC_KINDS_TYPES
!    #:set RName = rname("cov_mask",2, t1, k1)
!    module function cov2_2_cdp_cdp(x, dim, mask, corrected) result(res)
!      complex(dp), intent(in) :: x(:, :)
!      integer, intent(in) :: dim
!      logical, intent(in) :: mask(:,:)
!      logical, intent(in), optional :: corrected
!      complex(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
!                          , merge(size(x, 1), size(x, 2), mask = 1<dim))
!
!      integer :: i, j, n
!      complex(dp) :: centeri_(merge(size(x, 2), size(x, 1), mask = 1<dim))
!      complex(dp) :: centerj_(merge(size(x, 2), size(x, 1), mask = 1<dim))
!      logical :: mask_(merge(size(x, 2), size(x, 1), mask = 1<dim))
!
!      select case(dim)
!        case(1)
!          do i = 1, size(res, 2)
!            do j = 1, size(res, 1)
!             mask_ = merge(.true., .false., mask(:, i) .and. mask(:, j))
!             centeri_ = merge( x(:, i) - mean(x(:, i), mask = mask_),&
!              #:if t1[0] == 'r'
!                0._dp,&
!              #:else
!                cmplx(0,0,kind=dp),&
!              #:endif
!                mask_)
!             centerj_ = merge( x(:, j) - mean(x(:, j), mask = mask_),&
!              #:if t1[0] == 'r'
!                0._dp,&
!              #:else
!                cmplx(0,0,kind=dp),&
!              #:endif
!                mask_)
!
!              n = count(mask_)
!              res(j, i) = dot_product( centerj_, centeri_)&
!                           / (n - merge(1, 0,&
!                            optval(corrected, .true.) .and. n > 0))
!            end do
!          end do
!        case(2)
!          do i = 1, size(res, 2)
!            do j = 1, size(res, 1)
!             mask_ = merge(.true., .false., mask(i, :) .and. mask(j, :))
!             centeri_ = merge( x(i, :) - mean(x(i, :), mask = mask_),&
!              #:if t1[0] == 'r'
!                0._dp,&
!              #:else
!                cmplx(0,0,kind=dp),&
!              #:endif
!                mask_)
!             centerj_ = merge( x(j, :) - mean(x(j, :), mask = mask_),&
!              #:if t1[0] == 'r'
!                0._dp,&
!              #:else
!                cmplx(0,0,kind=dp),&
!              #:endif
!                mask_)
!
!              n = count(mask_)
!              res(j, i) = dot_product( centeri_, centerj_)&
!                           / (n - merge(1, 0,&
!                            optval(corrected, .true.) .and. n > 0))
!            end do
!          end do
!        case default
!          call error_stop("ERROR (cov): wrong dimension")
!      end select
!
!    end function cov2_2_cdp_cdp
!  #:endfor
!
!
!  #:for k1, t1 in INT_KINDS_TYPES
!    #:set RName = rname("cov_mask",2, t1, k1, 'dp')
!    module function cov2_2_cdp_cdp(x, dim, mask, corrected) result(res)
!      complex(dp), intent(in) :: x(:, :)
!      integer, intent(in) :: dim
!      logical, intent(in) :: mask(:,:)
!      logical, intent(in), optional :: corrected
!      real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
!                          , merge(size(x, 1), size(x, 2), mask = 1<dim))
!
!      integer :: i, j, n
!      real(dp) :: centeri_(merge(size(x, 2), size(x, 1), mask = 1<dim))
!      real(dp) :: centerj_(merge(size(x, 2), size(x, 1), mask = 1<dim))
!      logical :: mask_(merge(size(x, 2), size(x, 1), mask = 1<dim))
!
!      select case(dim)
!        case(1)
!          do i = 1, size(res, 2)
!            do j = 1, size(res, 1)
!              mask_ = merge(.true., .false., mask(:, i) .and. mask(:, j))
!              centeri_ = merge( x(:, i) - mean(x(:, i), mask = mask_),&
!                 0._dp, mask_)
!              centerj_ = merge( x(:, j) - mean(x(:, j), mask = mask_),&
!                 0._dp, mask_)
!
!              n = count(mask_)
!              res(j, i) = dot_product( centerj_, centeri_)&
!                           / (n - merge(1, 0,&
!                            optval(corrected, .true.) .and. n > 0))
!            end do
!          end do
!        case(2)
!          do i = 1, size(res, 2)
!            do j = 1, size(res, 1)
!              mask_ = merge(.true., .false., mask(i, :) .and. mask(j, :))
!              centeri_ = merge( x(i, :) - mean(x(i, :), mask = mask_),&
!                 0._dp, mask_)
!              centerj_ = merge( x(j, :) - mean(x(j, :), mask = mask_),&
!                 0._dp, mask_)
!
!              n = count(mask_)
!              res(j, i) = dot_product( centeri_, centerj_)&
!                           / (n - merge(1, 0,&
!                            optval(corrected, .true.) .and. n > 0))
!            end do
!          end do
!        case default
!          call error_stop("ERROR (cov): wrong dimension")
!      end select
!
!    end function cov2_2_cdp_cdp
!  #:endfor


end submodule
