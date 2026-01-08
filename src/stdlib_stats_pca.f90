submodule (stdlib_stats) stdlib_stats_pca
  use stdlib_kinds, only: sp, dp
  use stdlib_error, only: error_stop
  use stdlib_optval, only: optval
  use stdlib_linalg, only: svd, eigh
  use stdlib_linalg_constants, only: ilp
  use stdlib_linalg_blas, only: gemm
  use stdlib_linalg_state, only: linalg_state_type, LINALG_ERROR
  use stdlib_sorting, only: sort_index
  implicit none

contains

    module subroutine pca_sp(x, components, singular_values, x_mean, &
                                method, overwrite_x, err)
      real(sp), intent(inout), target :: x(:,:)
      real(sp), intent(out) :: components(:,:)
      real(sp), intent(out) :: singular_values(:)
      real(sp), intent(out), optional :: x_mean(:)
      character(*), intent(in), optional :: method
      logical, intent(in), optional :: overwrite_x
      type(linalg_state_type), intent(out), optional :: err

      type(linalg_state_type) :: err0
      integer(ilp) :: n, p, i, j, k, m, n_s
      real(sp), allocatable :: mu(:)
      character(16) :: method_
      
      n = size(x, 1, kind=ilp)
      p = size(x, 2, kind=ilp)
      k = size(components, 1, kind=ilp)
      
      method_ = optval(method, "svd")
      
      ! 1. Calculate mean using intrinsic sum (avoids submodule dependency issues)
      allocate(mu(p))
      do j = 1, p
         mu(j) = sum(x(:, j)) / real(n, sp)
      end do
      if (present(x_mean)) x_mean = mu

      if (method_ == "svd") then
         ! 2. Center data and call SVD with temporaries for robustness
         block
            real(sp), allocatable :: s_tmp(:), vt_tmp(:,:)
            n_s = min(n, p)
            allocate(s_tmp(n_s), vt_tmp(n_s, p))

            if (optval(overwrite_x, .false.)) then
               do i = 1, n
                  x(i, :) = x(i, :) - mu
               end do
               call svd(x, s_tmp, vt=vt_tmp, overwrite_a=.true., full_matrices=.false., err=err0)
            else
               block
                  real(sp), allocatable :: x_centered(:,:)
                  allocate(x_centered(n, p))
                  do i = 1, n
                     x_centered(i, :) = x(i, :) - mu
                  end do
                  call svd(x_centered, s_tmp, vt=vt_tmp, overwrite_a=.true., full_matrices=.false., err=err0)
               end block
            end if

            if (err0%ok()) then
               m = min(size(components, 1, kind=ilp), n_s)
               components(:m, :) = vt_tmp(:m, :)
               m = min(size(singular_values, 1, kind=ilp), n_s)
               singular_values(:m) = s_tmp(:m)
            end if
         end block
      else if (method_ == "eig" .or. method_ == "cov") then
         ! 3. Eigendecomposition of covariance matrix (computed inline)
         block
            real(sp), allocatable :: c(:,:), vectors(:,:), x_centered(:,:)
            real(sp), allocatable :: lambda(:), lambda_copy(:)
            integer(ilp), allocatable :: idx(:)
            real(sp) :: scale_factor

            allocate(c(p, p), lambda(p), lambda_copy(p), idx(p), vectors(p, p))
            allocate(x_centered(n, p))
            
            ! Center data
            do i = 1, n
               x_centered(i, :) = x(i, :) - mu
            end do
            
            ! Compute covariance matrix: C = X^T * X / (n-1)
            scale_factor = 1.0_sp / real(max(n-1, 1), sp)
            do i = 1, p
               do j = 1, p
                  c(i, j) = dot_product(x_centered(:, i), x_centered(:, j)) * scale_factor
               end do
            end do
            
            call eigh(c, lambda, vectors=vectors, err=err0)
            
            if (err0%ok()) then
               ! Sort eigenvalues in descending order using stdlib_sorting
               ! sort_index sorts in ascending order, so we negate values
               lambda_copy = -lambda
               call sort_index(lambda_copy, idx)
               
               ! Assign sorted results
               m = min(size(components, 1, kind=ilp), p)
               do i = 1, m
                  components(i, :) = vectors(:, idx(i))
                  if (lambda(idx(i)) > 0.0_sp) then
                     singular_values(i) = sqrt(lambda(idx(i)) * real(n-1, sp))
                  else
                     singular_values(i) = 0.0_sp
                  end if
               end do
            end if
         end block
      else
         err0 = linalg_state_type("pca", LINALG_ERROR, "Unknown method: "//method_)
      end if
      
      ! Handle error state: return error or stop if err not present
      call err0%handle(err)
      
    end subroutine pca_sp
    module subroutine pca_dp(x, components, singular_values, x_mean, &
                                method, overwrite_x, err)
      real(dp), intent(inout), target :: x(:,:)
      real(dp), intent(out) :: components(:,:)
      real(dp), intent(out) :: singular_values(:)
      real(dp), intent(out), optional :: x_mean(:)
      character(*), intent(in), optional :: method
      logical, intent(in), optional :: overwrite_x
      type(linalg_state_type), intent(out), optional :: err

      type(linalg_state_type) :: err0
      integer(ilp) :: n, p, i, j, k, m, n_s
      real(dp), allocatable :: mu(:)
      character(16) :: method_
      
      n = size(x, 1, kind=ilp)
      p = size(x, 2, kind=ilp)
      k = size(components, 1, kind=ilp)
      
      method_ = optval(method, "svd")
      
      ! 1. Calculate mean using intrinsic sum (avoids submodule dependency issues)
      allocate(mu(p))
      do j = 1, p
         mu(j) = sum(x(:, j)) / real(n, dp)
      end do
      if (present(x_mean)) x_mean = mu

      if (method_ == "svd") then
         ! 2. Center data and call SVD with temporaries for robustness
         block
            real(dp), allocatable :: s_tmp(:), vt_tmp(:,:)
            n_s = min(n, p)
            allocate(s_tmp(n_s), vt_tmp(n_s, p))

            if (optval(overwrite_x, .false.)) then
               do i = 1, n
                  x(i, :) = x(i, :) - mu
               end do
               call svd(x, s_tmp, vt=vt_tmp, overwrite_a=.true., full_matrices=.false., err=err0)
            else
               block
                  real(dp), allocatable :: x_centered(:,:)
                  allocate(x_centered(n, p))
                  do i = 1, n
                     x_centered(i, :) = x(i, :) - mu
                  end do
                  call svd(x_centered, s_tmp, vt=vt_tmp, overwrite_a=.true., full_matrices=.false., err=err0)
               end block
            end if

            if (err0%ok()) then
               m = min(size(components, 1, kind=ilp), n_s)
               components(:m, :) = vt_tmp(:m, :)
               m = min(size(singular_values, 1, kind=ilp), n_s)
               singular_values(:m) = s_tmp(:m)
            end if
         end block
      else if (method_ == "eig" .or. method_ == "cov") then
         ! 3. Eigendecomposition of covariance matrix (computed inline)
         block
            real(dp), allocatable :: c(:,:), vectors(:,:), x_centered(:,:)
            real(dp), allocatable :: lambda(:), lambda_copy(:)
            integer(ilp), allocatable :: idx(:)
            real(dp) :: scale_factor

            allocate(c(p, p), lambda(p), lambda_copy(p), idx(p), vectors(p, p))
            allocate(x_centered(n, p))
            
            ! Center data
            do i = 1, n
               x_centered(i, :) = x(i, :) - mu
            end do
            
            ! Compute covariance matrix: C = X^T * X / (n-1)
            scale_factor = 1.0_dp / real(max(n-1, 1), dp)
            do i = 1, p
               do j = 1, p
                  c(i, j) = dot_product(x_centered(:, i), x_centered(:, j)) * scale_factor
               end do
            end do
            
            call eigh(c, lambda, vectors=vectors, err=err0)
            
            if (err0%ok()) then
               ! Sort eigenvalues in descending order using stdlib_sorting
               ! sort_index sorts in ascending order, so we negate values
               lambda_copy = -lambda
               call sort_index(lambda_copy, idx)
               
               ! Assign sorted results
               m = min(size(components, 1, kind=ilp), p)
               do i = 1, m
                  components(i, :) = vectors(:, idx(i))
                  if (lambda(idx(i)) > 0.0_dp) then
                     singular_values(i) = sqrt(lambda(idx(i)) * real(n-1, dp))
                  else
                     singular_values(i) = 0.0_dp
                  end if
               end do
            end if
         end block
      else
         err0 = linalg_state_type("pca", LINALG_ERROR, "Unknown method: "//method_)
      end if
      
      ! Handle error state: return error or stop if err not present
      call err0%handle(err)
      
    end subroutine pca_dp


    module subroutine pca_transform_sp(x, components, x_mean, x_transformed)
      real(sp), intent(in) :: x(:,:)
      real(sp), intent(in) :: components(:,:)
      real(sp), intent(in), optional :: x_mean(:)
      real(sp), intent(out) :: x_transformed(:,:)

      integer(ilp) :: i, n, p, nc
      real(sp), allocatable :: x_centered(:,:)
      real(sp), parameter :: alpha = 1.0_sp, beta = 0.0_sp
      
      n = size(x, 1, kind=ilp)
      p = size(x, 2, kind=ilp)
      nc = size(components, 1, kind=ilp)
      
      allocate(x_centered(n, p))
      if (present(x_mean)) then
         do i = 1, n
            x_centered(i, :) = x(i, :) - x_mean
         end do
      else
         x_centered = x
      end if
      
      ! x_transformed = x_centered * components^T using GEMM
      ! GEMM: C = alpha * op(A) * op(B) + beta * C
      ! x_transformed(n, nc) = x_centered(n, p) * components(nc, p)^T
      call gemm('N', 'T', n, nc, p, alpha, x_centered, n, components, nc, beta, x_transformed, n)
    end subroutine pca_transform_sp
    module subroutine pca_transform_dp(x, components, x_mean, x_transformed)
      real(dp), intent(in) :: x(:,:)
      real(dp), intent(in) :: components(:,:)
      real(dp), intent(in), optional :: x_mean(:)
      real(dp), intent(out) :: x_transformed(:,:)

      integer(ilp) :: i, n, p, nc
      real(dp), allocatable :: x_centered(:,:)
      real(dp), parameter :: alpha = 1.0_dp, beta = 0.0_dp
      
      n = size(x, 1, kind=ilp)
      p = size(x, 2, kind=ilp)
      nc = size(components, 1, kind=ilp)
      
      allocate(x_centered(n, p))
      if (present(x_mean)) then
         do i = 1, n
            x_centered(i, :) = x(i, :) - x_mean
         end do
      else
         x_centered = x
      end if
      
      ! x_transformed = x_centered * components^T using GEMM
      ! GEMM: C = alpha * op(A) * op(B) + beta * C
      ! x_transformed(n, nc) = x_centered(n, p) * components(nc, p)^T
      call gemm('N', 'T', n, nc, p, alpha, x_centered, n, components, nc, beta, x_transformed, n)
    end subroutine pca_transform_dp


    module subroutine pca_inverse_transform_sp(x_reduced, components, x_mean, x_reconstructed)
      real(sp), intent(in) :: x_reduced(:,:)
      real(sp), intent(in) :: components(:,:)
      real(sp), intent(in), optional :: x_mean(:)
      real(sp), intent(out) :: x_reconstructed(:,:)

      integer(ilp) :: i, n, nc, p
      real(sp), parameter :: alpha = 1.0_sp, beta = 0.0_sp
      
      n = size(x_reduced, 1, kind=ilp)
      nc = size(x_reduced, 2, kind=ilp)
      p = size(components, 2, kind=ilp)
      
      ! x_reconstructed = x_reduced * components using GEMM
      ! GEMM: C = alpha * op(A) * op(B) + beta * C
      ! x_reconstructed(n, p) = x_reduced(n, nc) * components(nc, p)
      call gemm('N', 'N', n, p, nc, alpha, x_reduced, n, components, nc, beta, x_reconstructed, n)
      
      if (present(x_mean)) then
         do i = 1, n
            x_reconstructed(i, :) = x_reconstructed(i, :) + x_mean
         end do
      end if
    end subroutine pca_inverse_transform_sp
    module subroutine pca_inverse_transform_dp(x_reduced, components, x_mean, x_reconstructed)
      real(dp), intent(in) :: x_reduced(:,:)
      real(dp), intent(in) :: components(:,:)
      real(dp), intent(in), optional :: x_mean(:)
      real(dp), intent(out) :: x_reconstructed(:,:)

      integer(ilp) :: i, n, nc, p
      real(dp), parameter :: alpha = 1.0_dp, beta = 0.0_dp
      
      n = size(x_reduced, 1, kind=ilp)
      nc = size(x_reduced, 2, kind=ilp)
      p = size(components, 2, kind=ilp)
      
      ! x_reconstructed = x_reduced * components using GEMM
      ! GEMM: C = alpha * op(A) * op(B) + beta * C
      ! x_reconstructed(n, p) = x_reduced(n, nc) * components(nc, p)
      call gemm('N', 'N', n, p, nc, alpha, x_reduced, n, components, nc, beta, x_reconstructed, n)
      
      if (present(x_mean)) then
         do i = 1, n
            x_reconstructed(i, :) = x_reconstructed(i, :) + x_mean
         end do
      end if
    end subroutine pca_inverse_transform_dp

end submodule stdlib_stats_pca
