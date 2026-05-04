submodule (stdlib_sparse_spmv) stdlib_sparse_spmv_ell
contains

    !! spmv_ell
    module subroutine spmv_ell_1d_sp(matrix,vec_x,vec_y,alpha,beta,op)
        type(ELL_sp_type), intent(in) :: matrix
        real(sp), intent(in)    :: vec_x(:)
        real(sp), intent(inout) :: vec_y(:)
        real(sp), intent(in), optional :: alpha
        real(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op

        call spmv_ell_sub_1d_sp(matrix%data,matrix%index,matrix%K, &
            matrix%nnz,matrix%nrows,matrix%ncols,matrix%storage, &
            vec_x,vec_y,alpha,beta,op)
    end subroutine
    
    module subroutine spmv_ell_sub_1d_sp(data,index,mnz_p_row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
        real(sp), intent(in) :: data(:,:)
        integer(ilp), intent(in) :: index(:,:)
        integer, intent(in) :: mnz_p_row
        integer(ilp), intent(in) :: nnz
        integer(ilp), intent(in) :: nrows
        integer(ilp), intent(in) :: ncols
        integer, intent(in) :: storage
        real(sp), intent(in)    :: vec_x(:)
        real(sp), intent(inout) :: vec_y(:)
        real(sp), intent(in), optional :: alpha
        real(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(sp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j, k
        
        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_sp
        endif
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(i) = vec_y(i) + alpha_*data(i,k) * vec_x(j)
                    end do
                end do
            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(j) = vec_y(j) + alpha_*data(i,k) * vec_x(i)
                    end do
                end do
            else if( storage /= sparse_full .and. op_/=sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(i) = vec_y(i) + alpha_*data(i,k) * vec_x(j)
                        if(i==j) cycle 
                        vec_y(j) = vec_y(j) + alpha_*data(i,k) * vec_x(i)
                    end do
                end do
            end if
    end subroutine

    module subroutine spmv_ell_2d_sp(matrix,vec_x,vec_y,alpha,beta,op)
        type(ELL_sp_type), intent(in) :: matrix
        real(sp), intent(in)    :: vec_x(:,:)
        real(sp), intent(inout) :: vec_y(:,:)
        real(sp), intent(in), optional :: alpha
        real(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op

        call spmv_ell_sub_2d_sp(matrix%data,matrix%index,matrix%K, &
            matrix%nnz,matrix%nrows,matrix%ncols,matrix%storage, &
            vec_x,vec_y,alpha,beta,op)
    end subroutine
    
    module subroutine spmv_ell_sub_2d_sp(data,index,mnz_p_row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
        real(sp), intent(in) :: data(:,:)
        integer(ilp), intent(in) :: index(:,:)
        integer, intent(in) :: mnz_p_row
        integer(ilp), intent(in) :: nnz
        integer(ilp), intent(in) :: nrows
        integer(ilp), intent(in) :: ncols
        integer, intent(in) :: storage
        real(sp), intent(in)    :: vec_x(:,:)
        real(sp), intent(inout) :: vec_y(:,:)
        real(sp), intent(in), optional :: alpha
        real(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(sp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j, k
        
        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_sp
        endif
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(:,i) = vec_y(:,i) + alpha_*data(i,k) * vec_x(:,j)
                    end do
                end do
            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(:,j) = vec_y(:,j) + alpha_*data(i,k) * vec_x(:,i)
                    end do
                end do
            else if( storage /= sparse_full .and. op_/=sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(:,i) = vec_y(:,i) + alpha_*data(i,k) * vec_x(:,j)
                        if(i==j) cycle 
                        vec_y(:,j) = vec_y(:,j) + alpha_*data(i,k) * vec_x(:,i)
                    end do
                end do
            end if
    end subroutine

    module subroutine spmv_ell_1d_dp(matrix,vec_x,vec_y,alpha,beta,op)
        type(ELL_dp_type), intent(in) :: matrix
        real(dp), intent(in)    :: vec_x(:)
        real(dp), intent(inout) :: vec_y(:)
        real(dp), intent(in), optional :: alpha
        real(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op

        call spmv_ell_sub_1d_dp(matrix%data,matrix%index,matrix%K, &
            matrix%nnz,matrix%nrows,matrix%ncols,matrix%storage, &
            vec_x,vec_y,alpha,beta,op)
    end subroutine
    
    module subroutine spmv_ell_sub_1d_dp(data,index,mnz_p_row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
        real(dp), intent(in) :: data(:,:)
        integer(ilp), intent(in) :: index(:,:)
        integer, intent(in) :: mnz_p_row
        integer(ilp), intent(in) :: nnz
        integer(ilp), intent(in) :: nrows
        integer(ilp), intent(in) :: ncols
        integer, intent(in) :: storage
        real(dp), intent(in)    :: vec_x(:)
        real(dp), intent(inout) :: vec_y(:)
        real(dp), intent(in), optional :: alpha
        real(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(dp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j, k
        
        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_dp
        endif
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(i) = vec_y(i) + alpha_*data(i,k) * vec_x(j)
                    end do
                end do
            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(j) = vec_y(j) + alpha_*data(i,k) * vec_x(i)
                    end do
                end do
            else if( storage /= sparse_full .and. op_/=sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(i) = vec_y(i) + alpha_*data(i,k) * vec_x(j)
                        if(i==j) cycle 
                        vec_y(j) = vec_y(j) + alpha_*data(i,k) * vec_x(i)
                    end do
                end do
            end if
    end subroutine

    module subroutine spmv_ell_2d_dp(matrix,vec_x,vec_y,alpha,beta,op)
        type(ELL_dp_type), intent(in) :: matrix
        real(dp), intent(in)    :: vec_x(:,:)
        real(dp), intent(inout) :: vec_y(:,:)
        real(dp), intent(in), optional :: alpha
        real(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op

        call spmv_ell_sub_2d_dp(matrix%data,matrix%index,matrix%K, &
            matrix%nnz,matrix%nrows,matrix%ncols,matrix%storage, &
            vec_x,vec_y,alpha,beta,op)
    end subroutine
    
    module subroutine spmv_ell_sub_2d_dp(data,index,mnz_p_row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
        real(dp), intent(in) :: data(:,:)
        integer(ilp), intent(in) :: index(:,:)
        integer, intent(in) :: mnz_p_row
        integer(ilp), intent(in) :: nnz
        integer(ilp), intent(in) :: nrows
        integer(ilp), intent(in) :: ncols
        integer, intent(in) :: storage
        real(dp), intent(in)    :: vec_x(:,:)
        real(dp), intent(inout) :: vec_y(:,:)
        real(dp), intent(in), optional :: alpha
        real(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(dp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j, k
        
        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_dp
        endif
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(:,i) = vec_y(:,i) + alpha_*data(i,k) * vec_x(:,j)
                    end do
                end do
            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(:,j) = vec_y(:,j) + alpha_*data(i,k) * vec_x(:,i)
                    end do
                end do
            else if( storage /= sparse_full .and. op_/=sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(:,i) = vec_y(:,i) + alpha_*data(i,k) * vec_x(:,j)
                        if(i==j) cycle 
                        vec_y(:,j) = vec_y(:,j) + alpha_*data(i,k) * vec_x(:,i)
                    end do
                end do
            end if
    end subroutine

    module subroutine spmv_ell_1d_csp(matrix,vec_x,vec_y,alpha,beta,op)
        type(ELL_csp_type), intent(in) :: matrix
        complex(sp), intent(in)    :: vec_x(:)
        complex(sp), intent(inout) :: vec_y(:)
        complex(sp), intent(in), optional :: alpha
        complex(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op

        call spmv_ell_sub_1d_csp(matrix%data,matrix%index,matrix%K, &
            matrix%nnz,matrix%nrows,matrix%ncols,matrix%storage, &
            vec_x,vec_y,alpha,beta,op)
    end subroutine
    
    module subroutine spmv_ell_sub_1d_csp(data,index,mnz_p_row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
        complex(sp), intent(in) :: data(:,:)
        integer(ilp), intent(in) :: index(:,:)
        integer, intent(in) :: mnz_p_row
        integer(ilp), intent(in) :: nnz
        integer(ilp), intent(in) :: nrows
        integer(ilp), intent(in) :: ncols
        integer, intent(in) :: storage
        complex(sp), intent(in)    :: vec_x(:)
        complex(sp), intent(inout) :: vec_y(:)
        complex(sp), intent(in), optional :: alpha
        complex(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(sp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j, k
        
        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_csp
        endif
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(i) = vec_y(i) + alpha_*data(i,k) * vec_x(j)
                    end do
                end do
            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(j) = vec_y(j) + alpha_*data(i,k) * vec_x(i)
                    end do
                end do
            else if( storage /= sparse_full .and. op_/=sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(i) = vec_y(i) + alpha_*data(i,k) * vec_x(j)
                        if(i==j) cycle 
                        vec_y(j) = vec_y(j) + alpha_*data(i,k) * vec_x(i)
                    end do
                end do
            else if( storage == sparse_full .and. op_==sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(j) = vec_y(j) + alpha_*conjg(data(i,k)) * vec_x(i)
                    end do
                end do
            else if( storage /= sparse_full .and. op_==sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(i) = vec_y(i) + alpha_*conjg(data(i,k)) * vec_x(j)
                        if(i==j) cycle 
                        vec_y(j) = vec_y(j) + alpha_*conjg(data(i,k)) * vec_x(i)
                    end do
                end do
            end if
    end subroutine

    module subroutine spmv_ell_2d_csp(matrix,vec_x,vec_y,alpha,beta,op)
        type(ELL_csp_type), intent(in) :: matrix
        complex(sp), intent(in)    :: vec_x(:,:)
        complex(sp), intent(inout) :: vec_y(:,:)
        complex(sp), intent(in), optional :: alpha
        complex(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op

        call spmv_ell_sub_2d_csp(matrix%data,matrix%index,matrix%K, &
            matrix%nnz,matrix%nrows,matrix%ncols,matrix%storage, &
            vec_x,vec_y,alpha,beta,op)
    end subroutine
    
    module subroutine spmv_ell_sub_2d_csp(data,index,mnz_p_row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
        complex(sp), intent(in) :: data(:,:)
        integer(ilp), intent(in) :: index(:,:)
        integer, intent(in) :: mnz_p_row
        integer(ilp), intent(in) :: nnz
        integer(ilp), intent(in) :: nrows
        integer(ilp), intent(in) :: ncols
        integer, intent(in) :: storage
        complex(sp), intent(in)    :: vec_x(:,:)
        complex(sp), intent(inout) :: vec_y(:,:)
        complex(sp), intent(in), optional :: alpha
        complex(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(sp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j, k
        
        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_csp
        endif
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(:,i) = vec_y(:,i) + alpha_*data(i,k) * vec_x(:,j)
                    end do
                end do
            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(:,j) = vec_y(:,j) + alpha_*data(i,k) * vec_x(:,i)
                    end do
                end do
            else if( storage /= sparse_full .and. op_/=sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(:,i) = vec_y(:,i) + alpha_*data(i,k) * vec_x(:,j)
                        if(i==j) cycle 
                        vec_y(:,j) = vec_y(:,j) + alpha_*data(i,k) * vec_x(:,i)
                    end do
                end do
            else if( storage == sparse_full .and. op_==sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(:,j) = vec_y(:,j) + alpha_*conjg(data(i,k)) * vec_x(:,i)
                    end do
                end do
            else if( storage /= sparse_full .and. op_==sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(:,i) = vec_y(:,i) + alpha_*conjg(data(i,k)) * vec_x(:,j)
                        if(i==j) cycle 
                        vec_y(:,j) = vec_y(:,j) + alpha_*conjg(data(i,k)) * vec_x(:,i)
                    end do
                end do
            end if
    end subroutine

    module subroutine spmv_ell_1d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
        type(ELL_cdp_type), intent(in) :: matrix
        complex(dp), intent(in)    :: vec_x(:)
        complex(dp), intent(inout) :: vec_y(:)
        complex(dp), intent(in), optional :: alpha
        complex(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op

        call spmv_ell_sub_1d_cdp(matrix%data,matrix%index,matrix%K, &
            matrix%nnz,matrix%nrows,matrix%ncols,matrix%storage, &
            vec_x,vec_y,alpha,beta,op)
    end subroutine
    
    module subroutine spmv_ell_sub_1d_cdp(data,index,mnz_p_row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
        complex(dp), intent(in) :: data(:,:)
        integer(ilp), intent(in) :: index(:,:)
        integer, intent(in) :: mnz_p_row
        integer(ilp), intent(in) :: nnz
        integer(ilp), intent(in) :: nrows
        integer(ilp), intent(in) :: ncols
        integer, intent(in) :: storage
        complex(dp), intent(in)    :: vec_x(:)
        complex(dp), intent(inout) :: vec_y(:)
        complex(dp), intent(in), optional :: alpha
        complex(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(dp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j, k
        
        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_cdp
        endif
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(i) = vec_y(i) + alpha_*data(i,k) * vec_x(j)
                    end do
                end do
            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(j) = vec_y(j) + alpha_*data(i,k) * vec_x(i)
                    end do
                end do
            else if( storage /= sparse_full .and. op_/=sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(i) = vec_y(i) + alpha_*data(i,k) * vec_x(j)
                        if(i==j) cycle 
                        vec_y(j) = vec_y(j) + alpha_*data(i,k) * vec_x(i)
                    end do
                end do
            else if( storage == sparse_full .and. op_==sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(j) = vec_y(j) + alpha_*conjg(data(i,k)) * vec_x(i)
                    end do
                end do
            else if( storage /= sparse_full .and. op_==sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(i) = vec_y(i) + alpha_*conjg(data(i,k)) * vec_x(j)
                        if(i==j) cycle 
                        vec_y(j) = vec_y(j) + alpha_*conjg(data(i,k)) * vec_x(i)
                    end do
                end do
            end if
    end subroutine

    module subroutine spmv_ell_2d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
        type(ELL_cdp_type), intent(in) :: matrix
        complex(dp), intent(in)    :: vec_x(:,:)
        complex(dp), intent(inout) :: vec_y(:,:)
        complex(dp), intent(in), optional :: alpha
        complex(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op

        call spmv_ell_sub_2d_cdp(matrix%data,matrix%index,matrix%K, &
            matrix%nnz,matrix%nrows,matrix%ncols,matrix%storage, &
            vec_x,vec_y,alpha,beta,op)
    end subroutine
    
    module subroutine spmv_ell_sub_2d_cdp(data,index,mnz_p_row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
        complex(dp), intent(in) :: data(:,:)
        integer(ilp), intent(in) :: index(:,:)
        integer, intent(in) :: mnz_p_row
        integer(ilp), intent(in) :: nnz
        integer(ilp), intent(in) :: nrows
        integer(ilp), intent(in) :: ncols
        integer, intent(in) :: storage
        complex(dp), intent(in)    :: vec_x(:,:)
        complex(dp), intent(inout) :: vec_y(:,:)
        complex(dp), intent(in), optional :: alpha
        complex(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(dp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j, k
        
        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_cdp
        endif
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(:,i) = vec_y(:,i) + alpha_*data(i,k) * vec_x(:,j)
                    end do
                end do
            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(:,j) = vec_y(:,j) + alpha_*data(i,k) * vec_x(:,i)
                    end do
                end do
            else if( storage /= sparse_full .and. op_/=sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(:,i) = vec_y(:,i) + alpha_*data(i,k) * vec_x(:,j)
                        if(i==j) cycle 
                        vec_y(:,j) = vec_y(:,j) + alpha_*data(i,k) * vec_x(:,i)
                    end do
                end do
            else if( storage == sparse_full .and. op_==sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(:,j) = vec_y(:,j) + alpha_*conjg(data(i,k)) * vec_x(:,i)
                    end do
                end do
            else if( storage /= sparse_full .and. op_==sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(:,i) = vec_y(:,i) + alpha_*conjg(data(i,k)) * vec_x(:,j)
                        if(i==j) cycle 
                        vec_y(:,j) = vec_y(:,j) + alpha_*conjg(data(i,k)) * vec_x(:,i)
                    end do
                end do
            end if
    end subroutine


end submodule stdlib_sparse_spmv_ell
