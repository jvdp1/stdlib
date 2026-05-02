submodule (stdlib_sparse_spmv) stdlib_sparse_spmv_coo
contains

    !! spmv_coo
    module subroutine spmv_coo_1d_sp(matrix,vec_x,vec_y,alpha,beta,op)
        type(COO_sp_type), intent(in) :: matrix
        real(sp), intent(in)    :: vec_x(:)
        real(sp), intent(inout) :: vec_y(:)
        real(sp), intent(in), optional :: alpha
        real(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op

        call spmv_coo_sub_1d_sp(matrix%data, matrix%index, matrix%storage, matrix%nnz, vec_x,vec_y,alpha,beta,op)

    end subroutine

    module subroutine spmv_coo_sub_1d_sp(data, index, storage, nnz, vec_x,vec_y,alpha,beta,op)
        real(sp), intent(in) :: data(:) 
        integer(ilp), intent(in) :: index(:,:) !! Matrix coordinates index(2,nnz)
        integer, intent(in) :: storage !! storage
        integer(ilp), intent(in) :: nnz !! number of non-zero values
        real(sp), intent(in)    :: vec_x(:)
        real(sp), intent(inout) :: vec_y(:)
        real(sp), intent(in), optional :: alpha
        real(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(sp) :: alpha_
        character(1) :: op_
        integer(ilp) :: col_index, k, row_index

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_sp
        endif

        select case(op_)
        case(sparse_op_none)
            if(storage == sparse_full) then
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha_*data(k) * vec_x(col_index)
                end do

            else 
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha_*data(k) * vec_x(col_index)
                    if( row_index==col_index ) cycle
                    vec_y(col_index) = vec_y(col_index) + alpha_*data(k) * vec_x(row_index)
                end do

            end if
        case(sparse_op_transpose)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha_*data(k) * vec_x(col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha_*data(k) * vec_x(col_index)
                    if( row_index==col_index ) cycle
                    vec_y(col_index) = vec_y(col_index) + alpha_*data(k) * vec_x(row_index)
                end do

            end if
        end select
    end subroutine

    module subroutine spmv_coo_2d_sp(matrix,vec_x,vec_y,alpha,beta,op)
        type(COO_sp_type), intent(in) :: matrix
        real(sp), intent(in)    :: vec_x(:,:)
        real(sp), intent(inout) :: vec_y(:,:)
        real(sp), intent(in), optional :: alpha
        real(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op

        call spmv_coo_sub_2d_sp(matrix%data, matrix%index, matrix%storage, matrix%nnz, vec_x,vec_y,alpha,beta,op)

    end subroutine

    module subroutine spmv_coo_sub_2d_sp(data, index, storage, nnz, vec_x,vec_y,alpha,beta,op)
        real(sp), intent(in) :: data(:) 
        integer(ilp), intent(in) :: index(:,:) !! Matrix coordinates index(2,nnz)
        integer, intent(in) :: storage !! storage
        integer(ilp), intent(in) :: nnz !! number of non-zero values
        real(sp), intent(in)    :: vec_x(:,:)
        real(sp), intent(inout) :: vec_y(:,:)
        real(sp), intent(in), optional :: alpha
        real(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(sp) :: alpha_
        character(1) :: op_
        integer(ilp) :: col_index, k, row_index

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_sp
        endif

        select case(op_)
        case(sparse_op_none)
            if(storage == sparse_full) then
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha_*data(k) * vec_x(:,col_index)
                end do

            else 
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha_*data(k) * vec_x(:,col_index)
                    if( row_index==col_index ) cycle
                    vec_y(:,col_index) = vec_y(:,col_index) + alpha_*data(k) * vec_x(:,row_index)
                end do

            end if
        case(sparse_op_transpose)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha_*data(k) * vec_x(:,col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha_*data(k) * vec_x(:,col_index)
                    if( row_index==col_index ) cycle
                    vec_y(:,col_index) = vec_y(:,col_index) + alpha_*data(k) * vec_x(:,row_index)
                end do

            end if
        end select
    end subroutine

    module subroutine spmv_coo_1d_dp(matrix,vec_x,vec_y,alpha,beta,op)
        type(COO_dp_type), intent(in) :: matrix
        real(dp), intent(in)    :: vec_x(:)
        real(dp), intent(inout) :: vec_y(:)
        real(dp), intent(in), optional :: alpha
        real(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op

        call spmv_coo_sub_1d_dp(matrix%data, matrix%index, matrix%storage, matrix%nnz, vec_x,vec_y,alpha,beta,op)

    end subroutine

    module subroutine spmv_coo_sub_1d_dp(data, index, storage, nnz, vec_x,vec_y,alpha,beta,op)
        real(dp), intent(in) :: data(:) 
        integer(ilp), intent(in) :: index(:,:) !! Matrix coordinates index(2,nnz)
        integer, intent(in) :: storage !! storage
        integer(ilp), intent(in) :: nnz !! number of non-zero values
        real(dp), intent(in)    :: vec_x(:)
        real(dp), intent(inout) :: vec_y(:)
        real(dp), intent(in), optional :: alpha
        real(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(dp) :: alpha_
        character(1) :: op_
        integer(ilp) :: col_index, k, row_index

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_dp
        endif

        select case(op_)
        case(sparse_op_none)
            if(storage == sparse_full) then
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha_*data(k) * vec_x(col_index)
                end do

            else 
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha_*data(k) * vec_x(col_index)
                    if( row_index==col_index ) cycle
                    vec_y(col_index) = vec_y(col_index) + alpha_*data(k) * vec_x(row_index)
                end do

            end if
        case(sparse_op_transpose)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha_*data(k) * vec_x(col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha_*data(k) * vec_x(col_index)
                    if( row_index==col_index ) cycle
                    vec_y(col_index) = vec_y(col_index) + alpha_*data(k) * vec_x(row_index)
                end do

            end if
        end select
    end subroutine

    module subroutine spmv_coo_2d_dp(matrix,vec_x,vec_y,alpha,beta,op)
        type(COO_dp_type), intent(in) :: matrix
        real(dp), intent(in)    :: vec_x(:,:)
        real(dp), intent(inout) :: vec_y(:,:)
        real(dp), intent(in), optional :: alpha
        real(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op

        call spmv_coo_sub_2d_dp(matrix%data, matrix%index, matrix%storage, matrix%nnz, vec_x,vec_y,alpha,beta,op)

    end subroutine

    module subroutine spmv_coo_sub_2d_dp(data, index, storage, nnz, vec_x,vec_y,alpha,beta,op)
        real(dp), intent(in) :: data(:) 
        integer(ilp), intent(in) :: index(:,:) !! Matrix coordinates index(2,nnz)
        integer, intent(in) :: storage !! storage
        integer(ilp), intent(in) :: nnz !! number of non-zero values
        real(dp), intent(in)    :: vec_x(:,:)
        real(dp), intent(inout) :: vec_y(:,:)
        real(dp), intent(in), optional :: alpha
        real(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(dp) :: alpha_
        character(1) :: op_
        integer(ilp) :: col_index, k, row_index

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_dp
        endif

        select case(op_)
        case(sparse_op_none)
            if(storage == sparse_full) then
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha_*data(k) * vec_x(:,col_index)
                end do

            else 
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha_*data(k) * vec_x(:,col_index)
                    if( row_index==col_index ) cycle
                    vec_y(:,col_index) = vec_y(:,col_index) + alpha_*data(k) * vec_x(:,row_index)
                end do

            end if
        case(sparse_op_transpose)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha_*data(k) * vec_x(:,col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha_*data(k) * vec_x(:,col_index)
                    if( row_index==col_index ) cycle
                    vec_y(:,col_index) = vec_y(:,col_index) + alpha_*data(k) * vec_x(:,row_index)
                end do

            end if
        end select
    end subroutine

    module subroutine spmv_coo_1d_csp(matrix,vec_x,vec_y,alpha,beta,op)
        type(COO_csp_type), intent(in) :: matrix
        complex(sp), intent(in)    :: vec_x(:)
        complex(sp), intent(inout) :: vec_y(:)
        complex(sp), intent(in), optional :: alpha
        complex(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op

        call spmv_coo_sub_1d_csp(matrix%data, matrix%index, matrix%storage, matrix%nnz, vec_x,vec_y,alpha,beta,op)

    end subroutine

    module subroutine spmv_coo_sub_1d_csp(data, index, storage, nnz, vec_x,vec_y,alpha,beta,op)
        complex(sp), intent(in) :: data(:) 
        integer(ilp), intent(in) :: index(:,:) !! Matrix coordinates index(2,nnz)
        integer, intent(in) :: storage !! storage
        integer(ilp), intent(in) :: nnz !! number of non-zero values
        complex(sp), intent(in)    :: vec_x(:)
        complex(sp), intent(inout) :: vec_y(:)
        complex(sp), intent(in), optional :: alpha
        complex(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(sp) :: alpha_
        character(1) :: op_
        integer(ilp) :: col_index, k, row_index

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_csp
        endif

        select case(op_)
        case(sparse_op_none)
            if(storage == sparse_full) then
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha_*data(k) * vec_x(col_index)
                end do

            else 
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha_*data(k) * vec_x(col_index)
                    if( row_index==col_index ) cycle
                    vec_y(col_index) = vec_y(col_index) + alpha_*data(k) * vec_x(row_index)
                end do

            end if
        case(sparse_op_transpose)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha_*data(k) * vec_x(col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha_*data(k) * vec_x(col_index)
                    if( row_index==col_index ) cycle
                    vec_y(col_index) = vec_y(col_index) + alpha_*data(k) * vec_x(row_index)
                end do

            end if
        case(sparse_op_hermitian)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha_*conjg(data(k)) * vec_x(col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha_*conjg(data(k)) * vec_x(col_index)
                    if( row_index==col_index ) cycle
                    vec_y(col_index) = vec_y(col_index) + alpha_*conjg(data(k)) * vec_x(row_index)
                end do

            end if
        end select
    end subroutine

    module subroutine spmv_coo_2d_csp(matrix,vec_x,vec_y,alpha,beta,op)
        type(COO_csp_type), intent(in) :: matrix
        complex(sp), intent(in)    :: vec_x(:,:)
        complex(sp), intent(inout) :: vec_y(:,:)
        complex(sp), intent(in), optional :: alpha
        complex(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op

        call spmv_coo_sub_2d_csp(matrix%data, matrix%index, matrix%storage, matrix%nnz, vec_x,vec_y,alpha,beta,op)

    end subroutine

    module subroutine spmv_coo_sub_2d_csp(data, index, storage, nnz, vec_x,vec_y,alpha,beta,op)
        complex(sp), intent(in) :: data(:) 
        integer(ilp), intent(in) :: index(:,:) !! Matrix coordinates index(2,nnz)
        integer, intent(in) :: storage !! storage
        integer(ilp), intent(in) :: nnz !! number of non-zero values
        complex(sp), intent(in)    :: vec_x(:,:)
        complex(sp), intent(inout) :: vec_y(:,:)
        complex(sp), intent(in), optional :: alpha
        complex(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(sp) :: alpha_
        character(1) :: op_
        integer(ilp) :: col_index, k, row_index

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_csp
        endif

        select case(op_)
        case(sparse_op_none)
            if(storage == sparse_full) then
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha_*data(k) * vec_x(:,col_index)
                end do

            else 
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha_*data(k) * vec_x(:,col_index)
                    if( row_index==col_index ) cycle
                    vec_y(:,col_index) = vec_y(:,col_index) + alpha_*data(k) * vec_x(:,row_index)
                end do

            end if
        case(sparse_op_transpose)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha_*data(k) * vec_x(:,col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha_*data(k) * vec_x(:,col_index)
                    if( row_index==col_index ) cycle
                    vec_y(:,col_index) = vec_y(:,col_index) + alpha_*data(k) * vec_x(:,row_index)
                end do

            end if
        case(sparse_op_hermitian)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha_*conjg(data(k)) * vec_x(:,col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha_*conjg(data(k)) * vec_x(:,col_index)
                    if( row_index==col_index ) cycle
                    vec_y(:,col_index) = vec_y(:,col_index) + alpha_*conjg(data(k)) * vec_x(:,row_index)
                end do

            end if
        end select
    end subroutine

    module subroutine spmv_coo_1d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
        type(COO_cdp_type), intent(in) :: matrix
        complex(dp), intent(in)    :: vec_x(:)
        complex(dp), intent(inout) :: vec_y(:)
        complex(dp), intent(in), optional :: alpha
        complex(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op

        call spmv_coo_sub_1d_cdp(matrix%data, matrix%index, matrix%storage, matrix%nnz, vec_x,vec_y,alpha,beta,op)

    end subroutine

    module subroutine spmv_coo_sub_1d_cdp(data, index, storage, nnz, vec_x,vec_y,alpha,beta,op)
        complex(dp), intent(in) :: data(:) 
        integer(ilp), intent(in) :: index(:,:) !! Matrix coordinates index(2,nnz)
        integer, intent(in) :: storage !! storage
        integer(ilp), intent(in) :: nnz !! number of non-zero values
        complex(dp), intent(in)    :: vec_x(:)
        complex(dp), intent(inout) :: vec_y(:)
        complex(dp), intent(in), optional :: alpha
        complex(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(dp) :: alpha_
        character(1) :: op_
        integer(ilp) :: col_index, k, row_index

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_cdp
        endif

        select case(op_)
        case(sparse_op_none)
            if(storage == sparse_full) then
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha_*data(k) * vec_x(col_index)
                end do

            else 
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha_*data(k) * vec_x(col_index)
                    if( row_index==col_index ) cycle
                    vec_y(col_index) = vec_y(col_index) + alpha_*data(k) * vec_x(row_index)
                end do

            end if
        case(sparse_op_transpose)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha_*data(k) * vec_x(col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha_*data(k) * vec_x(col_index)
                    if( row_index==col_index ) cycle
                    vec_y(col_index) = vec_y(col_index) + alpha_*data(k) * vec_x(row_index)
                end do

            end if
        case(sparse_op_hermitian)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha_*conjg(data(k)) * vec_x(col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha_*conjg(data(k)) * vec_x(col_index)
                    if( row_index==col_index ) cycle
                    vec_y(col_index) = vec_y(col_index) + alpha_*conjg(data(k)) * vec_x(row_index)
                end do

            end if
        end select
    end subroutine

    module subroutine spmv_coo_2d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
        type(COO_cdp_type), intent(in) :: matrix
        complex(dp), intent(in)    :: vec_x(:,:)
        complex(dp), intent(inout) :: vec_y(:,:)
        complex(dp), intent(in), optional :: alpha
        complex(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op

        call spmv_coo_sub_2d_cdp(matrix%data, matrix%index, matrix%storage, matrix%nnz, vec_x,vec_y,alpha,beta,op)

    end subroutine

    module subroutine spmv_coo_sub_2d_cdp(data, index, storage, nnz, vec_x,vec_y,alpha,beta,op)
        complex(dp), intent(in) :: data(:) 
        integer(ilp), intent(in) :: index(:,:) !! Matrix coordinates index(2,nnz)
        integer, intent(in) :: storage !! storage
        integer(ilp), intent(in) :: nnz !! number of non-zero values
        complex(dp), intent(in)    :: vec_x(:,:)
        complex(dp), intent(inout) :: vec_y(:,:)
        complex(dp), intent(in), optional :: alpha
        complex(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(dp) :: alpha_
        character(1) :: op_
        integer(ilp) :: col_index, k, row_index

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_cdp
        endif

        select case(op_)
        case(sparse_op_none)
            if(storage == sparse_full) then
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha_*data(k) * vec_x(:,col_index)
                end do

            else 
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha_*data(k) * vec_x(:,col_index)
                    if( row_index==col_index ) cycle
                    vec_y(:,col_index) = vec_y(:,col_index) + alpha_*data(k) * vec_x(:,row_index)
                end do

            end if
        case(sparse_op_transpose)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha_*data(k) * vec_x(:,col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha_*data(k) * vec_x(:,col_index)
                    if( row_index==col_index ) cycle
                    vec_y(:,col_index) = vec_y(:,col_index) + alpha_*data(k) * vec_x(:,row_index)
                end do

            end if
        case(sparse_op_hermitian)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha_*conjg(data(k)) * vec_x(:,col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha_*conjg(data(k)) * vec_x(:,col_index)
                    if( row_index==col_index ) cycle
                    vec_y(:,col_index) = vec_y(:,col_index) + alpha_*conjg(data(k)) * vec_x(:,row_index)
                end do

            end if
        end select
    end subroutine


end submodule stdlib_sparse_spmv_coo
