!! The `stdlib_sparse_spmv` submodule provides matrix-vector product kernels.
!!
! This code was modified from https://github.com/jalvesz/FSPARSE by its author: Alves Jose
module stdlib_sparse_spmv
    use stdlib_sparse_constants
    use stdlib_sparse_kinds
    implicit none
    private

    !! Version experimental
    !!
    !! Apply the sparse matrix-vector product $$y = \alpha * op(M) * x + \beta * y $$
    !! [Specifications](../page/specs/stdlib_sparse.html#spmv)
    interface spmv
        module subroutine spmv_coo_1d_sp(matrix,vec_x,vec_y,alpha,beta,op)
            type(COO_sp_type), intent(in) :: matrix
            real(sp), intent(in)    :: vec_x(:)
            real(sp), intent(inout) :: vec_y(:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csr_1d_sp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSR_sp_type), intent(in) :: matrix
            real(sp), intent(in)    :: vec_x(:)
            real(sp), intent(inout) :: vec_y(:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csc_1d_sp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSC_sp_type), intent(in) :: matrix
            real(sp), intent(in)    :: vec_x(:)
            real(sp), intent(inout) :: vec_y(:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_ell_1d_sp(matrix,vec_x,vec_y,alpha,beta,op)
            type(ELL_sp_type), intent(in) :: matrix
            real(sp), intent(in)    :: vec_x(:)
            real(sp), intent(inout) :: vec_y(:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_coo_2d_sp(matrix,vec_x,vec_y,alpha,beta,op)
            type(COO_sp_type), intent(in) :: matrix
            real(sp), intent(in)    :: vec_x(:,:)
            real(sp), intent(inout) :: vec_y(:,:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csr_2d_sp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSR_sp_type), intent(in) :: matrix
            real(sp), intent(in)    :: vec_x(:,:)
            real(sp), intent(inout) :: vec_y(:,:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csc_2d_sp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSC_sp_type), intent(in) :: matrix
            real(sp), intent(in)    :: vec_x(:,:)
            real(sp), intent(inout) :: vec_y(:,:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_ell_2d_sp(matrix,vec_x,vec_y,alpha,beta,op)
            type(ELL_sp_type), intent(in) :: matrix
            real(sp), intent(in)    :: vec_x(:,:)
            real(sp), intent(inout) :: vec_y(:,:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_sellc_sp(matrix,vec_x,vec_y,alpha,beta,op)
            type(SELLC_sp_type), intent(in) :: matrix
            real(sp), intent(in)    :: vec_x(:)
            real(sp), intent(inout) :: vec_y(:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_coo_1d_dp(matrix,vec_x,vec_y,alpha,beta,op)
            type(COO_dp_type), intent(in) :: matrix
            real(dp), intent(in)    :: vec_x(:)
            real(dp), intent(inout) :: vec_y(:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csr_1d_dp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSR_dp_type), intent(in) :: matrix
            real(dp), intent(in)    :: vec_x(:)
            real(dp), intent(inout) :: vec_y(:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csc_1d_dp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSC_dp_type), intent(in) :: matrix
            real(dp), intent(in)    :: vec_x(:)
            real(dp), intent(inout) :: vec_y(:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_ell_1d_dp(matrix,vec_x,vec_y,alpha,beta,op)
            type(ELL_dp_type), intent(in) :: matrix
            real(dp), intent(in)    :: vec_x(:)
            real(dp), intent(inout) :: vec_y(:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_coo_2d_dp(matrix,vec_x,vec_y,alpha,beta,op)
            type(COO_dp_type), intent(in) :: matrix
            real(dp), intent(in)    :: vec_x(:,:)
            real(dp), intent(inout) :: vec_y(:,:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csr_2d_dp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSR_dp_type), intent(in) :: matrix
            real(dp), intent(in)    :: vec_x(:,:)
            real(dp), intent(inout) :: vec_y(:,:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csc_2d_dp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSC_dp_type), intent(in) :: matrix
            real(dp), intent(in)    :: vec_x(:,:)
            real(dp), intent(inout) :: vec_y(:,:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_ell_2d_dp(matrix,vec_x,vec_y,alpha,beta,op)
            type(ELL_dp_type), intent(in) :: matrix
            real(dp), intent(in)    :: vec_x(:,:)
            real(dp), intent(inout) :: vec_y(:,:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_sellc_dp(matrix,vec_x,vec_y,alpha,beta,op)
            type(SELLC_dp_type), intent(in) :: matrix
            real(dp), intent(in)    :: vec_x(:)
            real(dp), intent(inout) :: vec_y(:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_coo_1d_csp(matrix,vec_x,vec_y,alpha,beta,op)
            type(COO_csp_type), intent(in) :: matrix
            complex(sp), intent(in)    :: vec_x(:)
            complex(sp), intent(inout) :: vec_y(:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csr_1d_csp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSR_csp_type), intent(in) :: matrix
            complex(sp), intent(in)    :: vec_x(:)
            complex(sp), intent(inout) :: vec_y(:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csc_1d_csp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSC_csp_type), intent(in) :: matrix
            complex(sp), intent(in)    :: vec_x(:)
            complex(sp), intent(inout) :: vec_y(:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_ell_1d_csp(matrix,vec_x,vec_y,alpha,beta,op)
            type(ELL_csp_type), intent(in) :: matrix
            complex(sp), intent(in)    :: vec_x(:)
            complex(sp), intent(inout) :: vec_y(:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_coo_2d_csp(matrix,vec_x,vec_y,alpha,beta,op)
            type(COO_csp_type), intent(in) :: matrix
            complex(sp), intent(in)    :: vec_x(:,:)
            complex(sp), intent(inout) :: vec_y(:,:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csr_2d_csp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSR_csp_type), intent(in) :: matrix
            complex(sp), intent(in)    :: vec_x(:,:)
            complex(sp), intent(inout) :: vec_y(:,:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csc_2d_csp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSC_csp_type), intent(in) :: matrix
            complex(sp), intent(in)    :: vec_x(:,:)
            complex(sp), intent(inout) :: vec_y(:,:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_ell_2d_csp(matrix,vec_x,vec_y,alpha,beta,op)
            type(ELL_csp_type), intent(in) :: matrix
            complex(sp), intent(in)    :: vec_x(:,:)
            complex(sp), intent(inout) :: vec_y(:,:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_sellc_csp(matrix,vec_x,vec_y,alpha,beta,op)
            type(SELLC_csp_type), intent(in) :: matrix
            complex(sp), intent(in)    :: vec_x(:)
            complex(sp), intent(inout) :: vec_y(:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_coo_1d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
            type(COO_cdp_type), intent(in) :: matrix
            complex(dp), intent(in)    :: vec_x(:)
            complex(dp), intent(inout) :: vec_y(:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csr_1d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSR_cdp_type), intent(in) :: matrix
            complex(dp), intent(in)    :: vec_x(:)
            complex(dp), intent(inout) :: vec_y(:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csc_1d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSC_cdp_type), intent(in) :: matrix
            complex(dp), intent(in)    :: vec_x(:)
            complex(dp), intent(inout) :: vec_y(:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_ell_1d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
            type(ELL_cdp_type), intent(in) :: matrix
            complex(dp), intent(in)    :: vec_x(:)
            complex(dp), intent(inout) :: vec_y(:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_coo_2d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
            type(COO_cdp_type), intent(in) :: matrix
            complex(dp), intent(in)    :: vec_x(:,:)
            complex(dp), intent(inout) :: vec_y(:,:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csr_2d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSR_cdp_type), intent(in) :: matrix
            complex(dp), intent(in)    :: vec_x(:,:)
            complex(dp), intent(inout) :: vec_y(:,:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csc_2d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSC_cdp_type), intent(in) :: matrix
            complex(dp), intent(in)    :: vec_x(:,:)
            complex(dp), intent(inout) :: vec_y(:,:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_ell_2d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
            type(ELL_cdp_type), intent(in) :: matrix
            complex(dp), intent(in)    :: vec_x(:,:)
            complex(dp), intent(inout) :: vec_y(:,:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_sellc_cdp(matrix,vec_x,vec_y,alpha,beta,op)
            type(SELLC_cdp_type), intent(in) :: matrix
            complex(dp), intent(in)    :: vec_x(:)
            complex(dp), intent(inout) :: vec_y(:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
    end interface

    !! Version experimental
    !!
    !! Apply the COO sparse matrix-vector product $$y = \alpha * op(M) * x + \beta * y $$
    !! [Specifications](../page/specs/stdlib_sparse.html#spmv)
    interface spmv_coo
        module subroutine spmv_coo_sub_1d_sp(data, index, nnz, storage, vec_x,vec_y,alpha,beta,op)
            real(sp), intent(in) :: data(:)
            integer(ilp), intent(in) :: index(:,:)
            integer, intent(in) :: storage
            integer(ilp), intent(in) :: nnz
            real(sp), intent(in)    :: vec_x(:)
            real(sp), intent(inout) :: vec_y(:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_coo_sub_2d_sp(data, index, nnz, storage, vec_x,vec_y,alpha,beta,op)
            real(sp), intent(in) :: data(:)
            integer(ilp), intent(in) :: index(:,:)
            integer, intent(in) :: storage
            integer(ilp), intent(in) :: nnz
            real(sp), intent(in)    :: vec_x(:,:)
            real(sp), intent(inout) :: vec_y(:,:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_coo_sub_1d_dp(data, index, nnz, storage, vec_x,vec_y,alpha,beta,op)
            real(dp), intent(in) :: data(:)
            integer(ilp), intent(in) :: index(:,:)
            integer, intent(in) :: storage
            integer(ilp), intent(in) :: nnz
            real(dp), intent(in)    :: vec_x(:)
            real(dp), intent(inout) :: vec_y(:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_coo_sub_2d_dp(data, index, nnz, storage, vec_x,vec_y,alpha,beta,op)
            real(dp), intent(in) :: data(:)
            integer(ilp), intent(in) :: index(:,:)
            integer, intent(in) :: storage
            integer(ilp), intent(in) :: nnz
            real(dp), intent(in)    :: vec_x(:,:)
            real(dp), intent(inout) :: vec_y(:,:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_coo_sub_1d_csp(data, index, nnz, storage, vec_x,vec_y,alpha,beta,op)
            complex(sp), intent(in) :: data(:)
            integer(ilp), intent(in) :: index(:,:)
            integer, intent(in) :: storage
            integer(ilp), intent(in) :: nnz
            complex(sp), intent(in)    :: vec_x(:)
            complex(sp), intent(inout) :: vec_y(:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_coo_sub_2d_csp(data, index, nnz, storage, vec_x,vec_y,alpha,beta,op)
            complex(sp), intent(in) :: data(:)
            integer(ilp), intent(in) :: index(:,:)
            integer, intent(in) :: storage
            integer(ilp), intent(in) :: nnz
            complex(sp), intent(in)    :: vec_x(:,:)
            complex(sp), intent(inout) :: vec_y(:,:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_coo_sub_1d_cdp(data, index, nnz, storage, vec_x,vec_y,alpha,beta,op)
            complex(dp), intent(in) :: data(:)
            integer(ilp), intent(in) :: index(:,:)
            integer, intent(in) :: storage
            integer(ilp), intent(in) :: nnz
            complex(dp), intent(in)    :: vec_x(:)
            complex(dp), intent(inout) :: vec_y(:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_coo_sub_2d_cdp(data, index, nnz, storage, vec_x,vec_y,alpha,beta,op)
            complex(dp), intent(in) :: data(:)
            integer(ilp), intent(in) :: index(:,:)
            integer, intent(in) :: storage
            integer(ilp), intent(in) :: nnz
            complex(dp), intent(in)    :: vec_x(:,:)
            complex(dp), intent(inout) :: vec_y(:,:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
    end interface

    !! Version experimental
    !!
    !! Apply the CSC sparse matrix-vector product $$y = \alpha * op(M) * x + \beta * y $$
    !! [Specifications](../page/specs/stdlib_sparse.html#spmv)
    interface spmv_csc
        module subroutine spmv_csc_sub_1d_sp(data,colptr,row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            real(sp), intent(in) :: data(:)
            integer(ilp), intent(in) :: colptr(:) !! matrix column pointer
            integer(ilp), intent(in) :: row(:)  !! matrix row pointer
            integer(ilp), intent(in) :: nnz !! number of non-zero values
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage !! storage
            real(sp), intent(in)    :: vec_x(:)
            real(sp), intent(inout) :: vec_y(:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_csc_sub_2d_sp(data,colptr,row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            real(sp), intent(in) :: data(:)
            integer(ilp), intent(in) :: colptr(:) !! matrix column pointer
            integer(ilp), intent(in) :: row(:)  !! matrix row pointer
            integer(ilp), intent(in) :: nnz !! number of non-zero values
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage !! storage
            real(sp), intent(in)    :: vec_x(:,:)
            real(sp), intent(inout) :: vec_y(:,:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_csc_sub_1d_dp(data,colptr,row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            real(dp), intent(in) :: data(:)
            integer(ilp), intent(in) :: colptr(:) !! matrix column pointer
            integer(ilp), intent(in) :: row(:)  !! matrix row pointer
            integer(ilp), intent(in) :: nnz !! number of non-zero values
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage !! storage
            real(dp), intent(in)    :: vec_x(:)
            real(dp), intent(inout) :: vec_y(:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_csc_sub_2d_dp(data,colptr,row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            real(dp), intent(in) :: data(:)
            integer(ilp), intent(in) :: colptr(:) !! matrix column pointer
            integer(ilp), intent(in) :: row(:)  !! matrix row pointer
            integer(ilp), intent(in) :: nnz !! number of non-zero values
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage !! storage
            real(dp), intent(in)    :: vec_x(:,:)
            real(dp), intent(inout) :: vec_y(:,:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_csc_sub_1d_csp(data,colptr,row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            complex(sp), intent(in) :: data(:)
            integer(ilp), intent(in) :: colptr(:) !! matrix column pointer
            integer(ilp), intent(in) :: row(:)  !! matrix row pointer
            integer(ilp), intent(in) :: nnz !! number of non-zero values
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage !! storage
            complex(sp), intent(in)    :: vec_x(:)
            complex(sp), intent(inout) :: vec_y(:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_csc_sub_2d_csp(data,colptr,row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            complex(sp), intent(in) :: data(:)
            integer(ilp), intent(in) :: colptr(:) !! matrix column pointer
            integer(ilp), intent(in) :: row(:)  !! matrix row pointer
            integer(ilp), intent(in) :: nnz !! number of non-zero values
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage !! storage
            complex(sp), intent(in)    :: vec_x(:,:)
            complex(sp), intent(inout) :: vec_y(:,:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_csc_sub_1d_cdp(data,colptr,row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            complex(dp), intent(in) :: data(:)
            integer(ilp), intent(in) :: colptr(:) !! matrix column pointer
            integer(ilp), intent(in) :: row(:)  !! matrix row pointer
            integer(ilp), intent(in) :: nnz !! number of non-zero values
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage !! storage
            complex(dp), intent(in)    :: vec_x(:)
            complex(dp), intent(inout) :: vec_y(:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_csc_sub_2d_cdp(data,colptr,row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            complex(dp), intent(in) :: data(:)
            integer(ilp), intent(in) :: colptr(:) !! matrix column pointer
            integer(ilp), intent(in) :: row(:)  !! matrix row pointer
            integer(ilp), intent(in) :: nnz !! number of non-zero values
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage !! storage
            complex(dp), intent(in)    :: vec_x(:,:)
            complex(dp), intent(inout) :: vec_y(:,:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
    end interface

    !! Version experimental
    !!
    !! Apply the CSR sparse matrix-vector product $$y = \alpha * op(M) * x + \beta * y $$
    !! [Specifications](../page/specs/stdlib_sparse.html#spmv)
    interface spmv_csr
        module subroutine spmv_csr_sub_1d_sp(data,col,rowptr,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            real(sp), intent(in) :: data(:)
            integer(ilp), intent(in) :: col(:) !! matrix column pointer
            integer(ilp), intent(in) :: rowptr(:)  !! matrix row pointer
            integer(ilp), intent(in) :: nnz !! number of non-zero values
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage !! storage
            real(sp), intent(in)    :: vec_x(:)
            real(sp), intent(inout) :: vec_y(:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_csr_sub_2d_sp(data,col,rowptr,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            real(sp), intent(in) :: data(:)
            integer(ilp), intent(in) :: col(:) !! matrix column pointer
            integer(ilp), intent(in) :: rowptr(:)  !! matrix row pointer
            integer(ilp), intent(in) :: nnz !! number of non-zero values
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage !! storage
            real(sp), intent(in)    :: vec_x(:,:)
            real(sp), intent(inout) :: vec_y(:,:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_csr_sub_1d_dp(data,col,rowptr,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            real(dp), intent(in) :: data(:)
            integer(ilp), intent(in) :: col(:) !! matrix column pointer
            integer(ilp), intent(in) :: rowptr(:)  !! matrix row pointer
            integer(ilp), intent(in) :: nnz !! number of non-zero values
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage !! storage
            real(dp), intent(in)    :: vec_x(:)
            real(dp), intent(inout) :: vec_y(:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_csr_sub_2d_dp(data,col,rowptr,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            real(dp), intent(in) :: data(:)
            integer(ilp), intent(in) :: col(:) !! matrix column pointer
            integer(ilp), intent(in) :: rowptr(:)  !! matrix row pointer
            integer(ilp), intent(in) :: nnz !! number of non-zero values
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage !! storage
            real(dp), intent(in)    :: vec_x(:,:)
            real(dp), intent(inout) :: vec_y(:,:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_csr_sub_1d_csp(data,col,rowptr,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            complex(sp), intent(in) :: data(:)
            integer(ilp), intent(in) :: col(:) !! matrix column pointer
            integer(ilp), intent(in) :: rowptr(:)  !! matrix row pointer
            integer(ilp), intent(in) :: nnz !! number of non-zero values
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage !! storage
            complex(sp), intent(in)    :: vec_x(:)
            complex(sp), intent(inout) :: vec_y(:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_csr_sub_2d_csp(data,col,rowptr,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            complex(sp), intent(in) :: data(:)
            integer(ilp), intent(in) :: col(:) !! matrix column pointer
            integer(ilp), intent(in) :: rowptr(:)  !! matrix row pointer
            integer(ilp), intent(in) :: nnz !! number of non-zero values
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage !! storage
            complex(sp), intent(in)    :: vec_x(:,:)
            complex(sp), intent(inout) :: vec_y(:,:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_csr_sub_1d_cdp(data,col,rowptr,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            complex(dp), intent(in) :: data(:)
            integer(ilp), intent(in) :: col(:) !! matrix column pointer
            integer(ilp), intent(in) :: rowptr(:)  !! matrix row pointer
            integer(ilp), intent(in) :: nnz !! number of non-zero values
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage !! storage
            complex(dp), intent(in)    :: vec_x(:)
            complex(dp), intent(inout) :: vec_y(:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_csr_sub_2d_cdp(data,col,rowptr,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            complex(dp), intent(in) :: data(:)
            integer(ilp), intent(in) :: col(:) !! matrix column pointer
            integer(ilp), intent(in) :: rowptr(:)  !! matrix row pointer
            integer(ilp), intent(in) :: nnz !! number of non-zero values
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage !! storage
            complex(dp), intent(in)    :: vec_x(:,:)
            complex(dp), intent(inout) :: vec_y(:,:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
    end interface

    !! Version experimental
    !!
    !! Apply the ELL sparse matrix-vector product $$y = \alpha * op(M) * x + \beta * y $$
    !! [Specifications](../page/specs/stdlib_sparse.html#spmv)
    interface spmv_ell
        module subroutine spmv_ell_sub_1d_sp(data,index,mnz_p_row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            real(sp), intent(in) :: data(:,:)
            integer(ilp), intent(in) :: index(:,:)
            integer(ilp), intent(in) :: mnz_p_row
            integer(ilp), intent(in) :: nnz
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage
            real(sp), intent(in)    :: vec_x(:)
            real(sp), intent(inout) :: vec_y(:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_ell_sub_2d_sp(data,index,mnz_p_row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            real(sp), intent(in) :: data(:,:)
            integer(ilp), intent(in) :: index(:,:)
            integer(ilp), intent(in) :: mnz_p_row
            integer(ilp), intent(in) :: nnz
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage
            real(sp), intent(in)    :: vec_x(:,:)
            real(sp), intent(inout) :: vec_y(:,:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_ell_sub_1d_dp(data,index,mnz_p_row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            real(dp), intent(in) :: data(:,:)
            integer(ilp), intent(in) :: index(:,:)
            integer(ilp), intent(in) :: mnz_p_row
            integer(ilp), intent(in) :: nnz
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage
            real(dp), intent(in)    :: vec_x(:)
            real(dp), intent(inout) :: vec_y(:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_ell_sub_2d_dp(data,index,mnz_p_row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            real(dp), intent(in) :: data(:,:)
            integer(ilp), intent(in) :: index(:,:)
            integer(ilp), intent(in) :: mnz_p_row
            integer(ilp), intent(in) :: nnz
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage
            real(dp), intent(in)    :: vec_x(:,:)
            real(dp), intent(inout) :: vec_y(:,:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_ell_sub_1d_csp(data,index,mnz_p_row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            complex(sp), intent(in) :: data(:,:)
            integer(ilp), intent(in) :: index(:,:)
            integer(ilp), intent(in) :: mnz_p_row
            integer(ilp), intent(in) :: nnz
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage
            complex(sp), intent(in)    :: vec_x(:)
            complex(sp), intent(inout) :: vec_y(:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_ell_sub_2d_csp(data,index,mnz_p_row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            complex(sp), intent(in) :: data(:,:)
            integer(ilp), intent(in) :: index(:,:)
            integer(ilp), intent(in) :: mnz_p_row
            integer(ilp), intent(in) :: nnz
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage
            complex(sp), intent(in)    :: vec_x(:,:)
            complex(sp), intent(inout) :: vec_y(:,:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_ell_sub_1d_cdp(data,index,mnz_p_row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            complex(dp), intent(in) :: data(:,:)
            integer(ilp), intent(in) :: index(:,:)
            integer(ilp), intent(in) :: mnz_p_row
            integer(ilp), intent(in) :: nnz
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage
            complex(dp), intent(in)    :: vec_x(:)
            complex(dp), intent(inout) :: vec_y(:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_ell_sub_2d_cdp(data,index,mnz_p_row,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            complex(dp), intent(in) :: data(:,:)
            integer(ilp), intent(in) :: index(:,:)
            integer(ilp), intent(in) :: mnz_p_row
            integer(ilp), intent(in) :: nnz
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage
            complex(dp), intent(in)    :: vec_x(:,:)
            complex(dp), intent(inout) :: vec_y(:,:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
    end interface

    !! Version experimental
    !!
    !! Apply the SELLC sparse matrix-vector product $$y = \alpha * op(M) * x + \beta * y $$
    !! [Specifications](../page/specs/stdlib_sparse.html#spmv)
    interface spmv_sellc
        module subroutine spmv_sellc_sub_sp(data,ia,ja,cs,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            !! This algorithm was gracefully provided by Ivan Privec and adapted by Jose Alves
            real(sp), intent(in) :: data(:,:)
            integer(ilp), intent(in) :: ia(:)
            integer(ilp), intent(in) :: ja(:,:)
            integer, intent(in) :: cs
            integer(ilp), intent(in) :: nnz
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage
            real(sp), intent(in)    :: vec_x(:)
            real(sp), intent(inout) :: vec_y(:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_sellc_sub_dp(data,ia,ja,cs,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            !! This algorithm was gracefully provided by Ivan Privec and adapted by Jose Alves
            real(dp), intent(in) :: data(:,:)
            integer(ilp), intent(in) :: ia(:)
            integer(ilp), intent(in) :: ja(:,:)
            integer, intent(in) :: cs
            integer(ilp), intent(in) :: nnz
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage
            real(dp), intent(in)    :: vec_x(:)
            real(dp), intent(inout) :: vec_y(:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_sellc_sub_csp(data,ia,ja,cs,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            !! This algorithm was gracefully provided by Ivan Privec and adapted by Jose Alves
            complex(sp), intent(in) :: data(:,:)
            integer(ilp), intent(in) :: ia(:)
            integer(ilp), intent(in) :: ja(:,:)
            integer, intent(in) :: cs
            integer(ilp), intent(in) :: nnz
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage
            complex(sp), intent(in)    :: vec_x(:)
            complex(sp), intent(inout) :: vec_y(:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_sellc_sub_cdp(data,ia,ja,cs,nnz,nrows,ncols,storage,vec_x,vec_y,alpha,beta,op)
            !! This algorithm was gracefully provided by Ivan Privec and adapted by Jose Alves
            complex(dp), intent(in) :: data(:,:)
            integer(ilp), intent(in) :: ia(:)
            integer(ilp), intent(in) :: ja(:,:)
            integer, intent(in) :: cs
            integer(ilp), intent(in) :: nnz
            integer(ilp), intent(in) :: nrows
            integer(ilp), intent(in) :: ncols
            integer, intent(in) :: storage
            complex(dp), intent(in)    :: vec_x(:)
            complex(dp), intent(inout) :: vec_y(:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
    end interface

    public :: spmv
    public :: spmv_coo
    public :: spmv_csc
    public :: spmv_csr
    public :: spmv_ell
    public :: spmv_sellc

end module
