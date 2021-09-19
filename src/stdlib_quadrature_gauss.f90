submodule (stdlib_quadrature) stdlib_quadrature_gauss
    use stdlib_specialfunctions, only: legendre, dlegendre
    implicit none

    real(dop), parameter :: pi = acos(-1._dop)
    real(dop), parameter :: tolerance = 4._dop * epsilon(1._dop)
    integer, parameter :: newton_iters = 100

contains

    pure module subroutine gauss_legendre_fp64 (x, w, interval)
        real(dop), intent(out) :: x(:), w(:)
        real(dop), intent(in), optional :: interval(2)

        associate (n => size(x)-1 )
        select case (n)
            case (0)
                x = 0
                w = 2
            case (1)
                x(1) = -sqrt(1._dop/3._dop)
                x(2) = -x(1)
                w = 1
            case default
                block
                integer :: i,j
                real(dop) :: leg, dleg, delta

                do i = 0, (n+1)/2 - 1
                    ! use Gauss-Chebyshev points as an initial guess
                    x(i+1) = -cos((2*i+1)/(2._dop*n+2._dop) * pi)
                    do j = 1, newton_iters
                        leg  = legendre(n+1,x(i+1))
                        dleg = dlegendre(n+1,x(i+1))
                        delta = -leg/dleg
                        x(i+1) = x(i+1) + delta
                        if ( abs(delta) <= tolerance * abs(x(i+1)) )  exit
                    end do
                    x(n-i+1) = -x(i+1)

                    dleg = dlegendre(n+1,x(i+1))
                    w(i+1)   = 2._dop/((1-x(i+1)**2)*dleg**2) 
                    w(n-i+1) = w(i+1)
                end do

                if (mod(n,2) == 0) then
                    x(n/2+1) = 0

                    dleg = dlegendre(n+1, 0.0_dop)
                    w(n/2+1) = 2._dop/(dleg**2) 
                end if
                end block
        end select
        end associate

        if (present(interval)) then
            associate ( a => interval(1) , b => interval(2) )
            x = 0.5_dop*(b-a)*x+0.5_dop*(b+a)
            x(1)       = interval(1)
            x(size(x)) = interval(2)
            w = 0.5_dop*(b-a)*w
            end associate
        end if
    end subroutine

    pure module subroutine gauss_legendre_lobatto_fp64 (x, w, interval)
        real(dop), intent(out) :: x(:), w(:)
        real(dop), intent(in), optional :: interval(2)

        associate (n => size(x)-1)
        select case (n)
            case (1)
                x(1) = -1
                x(2) =  1
                w = 1
            case default
                block
                integer :: i,j
                real(dop) :: leg, dleg, delta

                x(1)   = -1._dop
                x(n+1) =  1._dop
                w(1)   =  2._dop/(n*(n+1._dop))
                w(n+1) =  2._dop/(n*(n+1._dop))

                do i = 1, (n+1)/2 - 1
                    ! initial guess from an approximate form given by SV Parter (1999)
                    x(i+1) = -cos( (i+0.25_dop)*pi/n  - 3/(8*n*pi*(i+0.25_dop)))
                    do j = 1, newton_iters
                        leg  = legendre(n+1,x(i+1)) - legendre(n-1,x(i+1))
                        dleg = dlegendre(n+1,x(i+1)) - dlegendre(n-1,x(i+1))
                        delta = -leg/dleg
                        x(i+1) = x(i+1) + delta
                        if ( abs(delta) <= tolerance * abs(x(i+1)) )  exit
                    end do
                    x(n-i+1) = -x(i+1)

                    leg = legendre(n, x(i+1))
                    w(i+1)   = 2._dop/(n*(n+1._dop)*leg**2) 
                    w(n-i+1) = w(i+1)
                end do

                if (mod(n,2) == 0) then
                    x(n/2+1) = 0

                    leg = legendre(n, 0.0_dop)
                    w(n/2+1)   = 2._dop/(n*(n+1._dop)*leg**2) 
                end if
                end block
        end select
        end associate
        
        if (present(interval)) then
            associate ( a => interval(1) , b => interval(2) )
            x = 0.5_dop*(b-a)*x+0.5_dop*(b+a)
            x(1)       = interval(1)
            x(size(x)) = interval(2)
            w = 0.5_dop*(b-a)*w
            end associate
        end if
    end subroutine
end submodule    
