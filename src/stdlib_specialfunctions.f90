module stdlib_specialfunctions
    use stdlib_kinds

    implicit none

    private

    public :: legendre 
    public :: dlegendre 


    interface legendre
        !! version: experimental
        !! 
        !! Legendre polynomial
        pure elemental module function legendre_fp64(n,x) result(leg)
            integer, intent(in) :: n
            real(dop), intent(in) :: x
            real(dop) :: leg
        end function
    end interface

    interface dlegendre
        !! version: experimental
        !! 
        !! First derivative Legendre polynomial
        pure elemental module function dlegendre_fp64(n,x) result(dleg)
            integer, intent(in) :: n
            real(dop), intent(in) :: x
            real(dop) :: dleg
        end function
    end interface

end module stdlib_specialfunctions
