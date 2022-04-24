!! The module STDLIB_HASHMAP_WRAPPERS provides wrappers for various
!! entities used by the hash map procedures. These include wrappers for the
!! `key` and `other` data, and hashing procedures to operate on entities of
!! the `key_type`.

module stdlib_hashmap_wrappers

    use, intrinsic :: iso_fortran_env, only : &
        character_storage_size

    use stdlib_hash_32bit

    use stdlib_kinds, only : &
        int8,                &
        int16,               &
        int32,               &
        int64,               &
        dp

    implicit none

    private

!! Public procedures
    public ::                    &
        copy_key,                &
        copy_other,              &
        fibonacci_hash,          &
        fnv_1_hasher,            &
        fnv_1a_hasher,           &
        free_key,                &
        free_other,              &
        get,                     &
        hasher_fun,              &
        operator(==),            &
        seeded_nmhash32_hasher,  &
        seeded_nmhash32x_hasher, &
        seeded_water_hasher,     &
        set

!! Public types
    public ::      &
        key_type,  &
        other_type

!! Public integers
    public ::   &
        int_hash

    integer, parameter ::               &
! Should be 8
        bits_int8  = bit_size(0_int8)

    integer, parameter ::                   &
        bits_char = character_storage_size, &
        bytes_char = bits_char/bits_int8

    type :: key_type
!! Version: Experimental
!!
!! A wrapper type for the key's true type
!        private
        integer(int8), allocatable :: value(:)
    end type key_type

    abstract interface
!! Version: Experimental
!!
!! Abstract interface to a 64 bit hash function operating on a KEY_TYPE
        pure function hasher_fun( key )  result(hash_value)
            import key_type, int_hash
            type(key_type), intent(in)    :: key
            integer(int_hash)             :: hash_value
        end function hasher_fun
    end interface

    type :: other_type
!! Version: Experimental
!!
!! A wrapper type for the other data's true type
!        private
        class(*), allocatable :: value
    end type other_type

    interface get

        module procedure get_char_key,   &
                         get_int8_key

    end interface get


    interface operator(==)
        module procedure equal_keys
    end interface operator(==)

    interface set

        module procedure set_char_key,   &
                         set_int8_key,   &
                         set_other

    end interface set

contains


    pure subroutine copy_key( key_in, key_out )
!! Version: Experimental
!!
!! Copies the contents of the key, key_in, to the key, key_out
!! Arguments:
!!     key_in  - the input key
!!     key_out - the output key
        type(key_type), intent(in)  :: key_in
        type(key_type), intent(out) :: key_out

        key_out % value = key_in % value

    end subroutine copy_key


    subroutine copy_other( other_in, other_out )
!! Version: Experimental
!!
!! Copies the other data, other_in, to the variable, other_out
!! Arguments:
!!     other_in  - the input data
!!     other_out - the output data
        type(other_type), intent(in)  :: other_in
        type(other_type), intent(out) :: other_out

        allocate(other_out % value, source = other_in % value )

    end subroutine copy_other


    function equal_keys( key1, key2 ) result(test) ! Chase's tester
!! Version: Experimental
!!
!! Compares two keys for equality
!! Arguments:
!!     key1 - the first key
!!     key2 - the second key
        logical                    :: test
        type(key_type), intent(in) :: key1
        type(key_type), intent(in) :: key2

        if ( size(key1 % value, kind=int64) /= &
             size(key2 % value, kind=int64) ) then
            test = .false.
            return
        end if

        if ( all( key1 % value == key2 % value ) ) then
            test = .true.
        else
            test = .false.
        end if

    end function equal_keys


    subroutine free_key( key )
!! Version: Experimental
!!
!! Frees the memory in a key
!! Arguments:
!!     key  - the key
        type(key_type), intent(inout) :: key

        if ( allocated( key % value ) ) deallocate( key % value )

    end subroutine free_key


    subroutine free_other( other )
!! Version: Experimental
!!
!! Frees the memory in the other data
!! Arguments:
!!     other  - the other data
        type(other_type), intent(inout) :: other

        if ( allocated( other % value) ) deallocate( other % value )

    end subroutine free_other


    subroutine get_char_key( key, value )
!! Version: Experimental
!!
!! Gets the contents of the key as a CHARACTER string
!! Arguments:
!!     key   - the input key
!!     value - the contents of key mapped to a CHARACTER string
        type(key_type), intent(in)             :: key
        character(:), allocatable, intent(out) :: value

        integer(int64) :: key_as_char
        integer(int64) :: key_size

        key_size = size( key % value, kind=int64 )
        select case( bytes_char )
        case(1)
            key_as_char = key_size
        case(2)
            if ( iand( key_size, 1_int64 ) > 0 ) then
                stop 'KEY does not map to a character string.'
            end if
            key_as_char = ishft( key_size, -1 )
        case(4)
            if ( iand( key_size, 3_int64) > 0 ) then
                stop 'KEY does not map to a character string.'
            end if
            key_as_char = ishft( key_size, -2 )
        case default
            stop 'CHARACTER has an unrecognized size.'
        end select

        allocate( character( len=key_as_char ) :: value )

        value(1:key_as_char) = transfer( key % value, value )

    end subroutine get_char_key

    subroutine get_other( other, value )
!! Version: Experimental
!!
!! Gets the contents of the other as a CLASS(*) string
!! Arguments:
!!     other - the input other data
!!     value - the contents of other mapped to a CLASS(*) variable
        type(other_type), intent(in)       :: other
        class(*), allocatable, intent(out) :: value

        allocate(value, source=other % value)

    end subroutine get_other


    subroutine get_int8_key( key, value )
!! Version: Experimental
!!
!! Gets the contents of the key as an INTEGER(INT8) vector
!! Arguments:
!!     key   - the input key
!!     value - the contents of key mapped to an INTEGER(INT8) vector
        type(key_type), intent(in)              :: key
        integer(int8), allocatable, intent(out) :: value(:)

        value = key % value

    end subroutine get_int8_key


    subroutine set_char_key( key, value )
!! Version: Experimental
!!
!! Sets the contents of the key from a CHARACTER string
!! Arguments:
!!     key   - the output key
!!     value - the input CHARACTER string
        type(key_type), intent(out) :: key
        character(*), intent(in)    :: value(:)

        key % value = transfer( value, key % value, &
                                bytes_char * len( value ) )

    end subroutine set_char_key


    subroutine set_other( other, value )
!! Version: Experimental
!!
!! Sets the contents of the other data from a CLASS(*) variable
!! Arguments:
!!     other - the output other data
!!     value - the input CLASS(*) variable
        type(other_type), intent(out) :: other
        class(*), intent(in)          :: value

        allocate(other % value, source=value)

    end subroutine set_other


    subroutine set_int8_key( key, value )
!! Version: Experimental
!!
!! Sets the contents of the key from an INTEGER(INT8) vector
!! Arguments:
!!     key   - the output key
!!     value - the input INTEGER(INT8) vector
        type(key_type), intent(out) :: key
        integer(int8), intent(in)   :: value(:)

        key % value = value

    end subroutine set_int8_key


    pure function fnv_1_hasher( key )
!! Version: Experimental
!!
!! Hashes a key with the FNV_1 algorithm
!! Arguments:
!!     key  - the key to be hashed
        type(key_type), intent(in)    :: key
        integer(int_hash)             :: fnv_1_hasher

        fnv_1_hasher = fnv_1_hash( key % value )

    end function fnv_1_hasher


    pure function fnv_1a_hasher( key )
!! Version: Experimental
!!
!! Hashes a key with the FNV_1a algorithm
!! Arguments:
!!     key  - the key to be hashed
        type(key_type), intent(in)    :: key
        integer(int_hash)             :: fnv_1a_hasher

        fnv_1a_hasher = fnv_1a_hash( key % value )

    end function fnv_1a_hasher


    pure function seeded_nmhash32_hasher( key )
!! Version: Experimental
!!
!! Hashes a key with the NMHASH32 hash algorithm
!! Arguments:
!!     key  - the key to be hashed
!!     seed - the seed (unused) for the hashing algorithm
        type(key_type), intent(in)    :: key
        integer(int_hash)             :: seeded_nmhash32_hasher

        seeded_nmhash32_hasher = nmhash32( key % value, &
            int( z'DEADBEEF', int32 ) )

    end function seeded_nmhash32_hasher


    pure function seeded_nmhash32x_hasher( key )
!! Version: Experimental
!!
!! Hashes a key with the NMHASH32X hash algorithm
!! Arguments:
!!     key  - the key to be hashed
!!     seed - the seed (unused) for the hashing algorithm
        type(key_type), intent(in)    :: key
        integer(int_hash)             :: seeded_nmhash32x_hasher

        seeded_nmhash32x_hasher = nmhash32x( key % value, &
            int( z'DEADBEEF', int32 ) )

    end function seeded_nmhash32x_hasher


    pure function seeded_water_hasher( key )
!! Version: Experimental
!!
!! Hashes a key with the waterhash algorithm
!! Arguments:
!!     key  - the key to be hashed
        type(key_type), intent(in)  :: key
        integer(int_hash)           :: seeded_water_hasher

        seeded_water_hasher = water_hash( key % value, &
            int( z'DEADBEEF1EADBEEF', int64 ) )

    end function seeded_water_hasher


end module stdlib_hashmap_wrappers