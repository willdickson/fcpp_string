module fcpp_string

    use, intrinsic :: iso_fortran_env, only : wp => real32
    use, intrinsic :: iso_c_binding,   only : ip => c_int64_t
    use, intrinsic :: iso_c_binding,   only : c_int32_t
    use, intrinsic :: iso_c_binding,   only : c_int64_t
    use, intrinsic :: iso_c_binding,   only : c_size_t

    use cpp_string_m, only : string_t
    use cpp_string_m, only : char
    use cpp_string_m, only : len
    use fmt_m,        only : sfmt => string_fmt

    implicit none
    private

    public :: string_t
    public :: char
    public :: len
    public :: test

contains

    subroutine test
        type(string_t) :: s
        real(wp)       :: rval
        integer(ip)    :: ival
        integer        :: i

        do i = 1, 10
            rval = i/3.2
            print *, i, sfmt('val = {2.3f}', rval)
        end do

        ival = 53
        print *, sfmt('integer = {}', ival)

        print *, 'kind(i)         = ', kind(i) 
        print *, 'kind(c_int32_t) = ', c_int32_t 
        print *, 'kind(c_int64_t) = ', c_int64_t 
        print *, 'kind(c_size_t)  = ', c_size_t 

    end subroutine test

end module fcpp_string
