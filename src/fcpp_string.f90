module fcpp_string

    use, intrinsic :: iso_fortran_env, only : wp => real32
    use, intrinsic :: iso_c_binding,   only : ip => c_int64_t
    use, intrinsic :: iso_c_binding,   only : c_int32_t
    use, intrinsic :: iso_c_binding,   only : c_int64_t
    use, intrinsic :: iso_c_binding,   only : c_size_t

    use cpp_string_m, only : string_t
    use cpp_string_m, only : char
    use cpp_string_m, only : len
    use fmt_m,        only : string_fmt

    implicit none
    private

    public :: string_t
    public :: char
    public :: len
    public :: string_fmt

    public :: test

contains

    subroutine test
        type(string_t) :: s
        real(wp)       :: rval
        integer(ip)    :: ival
        integer        :: i
        logical        :: flag

        do i = 1, 10
            rval = i/3.2
            print *, i, string_fmt('val = {2.3f}', rval)
        end do

        ival = 10000090 
        print *, string_fmt('integer = {#x}', ival)

        flag = .true.
        print *, string_fmt('flag = |{ ^9}|', flag)
        flag = .false.
        print *, string_fmt('flag = |{ ^9}|', flag)

        print *, string_fmt('string {_^15}', 'hello')

    end subroutine test

end module fcpp_string
