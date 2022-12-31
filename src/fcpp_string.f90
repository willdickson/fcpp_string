module fcpp_string

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
        real           :: val
        integer        :: i

        do i = 1, 100
            val = i/3.2
            print *, i, sfmt('val = {:1.3f}', val)
        end do


    end subroutine test

end module fcpp_string
