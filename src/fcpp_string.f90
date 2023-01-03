module fcpp_string

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

end module fcpp_string
