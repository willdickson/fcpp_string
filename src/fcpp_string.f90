module fcpp_string

    use, intrinsic :: iso_c_binding,  only : c_size_t
    use cpp_string_m, only : string_t
    use cpp_string_m, only : char
    use cpp_string_m, only : len

    implicit none
    private

    public :: string_t
    public :: char
    public :: len

end module fcpp_string
