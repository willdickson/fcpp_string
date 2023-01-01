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

end module fcpp_string
