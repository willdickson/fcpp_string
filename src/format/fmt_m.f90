module fmt_m

    use, intrinsic :: iso_c_binding,  only : c_ptr
    use, intrinsic :: iso_c_binding,  only : c_int
    use, intrinsic :: iso_c_binding,  only : c_int32_t
    use, intrinsic :: iso_c_binding,  only : c_int64_t
    use, intrinsic :: iso_c_binding,  only : c_size_t
    use, intrinsic :: iso_c_binding,  only : c_float
    use, intrinsic :: iso_c_binding,  only : c_double
    use, intrinsic :: iso_c_binding,  only : c_long_double

    use cpp_string_m, only : string_t
    use fmt_cdef,     only : fmt_int32_c
    use fmt_cdef,     only : fmt_int64_c
    use fmt_cdef,     only : fmt_size_c
    use fmt_cdef,     only : fmt_float_c
    use fmt_cdef,     only : fmt_float_with_prec_c
    use fmt_cdef,     only : fmt_double_c
    use fmt_cdef,     only : fmt_double_with_prec_c

    implicit none
    private

    public :: string_fmt

    public :: fmt_int32

    interface string_fmt
        procedure :: fmt_int32
        procedure :: fmt_int32_from_char
        procedure :: fmt_int64
        procedure :: fmt_int64_from_char
        procedure :: fmt_float
        procedure :: fmt_float_from_char
        procedure :: fmt_double
        procedure :: fmt_double_from_char
    end interface string_fmt

contains


    function fmt_int32(fmt_str, val) result(res_str)
        type(string_t),     intent(in) :: fmt_str
        integer(c_int32_t), intent(in) :: val
        type(string_t)                 :: res_str
        type(string_t)                 :: cor_str
        logical                        :: ok
        call correct_fmt_str(fmt_str, cor_str, ok)
        if (ok) then
            res_str = string_t(fmt_int32_c(cor_str % ptr, val))
        else
            res_str = cor_str 
        endif 
    end function fmt_int32


    function fmt_int32_from_char(fmt_chr, val) result(res_str)
        character(*),       intent(in) :: fmt_chr
        integer(c_int32_t), intent(in) :: val
        type(string_t)                 :: res_str
        type(string_t)                 :: fmt_str
        fmt_str = string_t(fmt_chr)
        res_str = fmt_int32(fmt_str, val)
    end function fmt_int32_from_char


    function fmt_int64(fmt_str, val) result(res_str)
        type(string_t),     intent(in) :: fmt_str
        integer(c_int64_t), intent(in) :: val
        type(string_t)                 :: res_str
        type(string_t)                 :: cor_str
        logical                        :: ok
        call correct_fmt_str(fmt_str, cor_str, ok)
        if (ok) then
            res_str = string_t(fmt_int64_c(cor_str % ptr, val))
        else
            res_str = cor_str 
        endif 
    end function fmt_int64


    function fmt_int64_from_char(fmt_chr, val) result(res_str)
        character(*),       intent(in) :: fmt_chr
        integer(c_int64_t), intent(in) :: val
        type(string_t)                 :: res_str
        type(string_t)                 :: fmt_str
        fmt_str = string_t(fmt_chr)
        res_str = fmt_int64(fmt_str, val)
    end function fmt_int64_from_char


    function fmt_float(fmt_str, val, prec) result(res_str)
        type(string_t),    intent(in) :: fmt_str
        real(c_float),     intent(in) :: val
        integer, optional, intent(in) :: prec
        type(string_t)                :: res_str
        type(string_t)                :: cor_str
        integer(c_int)                :: prec_c
        logical                       :: ok
        call correct_fmt_str(fmt_str, cor_str, ok)
        if (ok) then
            if (.not. present(prec)) then
                res_str = string_t(fmt_float_c(cor_str % ptr, val))
            else
                prec_c  = int(prec, kind=c_int)
                res_str = string_t(fmt_float_with_prec_c(cor_str % ptr, val, prec_c))
            end if
        else
            res_str = cor_str
        end if
    end function fmt_float


    function fmt_float_from_char(fmt_chr, val, prec) result(res_str)
        character(*),      intent(in) :: fmt_chr
        real(c_float),     intent(in) :: val
        integer, optional, intent(in) :: prec
        type(string_t)                :: res_str
        type(string_t)                :: fmt_str
        fmt_str = string_t(fmt_chr)
        res_str = fmt_float(fmt_str, val, prec)
    end function fmt_float_from_char


    function fmt_double(fmt_str, val, prec) result(res_str)
        type(string_t),    intent(in) :: fmt_str
        real(c_double),    intent(in) :: val
        integer, optional, intent(in) :: prec
        type(string_t)                :: res_str
        type(string_t)                :: cor_str
        integer(c_int)                :: prec_c
        logical                       :: ok
        call correct_fmt_str(fmt_str, cor_str, ok)
        if (ok) then
            if (.not. present(prec)) then
                res_str = string_t(fmt_double_c(cor_str % ptr, val))
            else
                prec_c  = int(prec, kind=c_int)
                res_str = string_t(fmt_double_with_prec_c(cor_str % ptr, val, prec_c))
            end if
        else
            res_str = cor_str
        end if
    end function fmt_double


    function fmt_double_from_char(fmt_chr, val, prec) result(res_str)
        character(*),      intent(in) :: fmt_chr
        real(c_double),    intent(in) :: val
        integer, optional, intent(in) :: prec
        type(string_t)                :: res_str
        type(string_t)                :: fmt_str
        fmt_str = string_t(fmt_chr)
        res_str = fmt_double(fmt_str, val, prec)
    end function fmt_double_from_char


    subroutine correct_fmt_str(fmt_str, cor_str, ok)
        type(string_t), intent(in)  :: fmt_str
        type(string_t), intent(out) :: cor_str
        logical,        intent(out) :: ok  
        integer                     :: pos
        ok = .true.
        cor_str = string_t(fmt_str)
        if (fmt_str % find('{:') == 0) then
            ! {: not found in string ... try to add it
            pos = fmt_str % find('{')
            if (pos /= 0) then
                ! Found it ... adding
                call cor_str % insert(pos+1,':')
            else
                ! Not Found .. return empty string
                ok = .false.
                cor_str  = string_t("{format_error: missing '{' in format string}")
            end if
        end if
    end subroutine correct_fmt_str



end module fmt_m
