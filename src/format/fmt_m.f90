module fmt_m

    use, intrinsic :: iso_fortran_env, only : int32
    use, intrinsic :: iso_fortran_env, only : int64
    use, intrinsic :: iso_fortran_env, only : real32 
    use, intrinsic :: iso_fortran_env, only : real64

    use, intrinsic :: iso_c_binding,  only : c_ptr
    use, intrinsic :: iso_c_binding,  only : c_int
    use, intrinsic :: iso_c_binding,  only : c_int32_t
    use, intrinsic :: iso_c_binding,  only : c_int64_t
    use, intrinsic :: iso_c_binding,  only : c_size_t
    use, intrinsic :: iso_c_binding,  only : c_float
    use, intrinsic :: iso_c_binding,  only : c_double
    use, intrinsic :: iso_c_binding,  only : c_long_double
    use, intrinsic :: iso_c_binding,  only : c_bool

    use cpp_string_m, only : string_t
    use fmt_cdef,     only : fmt_int32_c
    use fmt_cdef,     only : fmt_int64_c
    use fmt_cdef,     only : fmt_float_c
    use fmt_cdef,     only : fmt_float_with_prec_c
    use fmt_cdef,     only : fmt_double_c
    use fmt_cdef,     only : fmt_double_with_prec_c
    use fmt_cdef,     only : fmt_bool_c
    use fmt_cdef,     only : fmt_string_c

    implicit none
    private

    public :: string_fmt

    public :: fmt_int32

    interface string_fmt
        procedure :: fmt_int32
        procedure :: fmt_int32_from_char
        procedure :: fmt_int64
        procedure :: fmt_int64_from_char
        procedure :: fmt_real32
        procedure :: fmt_real32_from_char
        procedure :: fmt_real64
        procedure :: fmt_real64_from_char
        procedure :: fmt_logical
        procedure :: fmt_logical_from_char
        procedure :: fmt_string
        procedure :: fmt_string_from_char
        procedure :: fmt_char_from_char
        procedure :: fmt_char_from_string
    end interface string_fmt

contains


    function fmt_int32(fmt_str, val) result(res_str)
        type(string_t), intent(in) :: fmt_str
        integer(int32), intent(in) :: val
        type(string_t)             :: res_str
        type(string_t)             :: cor_str
        logical                    :: ok
        integer(c_int32_t)         :: val_c
        call correct_fmt_str(fmt_str, cor_str, ok)
        if (ok) then
            val_c = int(val,kind=c_int32_t)
            res_str = string_t(fmt_int32_c(cor_str % ptr, val_c))
        else
            res_str = cor_str 
        endif 
    end function fmt_int32


    function fmt_int32_from_char(fmt_chr, val) result(res_str)
        character(*),   intent(in) :: fmt_chr
        integer(int32), intent(in) :: val
        type(string_t)             :: res_str
        type(string_t)             :: fmt_str
        fmt_str = string_t(fmt_chr)
        res_str = fmt_int32(fmt_str, val)
    end function fmt_int32_from_char


    function fmt_int64(fmt_str, val) result(res_str)
        type(string_t), intent(in) :: fmt_str
        integer(int64), intent(in) :: val
        type(string_t)             :: res_str
        type(string_t)             :: cor_str
        logical                    :: ok
        integer(c_int64_t)         :: val_c
        call correct_fmt_str(fmt_str, cor_str, ok)
        if (ok) then
            val_c = int(val, kind=c_int64_t)
            res_str = string_t(fmt_int64_c(cor_str % ptr, val_c))
        else
            res_str = cor_str 
        endif 
    end function fmt_int64


    function fmt_int64_from_char(fmt_chr, val) result(res_str)
        character(*),   intent(in) :: fmt_chr
        integer(int64), intent(in) :: val
        type(string_t)             :: res_str
        type(string_t)             :: fmt_str
        fmt_str = string_t(fmt_chr)
        res_str = fmt_int64(fmt_str, val)
    end function fmt_int64_from_char


    function fmt_real32(fmt_str, val, prec) result(res_str)
        type(string_t),    intent(in) :: fmt_str
        real(real32),      intent(in) :: val
        integer, optional, intent(in) :: prec
        type(string_t)                :: res_str
        type(string_t)                :: cor_str
        real(c_float)                 :: val_c
        integer(c_int)                :: prec_c
        type(c_ptr)                   :: ptr
        logical                       :: ok
        call correct_fmt_str(fmt_str, cor_str, ok)
        if (ok) then
            val_c = real(val, kind=c_float)
            if (.not. present(prec)) then
                res_str = string_t(fmt_float_c(cor_str % ptr, val))
            else
                prec_c = int(prec, kind=c_int)
                ptr = fmt_float_with_prec_c(cor_str % ptr, val_c, prec_c)
                res_str = string_t(ptr)
            end if
        else
            res_str = cor_str
        end if
    end function fmt_real32


    function fmt_real32_from_char(fmt_chr, val, prec) result(res_str)
        character(*),      intent(in) :: fmt_chr
        real(real32),      intent(in) :: val
        integer, optional, intent(in) :: prec
        type(string_t)                :: res_str
        type(string_t)                :: fmt_str
        fmt_str = string_t(fmt_chr)
        res_str = fmt_real32(fmt_str, val, prec)
    end function fmt_real32_from_char


    function fmt_real64(fmt_str, val, prec) result(res_str)
        type(string_t),    intent(in) :: fmt_str
        real(real64),      intent(in) :: val
        integer, optional, intent(in) :: prec
        type(string_t)                :: res_str
        type(string_t)                :: cor_str
        real(c_double)                :: val_c
        integer(c_int)                :: prec_c
        type(c_ptr)                   :: ptr
        logical                       :: ok
        call correct_fmt_str(fmt_str, cor_str, ok)
        if (ok) then
            val_c = real(val, kind=c_double)
            if (.not. present(prec)) then
                res_str = string_t(fmt_double_c(cor_str % ptr, val))
            else
                prec_c  = int(prec, kind=c_int)
                ptr = fmt_double_with_prec_c(cor_str % ptr, val_c, prec_c)
                res_str = string_t(ptr)
            end if
        else
            res_str = cor_str
        end if
    end function fmt_real64


    function fmt_real64_from_char(fmt_chr, val, prec) result(res_str)
        character(*),      intent(in) :: fmt_chr
        real(real64),    intent(in) :: val
        integer, optional, intent(in) :: prec
        type(string_t)                :: res_str
        type(string_t)                :: fmt_str
        fmt_str = string_t(fmt_chr)
        res_str = fmt_real64(fmt_str, val, prec)
    end function fmt_real64_from_char


    function fmt_logical(fmt_str, val) result(res_str)
        type(string_t),    intent(in) :: fmt_str
        logical,           intent(in) :: val
        type(string_t)                :: res_str
        type(string_t)                :: cor_str
        logical(c_bool)               :: val_c
        logical                       :: ok

        call correct_fmt_str(fmt_str, cor_str, ok)
        if (ok) then
            val_c = logical(val,kind=c_bool)
            res_str = string_t(fmt_bool_c(cor_str % ptr, val_c))
        else
            res_str = cor_str
        end if
    end function fmt_logical


    function fmt_logical_from_char(fmt_chr, val) result(res_str)
        character(*),      intent(in) :: fmt_chr
        logical,           intent(in) :: val
        type(string_t)                :: res_str
        type(string_t)                :: fmt_str
        fmt_str = string_t(fmt_chr)
        res_str = fmt_logical(fmt_str, val)
    end function fmt_logical_from_char


    function fmt_string(fmt_str, val_str) result(res_str)
        type(string_t),    intent(in) :: fmt_str
        type(string_t),    intent(in) :: val_str
        type(string_t)                :: res_str
        type(string_t)                :: cor_str
        logical                       :: ok
        call correct_fmt_str(fmt_str, cor_str, ok)
        if (ok) then
            res_str = string_t(fmt_string_c(cor_str % ptr, val_str % ptr))
        else
            res_str = cor_str
        end if
    end function fmt_string


    function fmt_string_from_char(fmt_chr, val_str) result(res_str)
        character(*),      intent(in) :: fmt_chr
        type(string_t),    intent(in) :: val_str
        type(string_t)                :: res_str
        type(string_t)                :: fmt_str
        fmt_str = string_t(fmt_chr)
        res_str = fmt_string(fmt_str, val_str)
    end function fmt_string_from_char


    function fmt_char_from_string(fmt_str, val_chr) result(res_str)
        type(string_t),    intent(in) :: fmt_str
        character(*),      intent(in) :: val_chr
        type(string_t)                :: res_str
        type(string_t)                :: val_str
        val_str = string_t(val_chr)
        res_str = fmt_string(fmt_str, val_str)
    end function fmt_char_from_string


    function fmt_char_from_char(fmt_chr, val_chr) result(res_str)
        character(*),      intent(in) :: fmt_chr
        character(*),      intent(in) :: val_chr
        type(string_t)                :: res_str
        type(string_t)                :: fmt_str
        type(string_t)                :: val_str
        fmt_str = string_t(fmt_chr)
        val_str = string_t(val_chr)
        res_str = fmt_string(fmt_str, val_str)
    end function fmt_char_from_char


    ! ------------------------------------------------------------------------------

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
