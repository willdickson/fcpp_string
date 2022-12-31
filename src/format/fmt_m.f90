module fmt_m

    use, intrinsic :: iso_c_binding,  only : c_ptr
    use, intrinsic :: iso_c_binding,  only : c_int
    use, intrinsic :: iso_c_binding,  only : c_float
    use, intrinsic :: iso_c_binding,  only : c_double
    use, intrinsic :: iso_c_binding,  only : c_long_double

    use cpp_string_m, only : string_t
    use fmt_cdef,     only : fmt_float_c
    use fmt_cdef,     only : fmt_float_with_prec_c

    implicit none
    private

    public string_fmt


    interface string_fmt
        procedure :: fmt_float
        procedure :: fmt_float_from_char
    end interface string_fmt


contains

    function fmt_float(fmt_str, flt_val, prec) result(res_str)
        type(string_t),    intent(in) :: fmt_str
        real(c_float),     intent(in) :: flt_val
        integer, optional, intent(in) :: prec
        type(string_t)                :: res_str
        type(string_t)                :: fmt_str_
        integer(c_int)                :: prec_

        fmt_str_ = correct_fmt_str(fmt_str)
        if (fmt_str_ /= string_t('')) then
            if (.not. present(prec)) then
                res_str = string_t(fmt_float_c(fmt_str_ % ptr, flt_val))
            else
                prec_ = int(prec, kind=c_int)
                res_str = string_t(fmt_float_with_prec_c(fmt_str_ % ptr, flt_val, prec_))
            end if
        else
            res_str  = string_t('{FMT ERROR}')
        end if
    end function fmt_float


    function fmt_float_from_char(fmt_chr, flt_val, prec) result(res_str)
        character(*),      intent(in) :: fmt_chr
        real(c_float),     intent(in) :: flt_val
        integer, optional, intent(in) :: prec
        type(string_t)                :: res_str
        type(string_t)                :: fmt_str
        fmt_str = string_t(fmt_chr)
        res_str = fmt_float(fmt_str, flt_val, prec)
    end function fmt_float_from_char


    function correct_fmt_str(fmt_str) result(cor_str)
        type(string_t), intent(in) :: fmt_str
        type(string_t)             :: cor_str
        integer                    :: pos
        cor_str = string_t(fmt_str)
        if (fmt_str % find('{:') == 0) then
            ! {: not found in string ... try to add it
            pos = fmt_str % find('{')
            if (pos /= 0) then
                ! Found it ... adding
                call cor_str % insert(pos+1,':')
            else
                ! Not Found .. return empty string
                cor_str = ''
            end if
        end if
    end function correct_fmt_str



end module fmt_m
