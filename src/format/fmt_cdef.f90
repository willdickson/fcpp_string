module fmt_cdef
    use, intrinsic :: iso_c_binding,  only : c_ptr
    use, intrinsic :: iso_c_binding,  only : c_int
    use, intrinsic :: iso_c_binding,  only : c_float
    use, intrinsic :: iso_c_binding,  only : c_double
    use, intrinsic :: iso_c_binding,  only : c_long_double

    implicit none
    private

    public :: fmt_float_c
    public :: fmt_float_with_prec_c

    interface

        function fmt_float_c(fmt_str, flt_val) bind(c, name='fmt_float') result(res_str)
            import c_ptr
            import c_float
            implicit none
            type(c_ptr),   intent(in), value :: fmt_str
            real(c_float), intent(in), value :: flt_val 
            type(c_ptr)                      :: res_str
        end function fmt_float_c


        function fmt_float_with_prec_c(fmt_str, flt_val, prec) & 
                bind(c, name='fmt_float_with_prec')  result(res_str)
            import c_ptr
            import c_float
            import c_int
            implicit none
            type(c_ptr),    intent(in), value :: fmt_str
            real(c_float),  intent(in), value :: flt_val 
            integer(c_int), intent(in), value :: prec
            type(c_ptr)                       :: res_str
        end function fmt_float_with_prec_c

    end interface

end module fmt_cdef
