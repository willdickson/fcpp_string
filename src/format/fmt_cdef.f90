module fmt_cdef
    use, intrinsic :: iso_c_binding,  only : c_ptr
    use, intrinsic :: iso_c_binding,  only : c_int
    use, intrinsic :: iso_c_binding,  only : c_int32_t
    use, intrinsic :: iso_c_binding,  only : c_int64_t
    use, intrinsic :: iso_c_binding,  only : c_size_t
    use, intrinsic :: iso_c_binding,  only : c_float
    use, intrinsic :: iso_c_binding,  only : c_double

    implicit none
    private

    public :: fmt_int32_c
    public :: fmt_int64_c
    public :: fmt_size_c
    public :: fmt_float_c
    public :: fmt_float_with_prec_c
    public :: fmt_double_c
    public :: fmt_double_with_prec_c

    interface

        function fmt_int32_c(fmt_str, val) bind(c, name='fmt_int32') result(res_str)
            import c_ptr
            import c_int32_t
            implicit none
            type(c_ptr),        intent(in), value :: fmt_str
            integer(c_int32_t), intent(in), value :: val 
            type(c_ptr)                           :: res_str
        end function fmt_int32_c


        function fmt_int64_c(fmt_str, val) bind(c, name='fmt_int64') result(res_str)
            import c_ptr
            import c_int64_t
            implicit none
            type(c_ptr),        intent(in), value :: fmt_str
            integer(c_int64_t), intent(in), value :: val 
            type(c_ptr)                           :: res_str
        end function fmt_int64_c


        function fmt_size_c(fmt_str, val) bind(c, name='fmt_size') result(res_str)
            import c_ptr
            import c_size_t
            implicit none
            type(c_ptr),       intent(in), value :: fmt_str
            integer(c_size_t), intent(in), value :: val 
            type(c_ptr)                          :: res_str
        end function fmt_size_c


        function fmt_float_c(fmt_str, val) bind(c, name='fmt_float') result(res_str)
            import c_ptr
            import c_float
            implicit none
            type(c_ptr),   intent(in), value :: fmt_str
            real(c_float), intent(in), value :: val 
            type(c_ptr)                      :: res_str
        end function fmt_float_c


        function fmt_float_with_prec_c(fmt_str, val, prec) & 
                bind(c, name='fmt_float_with_prec')  result(res_str)
            import c_ptr
            import c_float
            import c_int
            implicit none
            type(c_ptr),    intent(in), value :: fmt_str
            real(c_float),  intent(in), value :: val 
            integer(c_int), intent(in), value :: prec
            type(c_ptr)                       :: res_str
        end function fmt_float_with_prec_c


        function fmt_double_c(fmt_str, val) bind(c, name='fmt_double') result(res_str)
            import c_ptr
            import c_double
            implicit none
            type(c_ptr),    intent(in), value :: fmt_str
            real(c_double), intent(in), value :: val 
            type(c_ptr)                       :: res_str
        end function fmt_double_c


        function fmt_double_with_prec_c(fmt_str, val, prec) & 
                bind(c, name='fmt_double_with_prec')  result(res_str)
            import c_ptr
            import c_double
            import c_int
            implicit none
            type(c_ptr),     intent(in), value :: fmt_str
            real(c_double),  intent(in), value :: val 
            integer(c_int),  intent(in), value :: prec
            type(c_ptr)                        :: res_str
        end function fmt_double_with_prec_c

    end interface

end module fmt_cdef
