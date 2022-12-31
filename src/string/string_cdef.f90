module string_cdef

    use, intrinsic :: iso_c_binding,  only : c_ptr
    use, intrinsic :: iso_c_binding,  only : c_int
    use, intrinsic :: iso_c_binding,  only : c_char
    use, intrinsic :: iso_c_binding,  only : c_bool
    use, intrinsic :: iso_c_binding,  only : c_size_t

    implicit none
    private

    public :: string_new_empty_c
    public :: string_new_from_char_c
    public :: string_new_from_ptr_c
    public :: string_delete_c
    public :: string_size_c
    public :: string_at_c
    public :: string_set_c
    public :: string_clear_c
    public :: string_append_c
    public :: string_append_from_char_c
    public :: string_push_back_c
    public :: string_pop_back_c
    public :: string_compare_c
    public :: string_empty_c
    public :: string_erase_c
    public :: string_insert_c
    public :: string_find_c
    public :: string_find_from_char_c
    public :: string_rfind_c
    public :: string_rfind_from_char_c

    interface

        function string_new_empty_c() & 
                bind(c, name='string_new_empty') result(ptr) 
            import c_ptr
            implicit none
            type(c_ptr) :: ptr
        end function string_new_empty_c


        function string_new_from_char_c(c) & 
                bind(c, name='string_new_from_char') result(ptr)
            import c_ptr
            import c_char
            implicit none
            character(kind=c_char), intent(in) :: c(*)
            type(c_ptr)                        :: ptr
        end function string_new_from_char_c


        function string_new_from_ptr_c(ptr) & 
                bind(c,name='string_new_from_ptr') result(new_ptr)
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ptr
            type(c_ptr)                    :: new_ptr
        end function string_new_from_ptr_c


        subroutine string_delete_c(ptr) & 
                bind(c, name='string_delete')
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ptr
        end subroutine string_delete_c


        function string_size_c(ptr) & 
                bind(c, name='string_size') result(val)
            import c_ptr
            import c_size_t
            implicit none
            type(c_ptr), intent(in), value :: ptr
            integer(c_size_t)              :: val
        end function string_size_c


        function string_at_c(ptr, n) & 
                bind(c, name='string_at') result(val)
            import c_ptr
            import c_char
            import c_size_t
            implicit none
            type(c_ptr), intent(in), value             :: ptr
            integer(kind=c_size_t), intent(in), value  :: n
            character(kind=c_char)                     :: val
        end function string_at_c


        subroutine string_set_c(ptr, pos, c) &
                bind(c, name='string_set')
            import c_ptr
            import c_char
            import c_size_t
            implicit none
            type(c_ptr), intent(in), value       :: ptr
            integer(c_size_t), intent(in), value :: pos
            character(kind=c_char), intent(in)   :: c(*)
        end subroutine string_set_c


        subroutine string_clear_c(ptr) & 
                bind(c, name='string_clear') 
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ptr
        end subroutine string_clear_c


        subroutine string_append_c(ptr1, ptr2) & 
                bind(c, name='string_append')
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ptr1
            type(c_ptr), intent(in), value :: ptr2  
        end subroutine string_append_c


        subroutine string_append_from_char_c(ptr, c) & 
                bind(c, name='string_append_from_char')
            import c_ptr
            import c_char
            implicit none
            type(c_ptr), intent(in), value     :: ptr
            character(kind=c_char), intent(in) :: c(*)
        end subroutine string_append_from_char_c


        subroutine string_push_back_c(ptr, c) &
                bind(c, name='string_push_back')
            import c_ptr
            import c_char
            implicit none
            type(c_ptr), intent(in), value      :: ptr
            character(kind=c_char), intent(in)  :: c(*)
        end subroutine string_push_back_c


        subroutine string_pop_back_c(ptr) &
                bind(c, name='string_pop_back')
            import c_ptr
            implicit none
            type(c_ptr), intent(in), value :: ptr
        end subroutine string_pop_back_c


        function string_compare_c(ptr1, ptr2) &
                bind(c, name='string_compare') result(val)
            import c_ptr
            import c_int
            implicit none
            type(c_ptr), intent(in), value :: ptr1
            type(c_ptr), intent(in), value :: ptr2
            integer(c_int)                 :: val
        end function string_compare_c


        function string_empty_c(ptr) &
                bind(c, name='string_empty') result(val)
            import c_ptr
            import c_bool
            implicit none
            type(c_ptr), intent(in), value :: ptr
            logical(c_bool)                :: val
        end function


        subroutine string_erase_c(ptr, pos, len) &
                bind(c, name='string_erase')
            import c_ptr
            import c_size_t
            implicit none
            type(c_ptr), intent(in), value        :: ptr
            integer(c_size_t), intent(in), value  :: pos
            integer(c_size_t), intent(in), value  :: len
        end subroutine string_erase_c


        subroutine string_insert_c(ptr1, pos, ptr2) &
                bind(c, name='string_insert')
            import c_ptr
            import c_size_t
            implicit none
            type(c_ptr), intent(in), value       :: ptr1
            integer(c_size_t), intent(in), value :: pos
            type(c_ptr), intent(in), value       :: ptr2
        end subroutine string_insert_c


        function string_find_c(ptr1, ptr2, pos) &
                bind(c, name='string_find') result(val)
            import c_ptr
            import c_size_t
            implicit none
            type(c_ptr), intent(in), value       :: ptr1
            type(c_ptr), intent(in), value       :: ptr2
            integer(c_size_t), intent(in), value :: pos
            integer(c_size_t)                    :: val
        end function string_find_c


        function string_find_from_char_c(ptr, c, pos) &
                bind(c, name='string_find_from_char') result(val)
            import c_ptr
            import c_char
            import c_size_t
            implicit none
            type(c_ptr), intent(in), value       :: ptr
            character(kind=c_char), intent(in)   :: c(*)
            integer(c_size_t), intent(in), value :: pos
            integer(c_size_t)                    :: val
        end function string_find_from_char_c


        function string_rfind_c(ptr1, ptr2, pos) &
                bind(c, name='string_rfind') result(val)
            import c_ptr
            import c_size_t
            implicit none
            type(c_ptr), intent(in), value       :: ptr1
            type(c_ptr), intent(in), value       :: ptr2
            integer(c_size_t), intent(in), value :: pos
            integer(c_size_t)                    :: val
        end function string_rfind_c


        function string_rfind_from_char_c(ptr, c, pos) &
                bind(c, name='string_rfind_from_char') result(val)
            import c_ptr
            import c_char
            import c_size_t
            implicit none
            type(c_ptr), intent(in), value       :: ptr
            character(kind=c_char), intent(in)   :: c(*)
            integer(c_size_t), intent(in), value :: pos
            integer(c_size_t)                    :: val
        end function string_rfind_from_char_c


    end interface


end module string_cdef
