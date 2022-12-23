module cpp_string_m

    use, intrinsic :: iso_c_binding,   only : c_ptr
    use, intrinsic :: iso_c_binding,   only : c_char
    use, intrinsic :: iso_c_binding,   only : c_size_t
    use, intrinsic :: iso_c_binding,   only : c_null_char
    use, intrinsic :: iso_c_binding,   only : c_null_ptr
    use, intrinsic :: iso_c_binding,   only : c_associated

    implicit none
    private

    public :: char

    type, public :: string_t
        type(c_ptr) :: ptr = c_null_ptr
    contains
        private
        procedure, public :: size          => string_size
        procedure, public :: to_character  => string_to_character
        procedure, public :: clear         => string_clear

        procedure         :: string_at_size_t
        procedure         :: string_at_integer
        generic,   public :: at            => string_at_size_t, &
                                              string_at_integer

        procedure         :: string_append
        procedure         :: string_append_char
        generic,   public :: append        => string_append, &
                                              string_append_char

        procedure         :: string_add
        procedure         :: string_add_char
        procedure, pass(this) :: char_add_string
        generic,   public :: operator(+)   =>  string_add,      &
                                               string_add_char, &
                                               char_add_string

        procedure         :: string_copy
        procedure         :: string_copy_from_char
        generic,   public :: assignment(=) => string_copy, &
                                              string_copy_from_char


        final             :: string_delete
    end type string_t

    interface char
        procedure :: string_to_char
    end interface


    interface string_t
        procedure :: string_new_empty
        procedure :: string_new_from_char
        procedure :: string_new_from_ptr
        procedure :: string_new_from_string
    end interface string_t


    ! C API 
    ! --------------------------------------------------------------------------
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
            type(c_ptr), intent(in),    value :: ptr1
            type(c_ptr), intent(in),    value :: ptr2  
        end subroutine string_append_c


        subroutine string_append_char_c(ptr, c) & 
                bind(c, name='string_append_char')
            import c_ptr
            import c_char
            implicit none
            type(c_ptr), intent(in), value     :: ptr
            character(kind=c_char), intent(in) :: c(*)
        end subroutine string_append_char_c


    end interface
    ! --------------------------------------------------------------------------

contains


    ! Constructors
    ! --------------------------------------------------------------------------
    function string_new_empty() result(string)
        type(string_t) :: string
        string % ptr = string_new_empty_c()
    end function string_new_empty


    function string_new_from_char(c) result(string)
        character(*), intent(in) :: c
        type(string_t)           :: string
        string % ptr = string_new_from_char_c(c//c_null_char)
    end function string_new_from_char


    function string_new_from_ptr(ptr) result(string)
        type(c_ptr), intent(in), value :: ptr
        type(string_t)                 :: string
        if (c_associated(ptr)) then 
            string % ptr = string_new_from_ptr_c(ptr)
        else
            string % ptr = string_new_empty_c()
        end if
    end function string_new_from_ptr


    function string_new_from_string(string) result(new_string)
        type(string_t), intent(in) :: string
        type(string_t)             :: new_string
        new_string  = string_new_from_ptr(string % ptr)
    end function string_new_from_string


    ! Destructor
    subroutine string_delete(this)
        type(string_t), intent(inout) :: this
        call string_delete_c(this % ptr)
    end subroutine string_delete


    ! Bound procedures
    function string_size(this) result(val)
        class(string_t), intent(in) :: this
        integer(c_size_t)           :: val
        if (c_associated(this % ptr)) then
            val = string_size_c(this % ptr)
        else
            val = 0
        end if
    end function string_size


    function string_at_size_t(this, n) result(val)
        class(string_t),        intent(in) :: this 
        integer(kind=c_size_t), intent(in) :: n
        character(kind=c_char)             :: val
        if (c_associated(this % ptr)) then
            val = string_at_c(this % ptr, n-1)
        else
            val = ''
        end if
    end function string_at_size_t


    function string_at_integer(this, n) result(val)
        class(string_t),        intent(in) :: this 
        integer,                intent(in) :: n
        character(kind=c_char)             :: val
        val = this % at(int(n,kind=c_size_t))
    end function string_at_integer


    function string_to_character(this) result(chrstr)
        class(string_t), intent(in) :: this
        character(:), allocatable   :: chrstr 
        integer(c_size_t)           :: strlen
        integer(c_size_t)           :: i
        if (c_associated(this % ptr)) then
            strlen = this % size()
            allocate(character(len=strlen) :: chrstr) 
            do i = 1, strlen
                chrstr(i:i) = string_at_c(this % ptr, i-1)
            end do
        else
            chrstr = ''
        end if
    end function string_to_character


    subroutine string_clear(this) 
        class(string_t), intent(inout) :: this
        if (c_associated(this % ptr)) then
            call string_clear_c(this % ptr)
        end if
    end subroutine string_clear


    subroutine string_append(this, other)
        class(string_t), intent(inout) :: this
        type(string_t),  intent(in)    :: other
        if (.not. c_associated(this % ptr)) then
            this % ptr = string_new_empty_c()
        end if
        call string_append_c(this % ptr, other % ptr)
    end subroutine string_append


    subroutine string_append_char(this, c)
        class(string_t), intent(inout) :: this
        character(*),    intent(in)    :: c
        if (.not. c_associated(this % ptr)) then
            this % ptr = string_new_empty_c()
        end if
        call string_append_char_c(this % ptr, c//c_null_char)
    end subroutine string_append_char


    function string_add(this, other) result(rval)
        class(string_t), intent(in) :: this
        type(string_t),  intent(in) :: other
        type(string_t)              :: rval
        call rval % append(this)
        call rval % append(other)
    end function string_add


    function string_add_char(this, c) result(rval)
        class(string_t), intent(in) :: this
        character(*),    intent(in)    :: c
        type(string_t)              :: rval
        call rval % append(this)
        call rval % append(c)
    end function string_add_char


    function char_add_string(c,this) result(rval)
        character(*),    intent(in)    :: c
        class(string_t), intent(in) :: this
        type(string_t)              :: rval
        call rval % append(c)
        call rval % append(this)
    end function char_add_string


    subroutine string_copy(this, other)
        class(string_t), intent(inout) :: this
        type(string_t),  intent(in)    :: other
        if (.not. c_associated(other % ptr)) then
            this % ptr = string_new_empty_c()
        else
            this % ptr = string_new_from_ptr_c(other % ptr)
        end if
    end subroutine string_copy


    subroutine string_copy_from_char(this, c)
        class(string_t), intent(inout) :: this
        character(*),    intent(in)    :: c
        this % ptr = string_new_from_char_c(c//c_null_char)
    end subroutine string_copy_from_char


    ! Utility subroutines/functions
    ! ------------------------------------------------------------

    function string_to_char(string) result(chrstr)
        type(string_t), intent(in) :: string
        character(:), allocatable  :: chrstr 
        chrstr = string % to_character()
    end function string_to_char


end module cpp_string_m


