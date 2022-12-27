module cpp_string_m

    use, intrinsic :: iso_c_binding,  only : c_ptr
    use, intrinsic :: iso_c_binding,  only : c_int
    use, intrinsic :: iso_c_binding,  only : c_char
    use, intrinsic :: iso_c_binding,  only : c_bool
    use, intrinsic :: iso_c_binding,  only : c_size_t
    use, intrinsic :: iso_c_binding,  only : c_null_char
    use, intrinsic :: iso_c_binding,  only : c_null_ptr
    use, intrinsic :: iso_c_binding,  only : c_associated

    use string_cdef, only : string_new_empty_c
    use string_cdef, only : string_new_from_char_c
    use string_cdef, only : string_new_from_ptr_c
    use string_cdef, only : string_delete_c
    use string_cdef, only : string_size_c
    use string_cdef, only : string_at_c
    use string_cdef, only : string_clear_c
    use string_cdef, only : string_append_c
    use string_cdef, only : string_append_from_char_c
    use string_cdef, only : string_push_back_c
    use string_cdef, only : string_pop_back_c
    use string_cdef, only : string_compare_c
    use string_cdef, only : string_empty_c
    use string_cdef, only : string_erase_c
    use string_cdef, only : string_insert_c
    use string_cdef, only : string_find_c
    use string_cdef, only : string_find_from_char_c
    use string_cdef, only : string_rfind_c
    use string_cdef, only : string_rfind_from_char_c

    implicit none
    private

    public :: char

    type, public :: string_t
        type(c_ptr) :: ptr = c_null_ptr
    contains
        private
        procedure, public :: size          => string_size, &
                                              string_size_int
        procedure, public :: to_character  => string_to_character
        procedure, public :: clear         => string_clear
        procedure, public :: compare       => string_compare
        procedure, public :: empty         => string_empty
        procedure, public :: push_back     => string_push_back
        procedure, public :: pop_back      => string_pop_back
        procedure         :: string_append
        procedure         :: string_append_from_char
        generic,   public :: append        => string_append, &
                                              string_append_from_char
        procedure         :: string_erase
        procedure         :: string_erase_at_int1
        procedure         :: string_erase_at_int2
        procedure         :: string_erase_at_int12
        generic,   public :: erase         => string_erase, &
                                              string_erase_at_int1, &
                                              string_erase_at_int2, &
                                              string_erase_at_int12
        procedure         :: string_at_size_t
        procedure         :: string_at_integer
        generic,   public :: at            => string_at_size_t, &
                                              string_at_integer
        procedure         :: string_insert
        procedure         :: string_insert_at_int
        generic,   public :: insert        => string_insert, &
                                              string_insert_at_int

        procedure         :: string_add
        procedure         :: string_add_char
        procedure, pass(this) :: char_add_string
        generic,   public :: operator(+)   =>  string_add, &
                                               string_add_char, &
                                               char_add_string
        procedure         :: string_copy
        procedure         :: string_copy_from_char

        procedure         :: string_find
        procedure         :: string_find_at_int
        procedure         :: string_find_from_char
        procedure         :: string_find_from_char_at_int
        generic,   public :: find          => string_find, &     
                                              string_find_at_int, &
                                              string_find_from_char, &
                                              string_find_from_char_at_int
        procedure         :: string_rfind
        procedure         :: string_rfind_at_int
        procedure         :: string_rfind_from_char
        procedure         :: string_rfind_from_char_at_int
        generic,   public :: rfind         => string_rfind, &
                                              string_rfind_at_int, &
                                              string_rfind_from_char, &
                                              string_rfind_from_char_at_int
        procedure         :: string_replace
        procedure         :: string_replace_from_char
        generic,   public :: replace       => string_replace, &
                                              string_replace_from_char
        generic,   public :: assignment(=) => string_copy, &
                                              string_copy_from_char
        procedure         :: string_equals
        generic,   public :: operator(==)  => string_equals
        procedure         :: string_not_equals
        generic,   public :: operator(/=)  => string_not_equals

        procedure         :: string_write_formatted
        generic,   public :: write(formatted)  => string_write_formatted

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
    ! --------------------------------------------------------------------------
    subroutine string_delete(this)
        type(string_t), intent(inout) :: this
        call string_delete_c(this % ptr)
    end subroutine string_delete


    ! Bound procedures
    ! --------------------------------------------------------------------------
    function string_size(this) result(val)
        class(string_t), intent(in) :: this
        integer(c_size_t)           :: val
        if (c_associated(this % ptr)) then
            val = string_size_c(this % ptr)
        else
            val = 0
        end if
    end function string_size


    function string_size_int(this) result(val)
        class(string_t), intent(in) :: this
        integer                     :: val
        if (c_associated(this % ptr)) then
            val = int(string_size_c(this % ptr),kind=kind(val))
        else
            val = 0
        end if
    end function string_size_int


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


    function string_compare(this, that) result(rval)
        class(string_t), intent(in) :: this
        type(string_t),  intent(in) :: that 
        integer(c_int)              :: rval
        logical                     :: ok_this
        logical                     :: ok_that
        type(string_t)              :: tmps 
        ok_this = c_associated(this % ptr)
        ok_that = c_associated(that % ptr)
        if ((.not. ok_this) .and. (.not. ok_that)) then
            rval = 0
        else
            if (.not. ok_this) then
                tmps = string_t()
                rval = string_compare_c(tmps % ptr, that % ptr)
            else if (.not. ok_that) then
                tmps = string_t()
                rval = string_compare_c(this % ptr, tmps % ptr)
            else 
                rval = string_compare_c(this % ptr, that % ptr)
            end if
        end if
    end function string_compare


    subroutine string_append(this, other)
        class(string_t), intent(inout) :: this
        type(string_t),  intent(in)    :: other
        if (.not. c_associated(this % ptr)) then
            this % ptr = string_new_empty_c()
        end if
        call string_append_c(this % ptr, other % ptr)
    end subroutine string_append


    subroutine string_append_from_char(this, c)
        class(string_t), intent(inout) :: this
        character(*),    intent(in)    :: c
        if (.not. c_associated(this % ptr)) then
            this % ptr = string_new_empty_c()
        end if
        call string_append_from_char_c(this % ptr, c//c_null_char)
    end subroutine string_append_from_char


    subroutine string_push_back(this, c)
        class(string_t), intent(inout)   :: this
        character(*),    intent(in)      :: c
        integer                          :: i
        if (.not. c_associated(this % ptr)) then
            this % ptr = string_new_empty_c()
        end if
        do i = 1, len(c)
            call string_push_back_c(this % ptr, c(i:i)//c_null_char)
        end do
    end subroutine string_push_back


    subroutine string_pop_back(this)
        class(string_t), intent(inout)   :: this
        if (.not. c_associated(this % ptr)) then
            this % ptr = string_new_empty_c()
        end if
        call string_pop_back_c(this % ptr)
    end subroutine string_pop_back


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


    function string_equals(this, that) result(rval)
        class(string_t), intent(in) :: this
        type(string_t),  intent(in) :: that
        logical                     :: rval
        integer(c_int)              :: comp
        if (this % compare(that) == 0) then
            rval = .true.
        else
            rval = .false.
        end if
    end function string_equals


    function string_not_equals(this, that) result(rval)
        class(string_t), intent(in) :: this
        type(string_t),  intent(in) :: that
        logical                     :: rval
        integer(c_int)              :: comp
        if (this % compare(that) == 0) then
            rval = .false.
        else
            rval = .true.
        end if
    end function string_not_equals


    function string_empty(this) result(rval)
        class(string_t), intent(in) :: this
        logical                     :: rval
        if (.not. c_associated(this % ptr)) then
            rval = .true.
        else
            rval = string_empty_c(this % ptr)
        end if
    end function string_empty


    subroutine string_erase(this, pos, len)
        class(string_t), intent(inout)  :: this
        integer(c_size_t), intent(in)   :: pos
        integer(c_size_t), intent(in)   :: len
        if (c_associated(this % ptr)) then
            call string_erase_c(this % ptr, pos-1, len)
        end if
    end subroutine string_erase


    subroutine string_erase_at_int1(this, pos, len)
        class(string_t), intent(inout)  :: this
        integer, intent(in), optional   :: pos
        integer(c_size_t), intent(in)   :: len
        integer(c_size_t)               :: pos_
        if (present(pos)) then
            pos_ = int(pos, kind=c_size_t)
        else
            pos_ = 1_c_size_t
        end if

        if (c_associated(this % ptr)) then
            call string_erase_c(this % ptr, pos_-1, len)
        end if
    end subroutine string_erase_at_int1


    subroutine string_erase_at_int2(this, pos, len)
        class(string_t), intent(inout)  :: this
        integer(c_size_t), intent(in)   :: pos
        integer, intent(in), optional   :: len
        integer(c_size_t)               :: len_
        if (present(len)) then
            len_ = int(len, kind=c_size_t)
        else
            len_ = this % size()
        end if

        if (c_associated(this % ptr)) then
            call string_erase_c(this % ptr, pos-1, len_)
        end if
    end subroutine string_erase_at_int2


    subroutine string_erase_at_int12(this, pos, len)
        class(string_t), intent(inout) :: this
        integer, intent(in), optional  :: pos
        integer, intent(in), optional  :: len
        integer(c_size_t)              :: pos_
        integer(c_size_t)              :: len_
        if (c_associated(this % ptr)) then
            if (present(pos)) then
                pos_ = int(pos, kind=c_size_t)
            else
                pos_ = 1_c_size_t
            end if
            if (present(len)) then
                len_ = int(len, kind=c_size_t)
            else
                len_ = this % size()
            end if
            call string_erase_c(this % ptr, pos_-1, len_)
        end if
    end subroutine string_erase_at_int12


    subroutine string_insert(this, pos, str)
        class(string_t), intent(inout) :: this
        integer(c_size_t), intent(in)  :: pos
        type(string_t), intent(in)     :: str 
        integer(c_size_t)              :: pos_c
        if (.not. c_associated(this % ptr)) then
            this % ptr = string_new_empty_c()
        end if
        if (c_associated(str % ptr)) then 
            pos_c = max(pos-1, 0)
            call string_insert_c(this % ptr, pos_c, str % ptr)
        end if
    end subroutine string_insert


    subroutine string_insert_at_int(this, pos, str)
        class(string_t), intent(inout) :: this
        integer,         intent(in)    :: pos
        type(string_t),  intent(in)    :: str 
        integer(c_size_t)              :: pos_
        pos_ = int(pos, kind=c_size_t)
        call this % string_insert(pos_, str)
    end subroutine string_insert_at_int


    function string_find(this, str, pos) result(rval)
        class(string_t),   intent(in)  :: this
        type(string_t),    intent(in)  :: str
        integer(c_size_t), intent(in)  :: pos
        integer(c_size_t)              :: rval
        integer(c_size_t)              :: pos_c
        if (.not. c_associated(this % ptr)) then
            rval = 0 
        else
            pos_c = max(pos-1, 0)
            rval = string_find_c(this % ptr, str % ptr, pos_c) + 1
        end if
    end function string_find


    function string_find_at_int(this, str, pos) result(rval)
        class(string_t),   intent(in)  :: this
        type(string_t),    intent(in)  :: str
        integer, optional, intent(in)  :: pos
        integer(c_size_t)              :: rval
        integer(c_size_t)              :: pos_
        if (present(pos)) then
            pos_ = int(pos, kind=c_size_t)
        else
            pos_ = 1_c_size_t
        end if
        rval = this % string_find(str, pos_)
    end function string_find_at_int


    function string_find_from_char(this, c, pos) result(rval)
        class(string_t),   intent(in) :: this
        character(*),      intent(in) :: c
        integer(c_size_t), intent(in) :: pos
        integer(c_size_t)             :: rval
        integer(c_size_t)             :: pos_c
        if (.not. c_associated(this % ptr)) then
            rval = 0 
        else
            pos_c = max(pos-1,0)
            rval = string_find_from_char_c(this % ptr, c//c_null_char, pos_c)
            rval = rval + 1
        end if
    end function string_find_from_char


    function string_find_from_char_at_int(this, c, pos) result(rval)
        class(string_t),   intent(in) :: this
        character(*),      intent(in) :: c
        integer, optional, intent(in) :: pos
        integer(c_size_t)             :: rval
        integer(c_size_t)             :: pos_
        if (present(pos)) then
            pos_ = int(pos, kind=c_size_t)
        else
            pos_ = 1_c_size_t
        end if
        rval = this % string_find_from_char(c, pos_)
    end function string_find_from_char_at_int


    function string_rfind(this, str, pos) result(rval)
        class(string_t),   intent(in)  :: this
        type(string_t),    intent(in)  :: str
        integer(c_size_t), intent(in)  :: pos
        integer(c_size_t)              :: rval
        integer(c_size_t)              :: pos_c
        if (.not. c_associated(this % ptr)) then
            rval = 0 
        else
            pos_c = max(pos-1, 0)
            rval = string_rfind_c(this % ptr, str % ptr, pos_c)
            rval = rval + 1
        end if
    end function string_rfind


    function string_rfind_at_int(this, str, pos) result(rval)
        class(string_t),   intent(in)  :: this
        type(string_t),    intent(in)  :: str
        integer, optional, intent(in)  :: pos
        integer(c_size_t)              :: rval
        integer(c_size_t)              :: pos_
        if (.not. c_associated(this % ptr)) then
            rval = 0
        else
            if (present(pos)) then
                pos_ = int(pos, kind=c_size_t)
            else
                pos_ = this % size()
            end if
            rval = this % string_rfind(str, pos_)
        end if
    end function string_rfind_at_int


    function string_rfind_from_char(this, c, pos) result(rval)
        class(string_t),   intent(in) :: this
        character(*),      intent(in) :: c
        integer(c_size_t), intent(in) :: pos
        integer(c_size_t)             :: rval
        integer(c_size_t)             :: pos_c
        if (.not. c_associated(this % ptr)) then
            rval = 0 
        else
            pos_c = max(pos-1,0)
            rval = string_rfind_from_char_c(this % ptr, c//c_null_char, pos_c)
            rval = rval + 1
        end if
    end function string_rfind_from_char


    function string_rfind_from_char_at_int(this, c, pos) result(rval)
        class(string_t),   intent(in) :: this
        character(*),      intent(in) :: c
        integer, optional, intent(in) :: pos
        integer(c_size_t)             :: rval
        integer(c_size_t)             :: pos_
        if (.not. c_associated(this % ptr)) then
            rval = 0
        else
            if (present(pos)) then
                pos_ = int(pos, kind=c_size_t)
            else
                pos_ = this % size() 
            end if
            rval = this % string_rfind_from_char(c, pos_)
        end if
    end function string_rfind_from_char_at_int


    subroutine string_replace(this, old, new, cnt)
        class(string_t),   intent(inout) :: this
        type(string_t),    intent(in)    :: old
        type(string_t),    intent(in)    :: new
        integer, optional, intent(in)    :: cnt

        logical                          :: ok
        integer(c_size_t)                :: old_size
        integer(c_size_t)                :: new_size
        integer(c_size_t)                :: pos
        integer                          :: cnt_  

        if (present(cnt)) then
            cnt_ = cnt
        else
            cnt_ = this % size()
        end if

        ok = c_associated(this % ptr) .and. &
             c_associated( old % ptr) .and. &
             c_associated( new % ptr)

        if (ok) then
            old_size = old % size()
            new_size = new % size()
            pos = 1
            do while (.true.) 
                pos = this % find(old, pos)
                if (pos == 0) then
                    exit
                end if
                call this % erase(pos, old_size)
                call this % insert(pos, new)
                pos = pos + new_size
                cnt_ = cnt_ - 1
                if (cnt_ == 0) then
                    exit
                end if
            end do
        end if

    end subroutine string_replace


    subroutine string_replace_from_char(this, old, new, cnt)
        class(string_t),   intent(inout) :: this
        character(*),      intent(in)    :: old
        character(*),      intent(in)    :: new
        integer, optional, intent(in)    :: cnt
        if (present(cnt)) then
            call this % string_replace(string_t(old), string_t(new), cnt)
        else
            call this % string_replace(string_t(old), string_t(new))
        end if
    end subroutine string_replace_from_char


    subroutine string_write_formatted(this, unit, iotype, v_list, iostat, iomsg)
        class(string_t),  intent(in)    :: this
        integer,          intent(in)    :: unit
        character(len=*), intent(in)    :: iotype
        integer,          intent(in)    :: v_list(:)
        integer,          intent(out)   :: iostat
        character(len=*), intent(inout) :: iomsg
        select case (iotype)
        case ('LISTDIRECTED', 'DT')
            write(unit,'(a)',iostat=iostat, iomsg=iomsg), char(this)
        case default
            iostat = 1
            iomsg = 'unsupported iotype '//iotype//' for string_t'
        end select
    end subroutine string_write_formatted


    ! Utility subroutines/functions
    ! --------------------------------------------------------------------------

    function string_to_char(string) result(chrstr)
        type(string_t), intent(in) :: string
        character(:), allocatable  :: chrstr 
        chrstr = string % to_character()
    end function string_to_char


end module cpp_string_m


