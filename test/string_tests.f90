module string_tests 

    use, intrinsic :: iso_c_binding, only : c_size_t

    use testdrive,   only : new_unittest 
    use testdrive,   only : unittest_type 
    use testdrive,   only : error_type 
    use testdrive,   only : check
    use fcpp_string, only : string_t
    use fcpp_string, only : char

    implicit none
    private

    public :: collect_string_tests

contains

    subroutine collect_string_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [  & 
            new_unittest('test constructor',  test_constructor),  &
            new_unittest('test size',         test_size),         &
            new_unittest('test to_character', test_to_character), &
            new_unittest('test clear',        test_clear),        &
            new_unittest('test compare',      test_compare),      &
            new_unittest('test empty',        test_empty),        &
            new_unittest('test push_back',    test_push_back),    &
            new_unittest('test pop_back',     test_pop_back),     &
            new_unittest('test append',       test_append),       &
            new_unittest('test erase',        test_erase)         &
            ]
    end subroutine collect_string_tests


    subroutine test_constructor(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t)            :: str1
        type(string_t)            :: str2
        type(string_t)            :: str3
        character(:), allocatable :: chr
        logical :: tmp

        ! empty constructor
        str1= string_t()
        call check(error, str1 % size() == 0)

        ! character constructor
        chr = 'first test string'
        str1 = string_t(chr)
        call check(error, char(str1) == chr)
        str2 = chr 
        call check(error, char(str2) == chr)
        
        ! string constructor
        str1 = string_t('second test string')
        str2 = string_t(str1)
        str3 = str1
        call check(error, str1 == str2)
        if (allocated(error)) return
        call check(error, char(str1) == char(str2))
        if (allocated(error)) return
        call check(error, str1 == str3)
        if (allocated(error)) return

        ! pointer constructor
        str1 = string_t('third test string')
        str2 = string_t(str1 % ptr)
        call check(error, str1 == str2)
        if (allocated(error)) return

    end subroutine test_constructor


    subroutine test_size(error)
        type(error_type), allocatable, intent(out) :: error
        character(:), allocatable :: chr
        type(string_t)            :: str
        integer                   :: str_size
        integer(c_size_t)         :: str_c_size 

        call check(error, str % size() == 0)
        if (allocated(error)) return

        chr = 'my test string'
        str = string_t(chr)
        str_size = str % size()
        call check(error, str_size == len(chr))
        if (allocated(error)) return

        str_c_size = str % size()
        call check(error, str_c_size == len(chr))
        if (allocated(error)) return
    end subroutine test_size


    subroutine test_to_character(error)
        type(error_type), allocatable, intent(out) :: error
        character(:), allocatable :: chr
        type(string_t)            :: str
        call check(error, str % to_character()=='')
        if (allocated(error)) return
        chr = 'this is a test string'
        str = string_t(chr)
        call check(error, chr == str % to_character())
        if (allocated(error)) return
    end subroutine test_to_character


    subroutine test_clear(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t) :: str
        call str % clear()
        call check(error, str % size() == 0)
        if (allocated(error)) return
        str = string_t('this is a string')
        call str % clear()
        call check(error, str % size() == 0)
        if (allocated(error)) return
    end subroutine test_clear


    subroutine test_compare(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t) :: str1
        type(string_t) :: str2
        integer        :: comp 

        ! Check for case when neither allocated
        comp = str1 % compare(str2)
        call check(error, comp == 0)
        if (allocated(error)) return

        ! Check for cases when one allocated 
        str1 = 'a test string'
        comp = str1 % compare(str2)
        call check(error, comp > 0)
        if (allocated(error)) return
        comp = str2 % compare(str1)
        call check(error, comp < 0)
        if (allocated(error)) return

        ! Check equals
        str1 = 'a test string'
        str2 = 'a test string'
        comp = str1 % compare(str2)
        call check(error, comp == 0)
        if (allocated(error)) return

        ! Check less than
        str1 = 'aabcde'
        str2 = 'abdcde'
        comp = str1 % compare(str2)
        call check(error, comp < 0)
        if (allocated(error)) return

        ! Check greater than
        str1 = 'abbcde'
        str2 = 'aadcde'
        comp = str1 % compare(str2)
        call check(error, comp > 0)
        if (allocated(error)) return
    end subroutine test_compare


    subroutine test_empty(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t) :: str

        ! Unallocated string
        call check(error, str % empty())
        if (allocated(error)) return

        ! Allocated empty string
        str = string_t()
        call check(error, str % empty())
        if (allocated(error)) return

        ! A nonmpty string
        str = string_t('a test string')
        call check(error, .not. str % empty())
        if (allocated(error)) return
    end subroutine test_empty


    subroutine test_push_back(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t)  :: str
        call str % push_back('a')
        call check(error, str == string_t('a'))
        if (allocated(error)) return

        call str % push_back('bc')
        call check(error, str == string_t('abc'))
        if (allocated(error)) return
    end subroutine test_push_back

    
    subroutine test_pop_back(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t)  :: str

        ! Check when string unallocate
        call str % pop_back()
        call check(error, str == string_t(''))
        if (allocated(error)) return

        ! Pop until emtpy check as we go
        str = 'abc'
        call str % pop_back()
        call check(error, str == string_t('ab'))
        if (allocated(error)) return

        call str % pop_back()
        call check(error, str == string_t('a'))
        if (allocated(error)) return

        call str % pop_back()
        call check(error, str == string_t(''))
        if (allocated(error)) return

        call str % pop_back()
        call check(error, str == string_t(''))
        if (allocated(error)) return
    end subroutine test_pop_back


    subroutine test_append(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t)  :: str

        call str % append('a')
        call check(error, str == string_t('a'))
        if (allocated(error)) return

        call str % append('b')
        call check(error, str == string_t('ab'))
        if (allocated(error)) return

        call str % append('c')
        call check(error, str == string_t('abc'))
        if (allocated(error)) return
    end subroutine test_append


    subroutine test_erase(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t)  :: str

        str = string_t('123456789')
        call str % erase(5,0)
        call check(error, str == string_t('123456789'))
        if (allocated(error)) return

        str = string_t('123456789')
        call str % erase(5,1)
        call check(error, str == string_t('12346789'))
        if (allocated(error)) return

        str = string_t('123456789')
        call str % erase(6,3)
        call check(error, str == string_t('123459'))
        if (allocated(error)) return

        str = string_t('123456789')
        call str % erase(1,5)
        call check(error, str == string_t('6789'))
        if (allocated(error)) return

        str = string_t('123456789')
        call str % erase(1,20)
        call check(error, str == string_t(''))
        if (allocated(error)) return

        str = string_t('123456789')
        call str % erase(-1,2)
        call check(error, str == string_t('3456789'))
        if (allocated(error)) return

        str = string_t('123456789')
        call str % erase(1,-2)
        call check(error, str == string_t('123456789'))
        if (allocated(error)) return

        str = string_t('123456789')
        call str % erase(-1,-2)
        call check(error, str == string_t('123456789'))
        if (allocated(error)) return
    end subroutine test_erase


end module string_tests
