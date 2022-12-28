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
            new_unittest('test erase',        test_erase),        &
            new_unittest('test insert',       test_insert),       &
            new_unittest('test operator(+)',  test_add),          &
            new_unittest('test operator(//)', test_concat)        &
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

        ! Check for case when neither string is initialized 
        comp = str1 % compare(str2)
        call check(error, comp == 0)
        if (allocated(error)) return

        ! Check for cases when one is not initialized 
        str1 = 'a test string'
        comp = str1 % compare(str2)
        call check(error, comp > 0)
        if (allocated(error)) return
        comp = str2 % compare(str1)
        call check(error, comp < 0)
        if (allocated(error)) return

        ! Check case when strings are equal
        str1 = 'a test string'
        str2 = 'a test string'
        comp = str1 % compare(str2)
        call check(error, comp == 0)
        if (allocated(error)) return

        ! Check less than case
        str1 = 'aabcde'
        str2 = 'abdcde'
        comp = str1 % compare(str2)
        call check(error, comp < 0)
        if (allocated(error)) return

        ! Check greater than case
        str1 = 'abbcde'
        str2 = 'aadcde'
        comp = str1 % compare(str2)
        call check(error, comp > 0)
        if (allocated(error)) return
    end subroutine test_compare


    subroutine test_empty(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t) :: str

        ! Check uninitialized string 
        call check(error, str % empty())
        if (allocated(error)) return

        ! Check initialized empty string
        str = string_t()
        call check(error, str % empty())
        if (allocated(error)) return

        ! Check nonempty string
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

        ! Check for uninitialized string 
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


    subroutine test_insert(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t)              :: str
        type(string_t)              :: str_insert
        type(string_t)              :: str_initial
        type(string_t), allocatable :: str_results(:)
        integer                     :: i

        ! Insert into uninitialized string
        call str % insert(1,string_t('abcdef'))
        call check(error, str == string_t('abcdef'))
        if (allocated(error)) return

        ! Insert into empty string
        call str % clear()
        call str % insert(1,'abcd')
        call check(error, str == string_t('abcd'))
        if (allocated(error)) return

        ! Insert at all positions
        str_insert = 'xy'
        str_initial = '1234'
        str_results = [string_t('xy1234'), string_t('1xy234'), & 
            string_t('12xy34'), string_t('123xy4'), string_t('1234xy')]

        do i = 1, size(str_results) 
            call str % clear()
            str = str_initial
            call str % insert(i,str_insert)
            call check(error, str == str_results(i))
            if (allocated(error)) return
        end do
    end subroutine test_insert


    subroutine test_add(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t)            :: str1
        type(string_t)            :: str2
        type(string_t)            :: str3

        ! Test adding uninitialized strings
        str3 = str1 + str2
        call check(error, str3 == string_t(''))
        if (allocated(error)) return
        call check(error, str3 % initialized())
        if (allocated(error)) return

        ! Test adding where one is uninitialized
        str1 = 'bob'
        str3 = str1 + str2
        call check(error, str3 == string_t('bob'))
        if (allocated(error)) return

        ! Test adding where both initialized
        str1 = 'this'
        str2 = 'that'
        str3 = str1 + str2 
        call check(error, str3 == string_t('thisthat'))
        if (allocated(error)) return

        ! Test adding characters
        str1 = 'one'
        str3 = str1 + ',' + 'two' + ',' + 'three'
        call check(error, str3 == string_t('one,two,three'))
        if (allocated(error)) return
        str1 = 'one,'
        str3 = str1 + 'two,' + 'three'
        call check(error, str3 == string_t('one,two,three'))
        if (allocated(error)) return
    end subroutine test_add


    subroutine test_concat(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t)            :: str1
        type(string_t)            :: str2
        type(string_t)            :: str3

        ! Test for  uninitialized strings
        str3 = str1//str2
        call check(error, str3 == string_t(''))
        if (allocated(error)) return
        call check(error, str3 % initialized())
        if (allocated(error)) return

        ! Test where one is uninitialized
        str1 = 'bob'
        str3 = str1//str2
        call check(error, str3 == string_t('bob'))
        if (allocated(error)) return

        ! Test where both initialized
        str1 = 'this'
        str2 = 'that'
        str3 = str1//str2 
        call check(error, str3 == string_t('thisthat'))
        if (allocated(error)) return

        !Test with mixed with characters and strings
        str1 = 'rufus,'
        str2 = 'charlie,'
        str3 = 'bob,'//str1//str2//'flash'
        call check(error, str3 == string_t('bob,rufus,charlie,flash'))
        if (allocated(error)) return
    end subroutine test_concat

end module string_tests
