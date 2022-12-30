module string_tests 

    use, intrinsic :: iso_c_binding, only : c_size_t
    use testdrive,   only : new_unittest 
    use testdrive,   only : unittest_type 
    use testdrive,   only : error_type 
    use testdrive,   only : check
    use fcpp_string, only : string_t
    use fcpp_string, only : char
    use fcpp_string, only : len 

    implicit none
    private

    public :: collect_string_tests

contains

    subroutine collect_string_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [  & 
            new_unittest('test constructor',   test_constructor),  &
            new_unittest('test size',          test_size),         &
            new_unittest('test to_character',  test_to_character), &
            new_unittest('test clear',         test_clear),        &
            new_unittest('test compare',       test_compare),      &
            new_unittest('test empty',         test_empty),        &
            new_unittest('test push_back',     test_push_back),    &
            new_unittest('test pop_back',      test_pop_back),     &
            new_unittest('test append',        test_append),       &
            new_unittest('test erase',         test_erase),        &
            new_unittest('test insert',        test_insert),       &
            new_unittest('test operator(+)',   test_add),          &
            new_unittest('test operator(//)',  test_concat),       &
            new_unittest('test copy',          test_copy),         &
            new_unittest('test assignment(=)', test_assignment),   &
            new_unittest('test find',          test_find),         &
            new_unittest('test rfind',         test_rfind),        &
            new_unittest('test replace',       test_replace),      &
            new_unittest('test operator(==)',  test_equals),       &
            new_unittest('test operator(/=)',  test_not_equals),   &
            new_unittest('test initialized',   test_initialized),  &
            new_unittest('test char',          test_char),         &
            new_unittest('test len',           test_len)           &
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


    subroutine test_copy(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t)  :: str1
        type(string_t)  :: str2

        ! Test uninitialized string
        str2 = str1 % copy()
        call check(error, str2 == string_t(''))
        if (allocated(error)) return

        ! Test initialized string
        str1 = 'the test string'
        str2 = str1 % copy()
        call check(error, str2 == str1)
        if (allocated(error)) return
    end subroutine test_copy


    subroutine test_assignment(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t)            :: str1
        type(string_t)            :: str2
        character(:), allocatable :: chr

        ! Test uninitialized string
        str2 = str1
        call check(error, str2 == string_t(''))
        if (allocated(error)) return

        ! Test initialized string
        str1 = 'the test string'
        str2 = str1
        call check(error, str2 == str1)
        if (allocated(error)) return

        ! Test character
        chr = 'a test string'
        str2 = chr
        call check(error, char(str2) == chr)
        if (allocated(error)) return
    end subroutine test_assignment


    subroutine test_find(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t)    :: str
        integer           :: loc 
        integer(c_size_t) :: loc_c

        ! Test on uninitialized string w/o pos arg
        loc = str % find('this')
        call check(error, loc == 0)
        if (allocated(error)) return

        loc_c = str % find('that')
        call check(error, loc_c == 0)
        if (allocated(error)) return

        ! Test on uninitialized string w/ pos arg
        loc = str % find('this', pos=5)
        call check(error, loc == 0)
        if (allocated(error)) return

        loc = str % find('this', pos=-11)
        call check(error, loc == 0)
        if (allocated(error)) return

        loc_c = str % find('that',pos=33)
        call check(error, loc_c == 0)
        if (allocated(error)) return

        loc_c = str % find('that',pos=-15)
        call check(error, loc_c == 0)
        if (allocated(error)) return

        ! Test on initialized string w/o pos arg
        str = '1234bob89'

        loc = str % find('bob')
        call check(error, loc == 5)
        if (allocated(error)) return

        loc_c = str % find('bob')
        call check(error, loc_c == 5)
        if (allocated(error)) return

        loc = str % find('alan')
        call check(error, loc == 0)
        if (allocated(error)) return

        loc_c = str % find('alan')
        call check(error, loc_c == 0)
        if (allocated(error)) return

        ! Test on initialized string w/ pos arg
        str = '12345alan01234'

        loc = str % find('bob',pos=3)
        call check(error, loc == 0)
        if (allocated(error)) return

        loc = str % find('alan',pos=3)
        call check(error, loc == 6)
        if (allocated(error)) return

        loc = str % find('alan',pos=6)
        call check(error, loc == 6)
        if (allocated(error)) return

        loc = str % find('alan',pos=7)
        call check(error, loc == 0)
        if (allocated(error)) return

        loc = str % find('alan',pos=9)
        call check(error, loc == 0)
        if (allocated(error)) return

        loc = str % find('alan',pos=21)
        call check(error, loc == 0)
        if (allocated(error)) return

        loc = str % find('alan',pos=-5)
        call check(error, loc == 6)
        if (allocated(error)) return
    end subroutine test_find


    subroutine test_rfind(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t)    :: str
        integer           :: loc 
        integer(c_size_t) :: loc_c

        ! Test on uninitialized string w/o pos arg
        loc = str % rfind('this')
        call check(error, loc == 0)
        if (allocated(error)) return

        loc_c = str % rfind('that')
        call check(error, loc_c == 0)
        if (allocated(error)) return

        ! Test on uninitialized string w/ pos arg
        loc = str % rfind('this', pos=5)
        call check(error, loc == 0)
        if (allocated(error)) return

        loc = str % rfind('this', pos=-11)
        call check(error, loc == 0)
        if (allocated(error)) return

        loc_c = str % rfind('that',pos=33)
        call check(error, loc_c == 0)
        if (allocated(error)) return

        loc_c = str % rfind('that',pos=-15)
        call check(error, loc_c == 0)
        if (allocated(error)) return

        ! Test on initialized string w/o pos arg
        str = 'bob4bob89'

        loc = str % rfind('bob')
        call check(error, loc == 5)
        if (allocated(error)) return

        loc_c = str % rfind('bob')
        call check(error, loc_c == 5)
        if (allocated(error)) return

        loc = str % rfind('alan')
        call check(error, loc == 0)
        if (allocated(error)) return

        loc_c = str % rfind('alan')
        call check(error, loc_c == 0)
        if (allocated(error)) return

        ! Test on initialized string w/ pos arg
        str = '12alan789alan45'

        loc = str % rfind('bob',pos=13)
        call check(error, loc == 0)
        if (allocated(error)) return

        loc = str % rfind('alan',pos=14)
        call check(error, loc == 10)
        if (allocated(error)) return

        loc = str % rfind('alan',pos=16)
        call check(error, loc == 10)
        if (allocated(error)) return

        loc = str % rfind('alan',pos=10)
        call check(error, loc == 10)
        if (allocated(error)) return

        loc = str % rfind('alan',pos=8)
        call check(error, loc == 3)
        if (allocated(error)) return

        loc = str % rfind('alan',pos=4)
        call check(error, loc == 3)
        if (allocated(error)) return

        loc = str % rfind('alan',pos=2)
        call check(error, loc == 0)
        if (allocated(error)) return

        loc = str % rfind('alan',pos=-5)
        call check(error, loc == 0)
        if (allocated(error)) return

    end subroutine test_rfind


    subroutine test_replace(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t)    :: str1
        type(string_t)    :: str2
        type(string_t)    :: str3


        ! Check case where all string are uninitialized
        block 
            type(string_t)    :: str1
            type(string_t)    :: str2
            type(string_t)    :: str3
            call str1 % replace(str2, str3)
            call check(error, str1 == string_t(''))
            if (allocated(error)) return
        end block

        ! Check case where str1, str1 uninitialized
        block
            type(string_t)    :: str1
            type(string_t)    :: str2
            type(string_t)    :: str3
            str3 = 'a string'
            call str1 % replace(str2, str3)
            call check(error, str1 == string_t(''))
            if (allocated(error)) return
        end block

        ! Check case where str1, str3  uninitialized
        block
            type(string_t)    :: str1
            type(string_t)    :: str2
            type(string_t)    :: str3
            str2 = 'a string'
            call str1 % replace(str2, str3)
            call check(error, str1 == string_t(''))
            if (allocated(error)) return
        end block
            
        ! Check case where str2, str3  uninitialized
        block
            type(string_t)    :: str1
            type(string_t)    :: str2
            type(string_t)    :: str3
            type(string_t)    :: str4
            str1 = 'a string'
            str4 = str1
            call str1 % replace(str2, str3)
            call check(error, str1 == str4)
            if (allocated(error)) return
        end block

        ! Check case where str1 uninitialized
        block
            type(string_t)    :: str1
            type(string_t)    :: str2
            type(string_t)    :: str3
            str2 = 'this'
            str3 = 'that'
            call str1 % replace(str2, str3)
            call check(error, str1 == string_t(''))
            if (allocated(error)) return
        end block

        ! Check case where str2 uninitialized
        block
            type(string_t)    :: str1
            type(string_t)    :: str2
            type(string_t)    :: str3
            type(string_t)    :: str4
            str1 = 'this'
            str4 = str1
            str3 = 'that'
            call str1 % replace(str2, str3)
            call check(error, str1 == str4)
            if (allocated(error)) return
        end block

        ! Check case where str3 uninitialized
        block
            type(string_t)    :: str1
            type(string_t)    :: str2
            type(string_t)    :: str3
            type(string_t)    :: str4
            str1 = 'this'
            str4 = str1
            str2 = 'that'
            call str1 % replace(str2, str3)
            call check(error, str1 == str4)
            if (allocated(error)) return
        end block

        ! Check case where all initialized, but not found
        block
            type(string_t)    :: str1
            type(string_t)    :: str2
            type(string_t)    :: str3
            type(string_t)    :: str4
            str1 = 'nothing to replace here'
            str4 = str1
            str2 = 'that'
            str3 = 'other'
            call str1 % replace(str2, str3)
            call check(error, str1 == str4)
            if (allocated(error)) return
        end block

        ! Check case where all initialized and we have relacements
        block
            type(string_t)    :: str1
            type(string_t)    :: str2
            type(string_t)    :: str3
            type(string_t)    :: str4

            str1 = 'bob, dave, steve, alan'
            str2 = ','
            str3 = ';'
            str4 = 'bob; dave; steve; alan'
            call str1 % replace(str2, str3)
            call check(error, str1 == str4)
            if (allocated(error)) return

            str1 = 'this is that or this is the other'
            str2 = 'this'
            str3 = 'frogger'
            str4 = 'frogger is that or frogger is the other'
            call str1 % replace(str2, str3)
            call check(error, str1 == str4)
            if (allocated(error)) return

            str1 = 'a string << with << stuff in it << '
            str2 = '<<'
            str3 = '{{'
            str4 = 'a string {{ with {{ stuff in it << '
            call str1 % replace(str2, str3, 2)
            call check(error, str1 == str4)
            if (allocated(error)) return

            str1 = 'a string << with << stuff in it << '
            str2 = '<<'
            str3 = '{{'
            str4 = str1
            call str1 % replace(str2, str3,-3) 
            call check(error, str1 == str4)
            if (allocated(error)) return
        end block

    end subroutine test_replace


    subroutine test_equals(error)
        type(error_type), allocatable, intent(out) :: error

        ! Check case where both unintialized
        block
            type(string_t)    :: str1
            type(string_t)    :: str2
            call check(error, str1==str2)
            if (allocated(error)) return
        end block

        ! Check case where one initialized, one unitialized
        block
            type(string_t)    :: str1
            type(string_t)    :: str2
            str1 = ''
            call check(error, str1==str2)
            if (allocated(error)) return

            call check(error, str2==str1)
            if (allocated(error)) return
        end block

        ! Check case where both are initialized 
        block
            type(string_t)    :: str1
            type(string_t)    :: str2
            str1 = ''
            str2 = ''
            call check(error, str1==str2)
            if (allocated(error)) return

            str1 = 'an example string'
            str2 = 'an example string'
            call check(error, str1==str2)
            if (allocated(error)) return
        end block
    end subroutine test_equals


    subroutine test_not_equals(error)
        type(error_type), allocatable, intent(out) :: error

        ! Check case where both uninitialized
        block
            type(string_t)    :: str1
            type(string_t)    :: str2
            call check(error, .not. (str1/=str2))
            if (allocated(error)) return
        end block

        ! Check case where one initialized, one unitialized
        block
            type(string_t)    :: str1
            type(string_t)    :: str2
            str1 = ''
            call check(error, .not. (str1/=str2))
            if (allocated(error)) return

            call check(error, .not. (str2/=str1))
            if (allocated(error)) return
        end block

        ! Check case where both are initialized 
        block
            type(string_t)    :: str1
            type(string_t)    :: str2
            str1 = ''
            str2 = ''
            call check(error, .not. (str1/=str2))
            if (allocated(error)) return

            str1 = 'the cat is tall'
            str2 = 'all dogs are brown'
            call check(error, str1/=str2)
            if (allocated(error)) return
        end block
    end subroutine test_not_equals


    subroutine test_initialized(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t)    :: str

        call check(error, .not. str % initialized())
        if (allocated(error)) return

        str = string_t()
        call check(error, str % initialized())
        if (allocated(error)) return

        str = 'this is a string' 
        call check(error, str % initialized())
        if (allocated(error)) return
    end subroutine test_initialized


    subroutine test_char(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t)    :: str

        call check(error, char(str) == '')
        if (allocated(error)) return

        str = 'bob is your uncle'
        call check(error, char(str) == 'bob is your uncle')
        if (allocated(error)) return

        call str % clear()
        call check(error, char(str) == '')
        if (allocated(error)) return
    end subroutine test_char


    subroutine test_len(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t)    :: str

        call check(error, len(str) == 0)
        if (allocated(error)) return

        str = string_t()
        call check(error, len(str) == 0)
        if (allocated(error)) return

        str = '12345'
        call check(error, len(str) == 5)
        if (allocated(error)) return
    end subroutine test_len

end module string_tests
