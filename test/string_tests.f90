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
            new_unittest('size', test_size)  &
            ]
    end subroutine collect_string_tests


    subroutine test_size(error)
        type(error_type), allocatable, intent(out) :: error

        character(:), allocatable :: chr
        type(string_t)            :: str
        integer                   :: str_size
        integer(c_size_t)         :: str_c_size 

        chr = 'my test string'
        str = string_t(chr)

        str_size = str % size()
        call check(error, str_size == len(chr))

        str_c_size = str % size()
        call check(error, str_c_size == len(chr))

    end subroutine test_size



end module string_tests
