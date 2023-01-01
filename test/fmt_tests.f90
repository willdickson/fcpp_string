module fmt_tests 

    use, intrinsic :: iso_fortran_env, only : int32
    use, intrinsic :: iso_fortran_env, only : int64
    use, intrinsic :: iso_fortran_env, only : real32 
    use, intrinsic :: iso_fortran_env, only : real64

    use testdrive,   only : new_unittest 
    use testdrive,   only : unittest_type 
    use testdrive,   only : error_type 
    use testdrive,   only : check
    use fcpp_string, only : string_fmt
    use fcpp_string, only : string_t

    implicit none
    private

    public :: collect_fmt_tests

contains

    subroutine collect_fmt_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [  & 
            new_unittest('test fmt_int32',   test_fmt_int32),   &
            new_unittest('test fmt_int64',   test_fmt_int64),   &
            new_unittest('test fmt_real32',  test_fmt_real32),  &
            new_unittest('test fmt_real64',  test_fmt_real64),  &
            new_unittest('test fmt_logical', test_fmt_logical), &
            new_unittest('test fmt_string',  test_fmt_string)   &
        ]
    end subroutine collect_fmt_tests

    subroutine test_fmt_int32(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t) :: s_nin
        type(string_t) :: s_tst
        type(string_t) :: s_tru

        s_tst = string_fmt(s_nin, 1_int32)
        s_tru = "{format_error: missing '{' in format string}"
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('{05d}', 12_int32)
        s_tru = '00012'
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('|{-^6}|', 22_int32)
        s_tru = '|--22--|'

        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('',10_int32)
        s_tru = "{format_error: missing '{' in format string}"
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('{',10_int32)
        s_tru = "{formar_error: missing '}' in format string}"
        call check(error, s_tst == s_tru)
        if (allocated(error)) return
    end subroutine test_fmt_int32


    subroutine test_fmt_int64(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t) :: s_nin
        type(string_t) :: s_tst
        type(string_t) :: s_tru
        type(string_t) :: tmp

        s_tst = string_fmt(s_nin, 1_int64)
        s_tru = "{format_error: missing '{' in format string}"
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('{05d}', 12_int64)
        s_tru = '00012'
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('|{-^6}|', 22_int64)
        s_tru = '|--22--|'
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('{#x}', 255_int64)
        s_tru = '0xff'
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('',10_int64)
        s_tru = "{format_error: missing '{' in format string}"
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('{::}',10_int64)
        s_tru = "{formar_error: invalid format specifier}"
        call check(error, s_tst == s_tru)
        if (allocated(error)) return
    end subroutine test_fmt_int64


    subroutine test_fmt_real32(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t) :: s_nin
        type(string_t) :: s_tst
        type(string_t) :: s_tru

        s_tst = string_fmt(s_nin, 1.0_real32)
        s_tru = "{format_error: missing '{' in format string}"
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('{1.3f}', 1.0_real32/3.0_real32)
        s_tru = '0.333'
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('|{ ^11.3f}|', 2.0_real32/3.0_real32)
        s_tru = '|   0.667   |'
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('{1.2e}', 10000.0_real32)
        s_tru = '1.00e+04'
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('',1.5_real32)
        s_tru = "{format_error: missing '{' in format string}"
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('{::}',0.1_real32)
        s_tru = "{formar_error: invalid format specifier}"
        call check(error, s_tst == s_tru)
        if (allocated(error)) return
    end subroutine test_fmt_real32


    subroutine test_fmt_real64(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t) :: s_nin
        type(string_t) :: s_tst
        type(string_t) :: s_tru

        s_tst = string_fmt(s_nin, 1.0_real64)
        s_tru = "{format_error: missing '{' in format string}"
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('{1.3f}', 1.0_real64/3.0_real64)
        s_tru = '0.333'
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('|{ ^11.3f}|', 2.0_real64/3.0_real64)
        s_tru = '|   0.667   |'
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('{:1.2e}', 10000.0_real64)
        s_tru = '1.00e+04'
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('',1.5_real64)
        s_tru = "{format_error: missing '{' in format string}"
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('{::}',0.1_real64)
        s_tru = "{formar_error: invalid format specifier}"
        call check(error, s_tst == s_tru)
        if (allocated(error)) return
    end subroutine test_fmt_real64


    subroutine test_fmt_logical(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t) :: s_nin
        type(string_t) :: s_tst
        type(string_t) :: s_tru

        s_tst = string_fmt(s_nin, .true.)
        s_tru = "{format_error: missing '{' in format string}"
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('{}', .true.)
        s_tru = 'true'
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('{}', .false.)
        s_tru = 'false'
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('{-^9}', .false.)
        s_tru = '--false--'
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('', .true.)
        s_tru = "{format_error: missing '{' in format string}"
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('{kalxa}', .false.)
        s_tru = "{formar_error: invalid format specifier}"
        call check(error, s_tst == s_tru)
        if (allocated(error)) return
    end subroutine test_fmt_logical
 

    subroutine test_fmt_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_t) :: s_nin
        type(string_t) :: s_fmt
        type(string_t) :: s_val
        type(string_t) :: s_tst
        type(string_t) :: s_tru

        s_tst = string_fmt(s_nin, s_nin)
        s_tru = "{format_error: missing '{' in format string}"
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_fmt = '{}'
        s_tst = string_fmt(s_fmt, s_nin)
        s_tru = '{}'
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_val = 'hello'
        s_tst = string_fmt(s_nin, s_nin)
        s_tru = "{format_error: missing '{' in format string}"
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_fmt = '[{<10}]'
        s_val = 'bobber'
        s_tst = string_fmt(s_fmt, s_val)
        s_tru = '[bobber    ]'
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_fmt = '({^4})'
        s_tst = string_fmt(s_fmt, 'aa')
        s_tru = '( aa )'
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_val = 'abc'
        s_tst = string_fmt('<{}>',s_val)
        s_tru = '<abc>'
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('', 'rufus')
        s_tru = "{format_error: missing '{' in format string}"
        call check(error, s_tst == s_tru)
        if (allocated(error)) return

        s_tst = string_fmt('{//..}', 'bob')
        s_tru = "{formar_error: invalid format specifier}"
        call check(error, s_tst == s_tru)
        if (allocated(error)) return
    end subroutine test_fmt_string


end module fmt_tests



