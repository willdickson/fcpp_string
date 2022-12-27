program test_main 
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive,    only : run_testsuite 
    use testdrive,    only : new_testsuite 
    use testdrive,    only : testsuite_type
    use string_tests, only : collect_string_tests
    implicit none

    integer :: i
    integer :: stat
    type(testsuite_type), allocatable :: testsuites(:)

    testsuites = [ &
        new_testsuite('string_tests', collect_string_tests)  &
        ]

    stat = 0

    do i = 1, size(testsuites)
        write(error_unit, *) 'testing: ', testsuites(i) % name
        call run_testsuite(testsuites(i) % collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, *) stat, 'tests failed!'
    end if

end program test_main
