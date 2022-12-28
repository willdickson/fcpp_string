program main

    use fcpp_string, only : test
    use fcpp_string, only : len
    use fcpp_string, only : string_t
    implicit none
    type(string_t) :: s
    
    print *,'s = ', s,  'len(s) = ', len(s)
    s = 'flash'
    print *,'s = ', s,  'len(s) = ', len(s)



    !call test()


end program main
