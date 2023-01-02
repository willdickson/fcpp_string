program fmt_examples

    use fcpp_string, only : string_t
    use fcpp_string, only : string_fmt

    implicit none

    ! integers
    ! -----------------------------------------------------
    block
        type(string_t) :: s 
        integer        :: n

        print *, ''
        print *, 'integers'

        n = 12
        s = string_fmt('{d}', n)
        print *, 's = ', s

        n = 25
        s = string_fmt('s = {06d}', n)
        print *, s

        n = 254 
        s = 's = ' // string_fmt('{#x}',n) // ' (hex)'
        print *, s

        n = 51
        s = 's = ' // string_fmt('|{-^10}|', n)
        print *, s
        s = 's = ' // string_fmt('|{-<10}|', n)
        print *, s
        s = 's = ' // string_fmt('|{->10}|', n)
        print *, s
    end block

    ! reals 
    ! -----------------------------------------------------
    block
        type(string_t) :: s
        real           :: x

        print *, ''
        print *, 'reals'

        x = 1.2
        s = string_fmt('{}',x)
        print *, 's = ', s

        x = 1.0/3.0
        s = string_fmt('s = {1.3f}', x)
        print *, s

    end block



end program fmt_examples
