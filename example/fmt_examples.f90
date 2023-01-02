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

        x = 5.0/3.0
        s = 's = ' // string_fmt('{2.4e}', x)
        print *, s

        x = 7.0/3.0
        s = 's = ' // string_fmt('{.{}}', x, 5)
        print *, s

        x = 1.0/7.0
        s = 's = ' // string_fmt('s = {#a}', x) // ' (hex)'
        print *, s

        x = 2.0/7.0
        s = 's = ' // string_fmt('{g}', x) // ' (general)'
        print *, s

        x = 5.0/7.0
        s = 's = ' // string_fmt('[{ ^16}]', x)
        print *, s
        s = 's = ' // string_fmt('[{*^16}]', x)
        print *, s

        x = 1.23
        s = '[-s, +s] = [' // string_fmt('{+}', -x) // ',' // string_fmt('{+}', -x) // ']'
        print *, s
    end block

    ! logicals
    ! -----------------------------------------------------
    block
        type(string_t) :: s
        logical        :: k

        print *, ''
        print *, 'logicals'

        k = .true.
        s = string_fmt('{}', k)
        print *, 's = ', s

        k = .false.
        s = string_fmt('{}', k)
        print *, 's = ', s

        k = .true.
        s =  's = <' // string_fmt('{_^9}', k) // '>'
        print *, s
        k = .false.
        s =  's = <' // string_fmt('{_^9}', k) // '>'
        print *, s

        k = .true.
        s =  's = |' // string_fmt('{ <9}', k) // '|'
        print *, s
        k = .false.
        s =  's = |' // string_fmt('{ <9}', k) // '|'
        print *, s
    end block


    ! strings 
    ! -----------------------------------------------------
    block
        type(string_t) :: s1
        type(string_t) :: s2

        print *, ''
        print *, 'strings'

        s1 = 'hello'
        s2 = string_fmt('{}', s1)
        print *, 's2 = ', s2

        s1 = 'hello'
        s2 = string_fmt('<{}>', s1)
        print *, 's2 = ', s2

        s1 = 'rufus'
        s2 = 's2 = [' // string_fmt('{ ^12}', s1) // ']'
        print *, s2
        s1 = 'bob'
        s2 = 's2 = [' // string_fmt('{ ^12}', s1) // ']'
        print *, s2
    end block

end program fmt_examples
