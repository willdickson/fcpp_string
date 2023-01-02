program string_examples

    use fcpp_string, only : string_t
    use fcpp_string, only : char
    use fcpp_string, only : len

    implicit none

    ! Constructors 
    ! ------------------------------------------------------------------
    block
        type(string_t) :: s

        print *, ''
        print *, 'create empty string using string_t' 
        s = string_t()  
        print *, 's = ', s
        print *, ''

        print *, 'create empty string from character' 
        s = ''
        print *, 's = ', s
        print *, ''

        print *, 'create a string using string_t' 
        s = string_t('the string')
        print *, 's = ', s
        print *, ''

        print *, 'create a string from characters' 
        s = 'a different string'
        print *, 's = ', s
        print *, ''
    end block

    ! The initialized method
    ! ---------------------------------------------------------------
    block
        type(string_t) :: s

        print *, 'test if string is intialized'
        print  *, 's % initialized() = ', s % initialized()
        s = ''
        print  *, 's % initialized() = ', s % initialized()
        print *, ''
    end block

    ! assignment(=)
    ! ---------------------------------------------------------------
    block
        type(string_t)            :: s1
        type(string_t)            :: s2
        type(string_t)            :: s
        character(:), allocatable :: c

        print *, 'copying string using assignment(=)'
        s1 = string_t('this')
        s2 = s1
        print *, 's1 = ', s1
        print *, 's2 = ', s2
        print *, ''

        print *, 'copying characters using assignment(=)'
        c = 'all the kings men'
        s = c
        print *, 'c = ', c
        print *, 's =', s
        print *, ''
    end block

    ! to_character
    ! ---------------------------------------------------------------
    block
        type(string_t)            :: s
        character(:), allocatable :: c

        print *, 'convert to character using to_character'
        s = 'hello'
        print *, 's =', s
        c = s % to_character()
        print *, 'c = ', c
        print *, ''
    end block

    ! char
    ! ---------------------------------------------------------------
    block
        type(string_t)            :: s
        character(:), allocatable :: c

        print *, 'convert to character using char function'
        s = 'hello'
        print *, 's = ', s
        c = char(s) 
        print *, 'c =  ', c
        print *, ''
    end block

    ! size 
    ! ---------------------------------------------------------------
    block
        type(string_t) :: s
        integer        :: n

        print *, 'get size of string with size'
        s = '123456'
        n = s % size()
        print *, 's = ', s
        print *, 's % size() = ', n
        print *, ''
    end block

    ! len (same value as size)
    ! ---------------------------------------------------------------
    block
        type(string_t) :: s
        integer        :: n

        print *, 'get len with len function (same result as size)'
        s = '12345678'
        n = len(s)
        print *, 's = ', s
        print *, 'len(s) = ', n
        print *, ''
    end block

    ! clear 
    ! ---------------------------------------------------------------
    block
        type(string_t) :: s

        print *, 'emptying string using clear,  call s % clear()'
        call s % clear()
        print *, 's = ', s
        print *, ''
    end block

    ! compare
    ! ---------------------------------------------------------------
    block
        type(string_t) :: s1
        type(string_t) :: s2

        print *, 'compare two strings using compare'
        s1 = 'abc'
        s2 = 'abc'
        print *, 's1 = ', s1  
        print *, 's2 = ', s2  
        print *, 's1 % compare(s2) = ', s1 % compare(s2)
        print *, ''
        s1 = 'abc'
        s2 = 'aac'
        print *, 's1 = ', s1  
        print *, 's2 = ', s2  
        print *, 's1 % compare(s2) = ', s1 % compare(s2)
        print *, ''
        s1 = 'abc'
        s2 = 'bbc'
        print *, 's1 = ', s1  
        print *, 's2 = ', s2  
        print *, 's1 % compare(s2) = ', s1 % compare(s2)
        print *, '' 
    end block
    
    !  empty 
    ! ---------------------------------------------------------------
    block
        type(string_t) :: s

        print *, 'check if string is empty using the empty method'
        s = ''
        print *, 's = ', s
        print *, 's % empty() = ', s % empty()
        print *, ''
        s = 'january'
        print *, 's = ', s
        print *, 's % empty() = ', s % empty()
        print *, ''
    end block

    ! push_back
    ! ---------------------------------------------------------------
    block
        type(string_t) :: s

        print *, 'adding characters with push_back'
        s = ''
        print *, 's = ', s
        call s % push_back('a')
        print *, 's = ', s
        call s % push_back('b')
        print *, 's = ', s
        print *, ''
    end block

    ! pop_back 
    ! ---------------------------------------------------------------
    block
        type(string_t) :: s

        print *, 'removing characters with pop_back'
        s = 'abc'
        print *, 's = ', s
        call s % pop_back()
        print *, 's = ', s
        call s % pop_back()
        print *, 's = ', s
    end block


    ! append 
    ! ---------------------------------------------------------------
    block
        type(string_t) :: s1
        type(string_t) :: s2

        print *, 'append string to string'
        s1 = 'bob '
        print *, 's1 = ', s1
        s2 = 'and rufus'
        print *, 's2 = ', s2
        call s1 % append(s2)
        print *, 's1 = ', s1
        print *, ''

        print *, 'append characters to string'
        s1 = 'this '
        print *, 's1 = ', s1
        call s1 % append('and that')
        print *, 's1 = ', s1
        print *, ''
    end block

    ! erase
    ! ---------------------------------------------------------------
    block
        type(string_t) :: s

        print *, 'removing characters with erase'
        s = '123456789'
        print *, 's = ', s
        call s % erase(2,3)
        print *, 's = ', s
        print *, ''
    end block

    ! at
    ! ---------------------------------------------------------------
    block
        type(string_t) :: s
        character      :: c

        print *, 'get character at position using at'
        s = 'abcdefghi'
        print *, 's = ', s
        c = s % at(5)
        print *, 's % at(5) = ', c
        print *, ''
    end block

    ! set
    ! ---------------------------------------------------------------
    block
        type(string_t) :: s

        print *, 'set character at position using set'
        s = '123456789'
        print *, 's = ', s
        call s % set(4,'a')
        print *, 's = ', s
        print *, ''
    end block

    ! insert 
    ! ---------------------------------------------------------------
    block
        type(string_t) :: s1
        type(string_t) :: s2

        print *, 'insert a string into a string using insert'
        s1 = 'one  three'
        s2 = 'two'  
        print *, 's1 = ', s1
        print *, 's2 = ', s2
        call s1 % insert(5,s2)
        print *, 's1 = ', s1
        print *, ''

        print *, 'insert characters into a string using insert'
        s1 = 'apples  oranges'
        print *, 's1 = ', s1
        call s1 % insert(8, 'and')
        print *, 's1 = ', s1
        print *, ''
    end block

    ! operator(+) 
    ! ---------------------------------------------------------------
    block
        type(string_t) :: s1
        type(string_t) :: s2
        type(string_t) :: s3

        print *, 'adding two strings using operator(+)'
        s1 = 'black'
        s2 = 'bird'
        s3 = s1 + s2
        print *, 's1 = ', s1
        print *, 's2 = ', s2
        print *, 's3 = s1 + s1 = ', s3
        print *, ''

        print *, 'adding string and character using operator(+)'
        s1 = 'blue'
        s2 = s1 + 'jay'
        print *, "s2 = s1 + 'jay' = ", s2 
        print *, '' 
    end block

    ! operator(//)
    ! ---------------------------------------------------------------
    block
        type(string_t) :: s1
        type(string_t) :: s2
        type(string_t) :: s3

        print *, 'concatenating strings using operator(//)'
        s1 = '12345'
        s2 = '67890'
        s3 = s1 // s2
        print *, 's1 = ', s1
        print *, 's2 = ', s2
        print *, 's3 = ', s3
        print *, ''

        print *, 'concatenating string and characters using operator(//)'
        s1 = 'orange'
        s2 = s1 // ' tree'
        print *, 's1 = ', s1
        print *, 's2 = ', s2
        print *, ''

        print *, 'create string from characters using operator(//)'
        s1 = 'apple' // ' ' // 'blossom'
        print *, 's1 = ', s1
        print *, ''
    end block

    ! find 
    ! ---------------------------------------------------------------
    block
        type(string_t) :: s1
        type(string_t) :: s2
        type(string_t) :: s
        integer        :: n

        print *, 'finding substring using find, string_t argument'
        s1 = 'one, two, three'
        s2 = 'two'
        n = s1 % find(s2)
        print *, 's1 = ', s1
        print *, 's2 = ', s2
        print *, 's1 % find(s2) = ', n
        print *, ''

        print *, 'finding substring using find, characters argument'
        s = '1234bobber1234'
        n = s % find('bobber')
        print *, 's = ', s
        print *, "s % find('bobber') = ", n
        print *, ''

        print *, 'finding substring using find, specify search start'
        s = 'one two one two'
        n = s % find('one', 5)
        print *, 's = ', s
        print *, "s % find('one', 5) = ", n
        print *, ''
    end block

    ! rfind 
    ! ---------------------------------------------------------------
    block
        type(string_t) :: s1
        type(string_t) :: s2
        type(string_t) :: s
        integer        :: n

        print *, 'finding substring using rfind, string_t argument'
        s1 = 'two, one, two, three'
        s2 = 'two'
        n = s1 % rfind(s2)
        print *, 's1 = ', s1
        print *, 's2 = ', s2
        print *, 's1 % rfind(s2) = ', n
        print *, ''

        print *, 'finding substring using rfind, characters argument'
        s = 'bobber, rufus, bobber, rufus'
        n = s % rfind('bobber')
        print *, 's = ', s
        print *, "s % rfind('bobber') = ", n
        print *, ''

        print *, 'finding substring using rfind, specify search start'
        s = 'one two one two'
        n = s % rfind('two', 10)
        print *, 's = ', s
        print *, "s % rfind('two', 10) = ", n
        print *, ''
    end block

    ! replace 
    ! ---------------------------------------------------------------
    block
        type(string_t) :: s1
        type(string_t) :: s2
        type(string_t) :: s3
        type(string_t) :: s

        print *, 'replace substrings using replace, string_t arguments'
        s1 = 'one, two, three, four'
        s2 = ','
        s3 = ';'
        print *, 's1 = ', s1
        print *, 's2 = ', s2
        print *, 's3 = ', s3
        call s1 % replace(s2, s3)
        print *, 'call s1 % replace(s2, s3)'
        print *, ''

        print *, 'replace substrings using replace, character arguments'
        s = 'one, two, three, four'
        print *, 's = ', s
        call s % replace(', ', '-')
        print *, "call s % replace(', ', '-')"
        print *, 's = ', s
        print *, ''

        print *, 'replace substrings using replace with count argument'
        s = 'one one one one one'
        print *, 's = ', s
        call s % replace('one', 'two', 3)
        print *, "call s % replace('one', 'two', 3)"
        print *, 's = ', s
        print *, ''
    end block

    ! operator(==) 
    ! ---------------------------------------------------------------
    block
        type(string_t) :: s1
        type(string_t) :: s2

        print *, 'test if two strings are equal using operator(==)'
        s1 = 'chair'
        s2 = 'chair'
        print *, 's1 = ', s1
        print *, 's2 = ', s2
        if (s1 == s2) then
            print *, 's1 is equal to s2'
        else
            print *, 's1 not equal to s2'
        end if
        print *, ''

        s1 = 'chair'
        s2 = 'table'
        print *, 's1 = ', s1
        print *, 's2 = ', s2
        if (s1 == s2) then
            print *, 's1 is equal to s2'
        else
            print *, 's1 not equal to s2'
        end if
        print *, ''
    end block

    ! operator(==) 
    ! ---------------------------------------------------------------
    block
        type(string_t) :: s1
        type(string_t) :: s2

        print *, 'test if two string are not equal using operator(/=)'
        s1 = 'golf'
        s2 = 'club'
        print *, 's1 = ', s1
        print *, 's2 = ', s2
        if (s1 /= s2) then
            print *, 's1 is not equal to s2'
        else
            print *, 's1 is equal to s2'
        end if
        print *, ''

        s1 = 'club'
        s2 = 'club'
        print *, 's1 = ', s1
        print *, 's2 = ', s2
        if (s1 /= s2) then
            print *, 's1 is not equal to s2'
        else
            print *, 's1 is equal to s2'
        end if
        print *, ''
    end block

end program string_examples
