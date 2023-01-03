program basic
    use fcpp_string, only : string_t   
    use fcpp_string, only : string_fmt
    use fcpp_string, only : char
    use fcpp_string, only : len
    
    type(string_t)            :: s
    integer                   :: n
    character(:), allocatable :: c
    
    ! constructor
    s = string_t('a string')
    
    ! construct from character
    s = 'another fine string'
    
    ! replace method
    call s % replace('fine', 'simple')
    
    ! get string length with size
    n = s % size()
    
    ! can also use len to get string length
    n = len(s)
    
    ! find a word
    n = s % find('fine')
    
    ! concatenation
    s = s // ' blah, blah, blah'
    
    ! formating integers
    s = string_fmt('{06d}', 12) 
    
    ! formating floats
    s = 'the velocity = ' // string_fmt('{1.2f}', 1.0/3.0) // ' (m/s)' 
    
    ! works with print/write 
    print *, s
    
    ! convert to character with to_character
    c = s % to_character()
    
    ! or with char
    c = char(s)

end program basic
