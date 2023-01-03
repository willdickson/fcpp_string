![fcpp_string](media/fcpp_string_logo.png)
==========================================

fcpp_string is a fortran wrapper for c++ strings designed to (hopefully) make
it a bit easier to mix c++ and fortran code.  It provides a string type
*string_t* and some basic string formatting *string_fmt* based on
[fmtlib](https://github.com/fmtlib/fmt).

The string_t type is mostly a straight wrapper of C++'s std::string class. It
provides many of the same methods as std::string and for the most part they
work in the same way.  There are a couple of exceptions such as *replace* which
is implement to operate like Python's replace.

Some simple examples:

```fortran

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
```







