module fcppstring
    use cpp_string_m, only : string_t
    use cpp_string_m, only : char
    use, intrinsic :: iso_c_binding,  only : c_size_t
    implicit none
    private

    public :: test 

contains
    subroutine test
        type(string_t) :: s1 
        type(string_t) :: s2
        type(string_t) :: s3 
        type(string_t) :: s4

        s2 = string_t('string value')
        s3 = s1 + 'this thing' 

        call s3 % push_back('a')

        !s3 = string_t(s2)

        s4 = s2 % at(1)
        print *, char(s4)

        print *, 's1 = ', char(s1) 
        print *, 'len(char(s1)) = ', len(char(s1))
        print *, ''
        print *, 's2 = ', char(s2) 
        print *, 'len(char(s2)) = ', len(char(s2))
        print *, ''
        print *, 's3 = ', char(s3) 
        print *, 'len(char(s3)) = ', len(char(s3))

        s2 = string_t('another value')

        print *, ''
        print *, 's2 = ', char(s2) 
        print *, 'len(char(s2)) = ', len(char(s2))
        print *, ''
        print *, 's3 = ', char(s3) 
        print *, 'len(char(s3)) = ', len(char(s3))

        call s3 % clear()

        print *, ''
        print *, 's2 = ', char(s2) 
        print *, 'len(char(s2)) = ', len(char(s2))
        print *, ''
        print *, 's3 = ', char(s3) 
        print *, 'len(char(s3)) = ', len(char(s3))
        print *, ''

        s1 = string_t('hello')
        call s3 % append(s1)
        call s3 % append(s2)
        print *, ''
        print *, 's3 = ', char(s3) 
        print *, 'len(char(s3)) = ', len(char(s3))
        print *, ''

        s1 = 'a'
        s2 = 'b'

        print *, 's1 = ', char(s1) 
        print *, 's2 = ', char(s2) 
        print *, 's1 % compare(s2) = ', s1 % compare(s2)
        print *, 's1 == s2, ', s1 == s2
        print *, 's1 == s1, ', s1 == s1
        print *, 's1 /= s2, ', s1 /= s2
        print *, 's1 /= s1, ', s1 /= s1
        print *, ''

        s1 = '123456'
        s2 = 'abc'
        print *, 's1 = ', char(s1)
        call s1 % erase(2,3)
        print *, 's1 = ', char(s1)
        print *, ''

        call s1 % insert(2, s2)
        print *, char(s1)
        print *, ''

        call s1 % pop_back()
        print *, char(s1)
        print *, ''
        call s1 % pop_back()
        print *, char(s1)
        print *, ''


    end subroutine test
end module fcppstring
