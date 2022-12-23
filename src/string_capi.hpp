#ifndef STRING_CAPI_HPP
#define STRING_CAPI_HPP
#include <string>
#include <iostream>

extern "C" 
{
    std::string *string_new_empty();

    std::string *string_new_from_char(char s[]);

    std::string *string_new_from_ptr(std::string *s);

    void string_delete(std::string *s);

    size_t string_size(std::string *s);

    char string_at(std::string *s, size_t n);

    void string_clear(std::string *s);

    void string_append(std::string *s0, std::string *s1);

    void string_append_char(std::string *s, char c[]);

    int string_compare(std::string *s0, std::string *s1);

}

#endif // STRING_CAPI_HPP
