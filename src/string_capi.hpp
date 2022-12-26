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

    void string_append_from_char(std::string *s, char c[]);

    void string_push_back(std::string *s, char c[1]);

    int string_compare(std::string *s0, std::string *s1);

    bool string_empty(std::string *s);

    void string_erase(std::string *s, size_t pos, size_t len);

    void string_insert(std::string *s0, size_t pos, std::string *s1);

    void string_pop_back(std::string *s);

    size_t string_find(std::string *s0, std::string *s1, size_t pos);

    size_t string_find_from_char(std::string *s0, char c[], size_t pos);

    size_t string_rfind(std::string *s0, std::string *s1, size_t pos);

    size_t string_rfind_from_char(std::string *s0, char c[], size_t pos);
}

#endif // STRING_CAPI_HPP
