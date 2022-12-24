#include "string_capi.hpp"


std::string *string_new_empty()
{
    return new std::string();
}


std::string *string_new_from_char(char s[])
{
    return new std::string(s);
}


std::string *string_new_from_ptr(std::string *s)
{
    return new std::string(*s);
}

void string_delete(std::string *s)
{
    if (s != nullptr) 
    {
        delete s;
    }
}


size_t string_size(std::string *s)
{
    size_t val = 0;
    if (s != nullptr) 
    {
        val = s -> size();
    }
    return val;
}


char string_at(std::string *s, size_t n)
{
    char val = '\0';
    if (s != nullptr) 
    {
        if (n < s->size()) {
            val = s -> at(n);
        }
    }
    return val;
}


void string_clear(std::string *s)
{
    if (s != nullptr) 
    {
        s -> clear();
    }
}


void string_append(std::string *s0, std::string *s1)
{
    if ((s0 != nullptr) && (s1 != nullptr))
    {
        s0 -> append(*s1);
    }

}


void string_append_char(std::string *s, char c[])
{
    if (s != nullptr)
    {
        s -> append(c);
    }
}


void string_push_back(std::string *s, char c[1])
{
    if (s != nullptr)
    {
        s -> push_back(c[0]);
    }
}


int string_compare(std::string *s0, std::string *s1)
{
    int rval = 0;
    if ((s0 != nullptr) && (s1 != nullptr)) 
    {
        rval = s0 -> compare(*s1);
    }
    else if ((s0 != nullptr) && (s1 == nullptr))
    {
        rval = s0 -> compare(std::string());
    }
    else if ((s0 == nullptr) && (s1 != nullptr))
    {
        rval = std::string().compare(*s1);
    }
    return rval;
}


bool string_empty(std::string *s)
{
    bool rval = true;
    if (s != nullptr) 
    {
        rval = s -> empty();
    }
    return rval;
}


