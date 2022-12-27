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


void string_append_from_char(std::string *s, char c[])
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


void string_erase(std::string *s, size_t pos, size_t len)
{
    if (s != nullptr) 
    {
        s -> erase(pos,len);
    }
}


void string_insert(std::string *s0, size_t pos, std::string *s1)
{
    if ((s0 != nullptr) && (s1 != nullptr))
    {
        s0 -> insert(pos, *s1);
    }
}


void string_pop_back(std::string *s)
{
    if (s != nullptr)
    {
        s -> pop_back();
    }
}


size_t string_find(std::string *s0, std::string *s1, size_t pos)
{
    size_t rval = std::string::npos;
    if ((s0 != nullptr) && (s1 != nullptr))
    {
        rval = s0 -> find(*s1, pos);
    }
    return rval;
}


size_t string_find_from_char(std::string *s, char c[], size_t pos)
{
    size_t rval = std::string::npos;
    if (s != nullptr) 
    {
        rval = s -> find(c, pos);
    }
    return rval;
}


size_t string_rfind(std::string *s0, std::string *s1, size_t pos)
{
    size_t rval = std::string::npos;
    if ((s0 != nullptr) && (s1 != nullptr)) 
    {
        rval = s0 -> rfind(*s1, pos);
    }
    return rval;
}


size_t string_rfind_from_char(std::string *s, char c[], size_t pos)
{
    size_t rval = std::string::npos;
    if (s != nullptr) 
    {
        rval = s -> rfind(c, pos);
    }
    return rval;
}

