#include "fmt_capi.hpp"
#define FMT_HEADER_ONLY
#include "fmt/format.h"
#include <iostream>

std::string *fmt_float(std::string *fmt_str, float value)
{
    return new std::string(fmt::format(*fmt_str, value));
}


std::string *fmt_float_with_prec(std::string *fmt_str, float value, int prec)
{
    return new std::string(fmt::format(*fmt_str, value, prec));
}


std::string *fmt_double(std::string *fmt_str, double value)
{
    return new std::string(fmt::format(*fmt_str, value));
}


std::string *fmt_double_with_prec(std::string *fmt_str, double value, int prec)
{
    return new std::string(fmt::format(*fmt_str, value, prec));
}
