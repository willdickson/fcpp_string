#ifndef FMT_CAPI_H
#define FMT_CAPI_H
#include <string>
#include <cstdint> 

extern "C" 
{
    std::string *fmt_float(std::string *fmt_str, float value);

    std::string *fmt_float_with_prec(std::string *fmt_str, float value, int prec);

    std::string *fmt_double(std::string *fmt_str, double value);

    std::string *fmt_double_with_prec(std::string *fmt_str, double value, int prec);
}

#endif // FMT_CAPI_H
