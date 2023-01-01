#ifndef FMT_CAPI_H
#define FMT_CAPI_H
#include <string>
#include <cstdint> 

extern "C" 
{
    std::string *fmt_int32(std::string *fmt_str, int32_t value);

    std::string *fmt_int64(std::string *fmt_str, int64_t value);

    std::string *fmt_size(std::string *fmt_str, size_t value);

    std::string *fmt_float(std::string *fmt_str, float value);

    std::string *fmt_float_with_prec(std::string *fmt_str, float value, int prec);

    std::string *fmt_double(std::string *fmt_str, double value);

    std::string *fmt_double_with_prec(std::string *fmt_str, double value, int prec);
}

#endif // FMT_CAPI_H
