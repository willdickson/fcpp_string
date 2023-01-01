#include "fmt_capi.hpp"
#define FMT_HEADER_ONLY
#include "fmt/format.h"
#include <iostream>

namespace fmtver = fmt::v9;

std::string *fmt_error_msg(fmtver::format_error &ex)
{ 
    std::string msg;
    msg.append("{formar_error: ");
    msg.append(ex.what());
    msg.append("}");
    return new std::string(msg);
}


std::string *fmt_int32(std::string *fmt_str, int32_t value)
{
    try
    {
        return new std::string(fmt::format(*fmt_str, value));
    }
    catch (fmtver::format_error &ex)
    {
        return fmt_error_msg(ex);
    }
}


std::string *fmt_int64(std::string *fmt_str, int64_t value)
{
    try
    {
        return new std::string(fmt::format(*fmt_str, value));
    }
    catch (fmtver::format_error &ex)
    {
        return fmt_error_msg(ex);
    }
}


std::string *fmt_float(std::string *fmt_str, float value)
{
    try
    {
        return new std::string(fmt::format(*fmt_str, value));
    }
    catch (fmtver::format_error &ex)
    {
        return fmt_error_msg(ex);
    }
}


std::string *fmt_float_with_prec(std::string *fmt_str, float value, int prec)
{
    try
    {
        return new std::string(fmt::format(*fmt_str, value, prec));
    }
    catch (fmtver::format_error &ex)
    {
        return fmt_error_msg(ex);
    }
}


std::string *fmt_double(std::string *fmt_str, double value)
{
    try
    {
        return new std::string(fmt::format(*fmt_str, value));
    }
    catch (fmtver::format_error &ex)
    {
        return fmt_error_msg(ex);
    }
}


std::string *fmt_double_with_prec(std::string *fmt_str, double value, int prec)
{
    try
    {
        return new std::string(fmt::format(*fmt_str, value, prec));
    }
    catch (fmtver::format_error &ex)
    {
        return fmt_error_msg(ex);
    }
}


std::string *fmt_bool(std::string *fmt_str, int value)
{
    try
    {
        return new std::string(fmt::format(*fmt_str, bool(value)));
    }
    catch (fmtver::format_error &ex)
    {
        return fmt_error_msg(ex);
    }
}


std::string *fmt_string(std::string *fmt_str, std::string *val_str)
{
    if (val_str != nullptr) 
    {
        try
        {
            return new std::string(fmt::format(*fmt_str, *val_str));
        }
        catch (fmtver::format_error &ex)
        {
            return fmt_error_msg(ex);
        }
    }
    else
    {
        return new std::string("{}");
    }
}
