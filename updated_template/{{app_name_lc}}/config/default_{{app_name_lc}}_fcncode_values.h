#ifndef DEFAULT_{{app_name_uc}}_FCNCODE_VALUES_H
#define DEFAULT_{{app_name_uc}}_FCNCODE_VALUES_H

#define {{app_name_uc}}_CCVAL(x) {{app_name_uc}}_FunctionCode_##x

enum {{app_name_uc}}_FunctionCode
{
    {{app_name_uc}}_FunctionCode_NOOP           = 0,
    {{app_name_uc}}_FunctionCode_RESET_COUNTERS = 1,
    {{app_name_uc}}_FunctionCode_REEVAL         = 2,
};

#endif /* DEFAULT_{{app_name_uc}}_FCNCODE_VALUES_H */
