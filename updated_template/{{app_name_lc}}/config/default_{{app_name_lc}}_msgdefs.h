#ifndef DEFAULT_{{app_name_uc}}_MSGDEFS_H
#define DEFAULT_{{app_name_uc}}_MSGDEFS_H

#include "common_types.h"

typedef struct {{app_name_uc}}_HkTlm_Payload
{
    uint8 CommandErrorCounter;
    uint8 CommandCounter;
    uint8 spare[2];
} {{app_name_uc}}_HkTlm_Payload_t;

#endif /* DEFAULT_{{app_name_uc}}_MSGDEFS_H */
