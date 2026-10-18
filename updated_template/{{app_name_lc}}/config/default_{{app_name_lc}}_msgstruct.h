#ifndef DEFAULT_{{app_name_uc}}_MSGSTRUCT_H
#define DEFAULT_{{app_name_uc}}_MSGSTRUCT_H

#include "{{app_name_lc}}_msgdefs.h"
#include "cfe_msg_hdr.h"

typedef struct
{
    CFE_MSG_CommandHeader_t CommandHeader;
} {{app_name_uc}}_NoopCmd_t;

typedef struct
{
    CFE_MSG_CommandHeader_t CommandHeader;
} {{app_name_uc}}_ResetCountersCmd_t;

typedef struct
{
    CFE_MSG_CommandHeader_t CommandHeader;
} {{app_name_uc}}_ReevalCmd_t;

typedef struct
{
    CFE_MSG_CommandHeader_t CommandHeader;
} {{app_name_uc}}_SendHkCmd_t;

typedef struct
{
    CFE_MSG_TelemetryHeader_t TelemetryHeader;
    {{app_name_uc}}_HkTlm_Payload_t Payload;
} {{app_name_uc}}_HkTlm_t;

#endif /* DEFAULT_{{app_name_uc}}_MSGSTRUCT_H */
