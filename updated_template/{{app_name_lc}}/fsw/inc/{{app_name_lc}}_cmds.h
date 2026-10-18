#ifndef {{app_name_uc}}_CMDS_H
#define {{app_name_uc}}_CMDS_H

#include "cfe.h"
#include "{{app_name_lc}}_msg.h"

#ifdef CFE_EDS_ENABLED
#include "{{app_name_lc}}_eds_dictionary.h"

CFE_Status_t {{app_name_uc}}_NoopCmd(const EdsDataType_{{app_name_uc}}_NoopCmd_t *Msg);
CFE_Status_t {{app_name_uc}}_ResetCountersCmd(const EdsDataType_{{app_name_uc}}_ResetCountersCmd_t *Msg);
CFE_Status_t {{app_name_uc}}_ReevalCmd(const EdsDataType_{{app_name_uc}}_ReevalCmd_t *Msg);
CFE_Status_t {{app_name_uc}}_SendHkCmd(const EdsDataType_{{app_name_uc}}_SendHkCmd_t *Msg);

#else

CFE_Status_t {{app_name_uc}}_NoopCmd(const void *Msg);
CFE_Status_t {{app_name_uc}}_ResetCountersCmd(const void *Msg);
CFE_Status_t {{app_name_uc}}_ReevalCmd(const void *Msg);
CFE_Status_t {{app_name_uc}}_SendHkCmd(const void *Msg);

#endif

#endif /* {{app_name_uc}}_CMDS_H */
