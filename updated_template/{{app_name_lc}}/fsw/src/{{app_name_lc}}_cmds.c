#include "{{app_name_lc}}_cmds.h"
#include "{{app_name_lc}}.h"
#include "{{app_name_lc}}_eventids.h"
#include "copilot.h"

extern copilot_hk_tlm_t {{app_name_uc}}_HkTelemetryPkt;

#ifdef CFE_EDS_ENABLED

CFE_Status_t {{app_name_uc}}_NoopCmd(const EdsDataType_{{app_name_uc}}_NoopCmd_t *Msg)
{
    {{app_name_uc}}_HkTelemetryPkt.Payload.copilot_command_count++;
    CFE_EVS_SendEvent({{app_name_uc}}_COMMAND_ERR_EID, CFE_EVS_EventType_INFORMATION, "{{app_name_uc}}: No-op command");
    return CFE_SUCCESS;
}

CFE_Status_t {{app_name_uc}}_ResetCountersCmd(const EdsDataType_{{app_name_uc}}_ResetCountersCmd_t *Msg)
{
    {{app_name_uc}}_ResetCounters();
    return CFE_SUCCESS;
}

CFE_Status_t {{app_name_uc}}_ReevalCmd(const EdsDataType_{{app_name_uc}}_ReevalCmd_t *Msg)
{
    {{app_name_uc}}_HkTelemetryPkt.Payload.copilot_command_count++;
    copilot_step();
    return CFE_SUCCESS;
}

CFE_Status_t {{app_name_uc}}_SendHkCmd(const EdsDataType_{{app_name_uc}}_SendHkCmd_t *Msg)
{
    CFE_SB_TimeStampMsg(CFE_MSG_PTR({{app_name_uc}}_HkTelemetryPkt.TelemetryHeader));
    CFE_SB_TransmitMsg(CFE_MSG_PTR({{app_name_uc}}_HkTelemetryPkt.TelemetryHeader), true);
    return CFE_SUCCESS;
}

#else

CFE_Status_t {{app_name_uc}}_NoopCmd(const void *Msg)
{
    {{app_name_uc}}_HkTelemetryPkt.Payload.copilot_command_count++;
    CFE_EVS_SendEvent({{app_name_uc}}_COMMAND_ERR_EID, CFE_EVS_EventType_INFORMATION, "{{app_name_uc}}: No-op command");
    return CFE_SUCCESS;
}

CFE_Status_t {{app_name_uc}}_ResetCountersCmd(const void *Msg)
{
    {{app_name_uc}}_ResetCounters();
    return CFE_SUCCESS;
}

CFE_Status_t {{app_name_uc}}_ReevalCmd(const void *Msg)
{
    {{app_name_uc}}_HkTelemetryPkt.Payload.copilot_command_count++;
    copilot_step();
    return CFE_SUCCESS;
}

CFE_Status_t {{app_name_uc}}_SendHkCmd(const void *Msg)
{
    CFE_SB_TimeStampMsg(CFE_MSG_PTR({{app_name_uc}}_HkTelemetryPkt.TelemetryHeader));
    CFE_SB_TransmitMsg(CFE_MSG_PTR({{app_name_uc}}_HkTelemetryPkt.TelemetryHeader), true);
    return CFE_SUCCESS;
}

#endif
