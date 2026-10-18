#ifdef CFE_EDS_ENABLED

#include "{{app_name_lc}}.h"
#include "{{app_name_lc}}_dispatch.h"
#include "{{app_name_lc}}_cmds.h"
#include "{{app_name_lc}}_eventids.h"
#include "{{app_name_lc}}_msgids.h"
#include "{{app_name_lc}}_msg.h"

#include "{{app_name_lc}}_eds_dispatcher.h"
#include "{{app_name_lc}}_eds_dictionary.h"

{{#impl_extra_header}}
{{{.}}}
{{/impl_extra_header}}

extern copilot_hk_tlm_t {{app_name_uc}}_HkTelemetryPkt;

static const EdsDispatchTable_EdsComponent_{{app_name_uc}}_Application_CFE_SB_Telecommand_t {{app_name_uc}}_TC_DISPATCH_TABLE =
{
    .CMD =
    {
        .NoopCmd_indication          = {{app_name_uc}}_NoopCmd,
        .ResetCountersCmd_indication = {{app_name_uc}}_ResetCountersCmd,
        .ReevalCmd_indication        = {{app_name_uc}}_ReevalCmd
    },
    .SEND_HK =
    {
        .indication = {{app_name_uc}}_SendHkCmd
    }
};

void {{app_name_uc}}_TaskPipe(const CFE_SB_Buffer_t *BufPtr)
{
    CFE_Status_t      Status;
    CFE_SB_MsgId_t    MsgId;
    CFE_MSG_Size_t    MsgSize;
    CFE_MSG_FcnCode_t MsgFc;

    /* Check if it's a known {{app_name_uc}} command */
    Status = EdsDispatch_EdsComponent_{{app_name_uc}}_Application_Telecommand(BufPtr, &{{app_name_uc}}_TC_DISPATCH_TABLE);

    if (Status != CFE_SUCCESS)
    {
        CFE_MSG_GetMsgId(&BufPtr->Msg, &MsgId);

        if (false) {}
        {{#msgCases}}
        else if (CFE_SB_MsgId_Equal(MsgId, CFE_SB_ValueToMsgId({{msgInfoId}})))
        {
            {{app_name_uc}}_Process{{msgInfoDesc}}(BufPtr);
        }
        {{/msgCases}}
        else
        {
            CFE_MSG_GetSize(&BufPtr->Msg, &MsgSize);
            CFE_MSG_GetFcnCode(&BufPtr->Msg, &MsgFc);
            ++{{app_name_uc}}_HkTelemetryPkt.Payload.copilot_command_error_count;

            if (Status == CFE_STATUS_UNKNOWN_MSG_ID)
            {
                CFE_EVS_SendEvent({{app_name_uc}}_COMMAND_ERR_EID, CFE_EVS_EventType_ERROR,
                                  "{{app_name_uc}}: invalid command packet,MID = 0x%x", (unsigned int)CFE_SB_MsgIdToValue(MsgId));
            }
            else if (Status == CFE_STATUS_WRONG_MSG_LENGTH)
            {
                CFE_EVS_SendEvent({{app_name_uc}}_COMMAND_ERR_EID, CFE_EVS_EventType_ERROR,
                                  "{{app_name_uc}}: Invalid Msg length: ID = 0x%X, CC = %u, Len = %u",
                                  (unsigned int)CFE_SB_MsgIdToValue(MsgId), (unsigned int)MsgFc, (unsigned int)MsgSize);
            }
            else
            {
                CFE_EVS_SendEvent({{app_name_uc}}_COMMAND_ERR_EID, CFE_EVS_EventType_ERROR,
                                  "{{app_name_uc}}: Invalid ground command code: CC = %d", (int)MsgFc);
            }
        }
    }
}

#endif
