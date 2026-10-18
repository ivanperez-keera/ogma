#ifndef CFE_EDS_ENABLED

#include "{{app_name_lc}}.h"
#include "{{app_name_lc}}_dispatch.h"
#include "{{app_name_lc}}_cmds.h"
#include "{{app_name_lc}}_eventids.h"
#include "{{app_name_lc}}_msgids.h"
#include "{{app_name_lc}}_msg.h"
#include "{{app_name_lc}}_fcncodes.h"

{{#impl_extra_header}}
{{{.}}}
{{/impl_extra_header}}

void {{app_name_uc}}_ProcessGroundCommand(const CFE_SB_Buffer_t *BufPtr)
{
    CFE_MSG_FcnCode_t CommandCode;

    CFE_MSG_GetFcnCode(&BufPtr->Msg, &CommandCode);

    switch (CommandCode)
    {
        case {{app_name_uc}}_NOOP_CC:
            {{app_name_uc}}_NoopCmd(BufPtr);
            break;

        case {{app_name_uc}}_RESET_CC:
            {{app_name_uc}}_ResetCountersCmd(BufPtr);
            break;

        case {{app_name_uc}}_REEVAL_CC:
            {{app_name_uc}}_ReevalCmd(BufPtr);
            break;

        default:
            CFE_EVS_SendEvent({{app_name_uc}}_COMMAND_ERR_EID, CFE_EVS_EventType_ERROR,
                              "Invalid ground command code: CC = %d", CommandCode);
            break;
    }
}

void {{app_name_uc}}_TaskPipe(const CFE_SB_Buffer_t *BufPtr)
{
    CFE_SB_MsgId_t MsgId;

    CFE_MSG_GetMsgId(&BufPtr->Msg, &MsgId);

    if (CFE_SB_MsgId_Equal(MsgId, CFE_SB_ValueToMsgId({{app_name_uc}}_CMD_MID)))
    {
        {{app_name_uc}}_ProcessGroundCommand(BufPtr);
    }
    else if (CFE_SB_MsgId_Equal(MsgId, CFE_SB_ValueToMsgId({{app_name_uc}}_SEND_HK_MID)))
    {
        {{app_name_uc}}_SendHkCmd(BufPtr);
    }
    {{#msgCases}}
    else if (CFE_SB_MsgId_Equal(MsgId, CFE_SB_ValueToMsgId({{msgInfoId}})))
    {
        {{app_name_uc}}_Process{{msgInfoDesc}}(BufPtr);
    }
    {{/msgCases}}
    else
    {
        CFE_EVS_SendEvent({{app_name_uc}}_COMMAND_ERR_EID, CFE_EVS_EventType_ERROR,
                          "{{app_name_uc}}: invalid command packet,MID = 0x%x", (unsigned int)CFE_SB_MsgIdToValue(MsgId));
    }
}

#endif
