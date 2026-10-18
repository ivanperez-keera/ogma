/*******************************************************************************
** File: {{app_name_lc}}.c
**
** Purpose:
**   This file contains the source code for the {{app_name_uc}}.
**
*******************************************************************************/

/*
**   Include Files:
*/

#include "{{app_name_lc}}.h"
#include "{{app_name_lc}}_perfids.h"
#include "{{app_name_lc}}_msgids.h"
#include "{{app_name_lc}}_msg.h"
#include "{{app_name_lc}}_eventids.h"
#include "{{app_name_lc}}_version.h"
#include "{{app_name_lc}}_dispatch.h"

{{#impl_extra_header}}
{{{.}}}
{{/impl_extra_header}}

{{#copilot}}
#include "{{{copilot.specName}}}_types.h"
#include "{{{copilot.specName}}}.h"
{{/copilot}}

{{#variables}}
{{varDeclType}} {{varDeclName}};
{{/variables}}

/*
** global data
*/

copilot_hk_tlm_t    {{app_name_uc}}_HkTelemetryPkt;
CFE_SB_PipeId_t     {{app_name_uc}}_CommandPipe;
CFE_SB_Buffer_t    *SBBufPtr;

static CFE_EVS_BinFilter_t  {{app_name_uc}}_EventFilters[] =
       {  /* Event ID    mask */
          { {{app_name_uc}}_STARTUP_INF_EID,       0x0000},
          { {{app_name_uc}}_COMMAND_ERR_EID,       0x0000},
          { {{app_name_uc}}_COMMANDCPVIOL_INF_EID, 0x0000},
       };

/** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* {{app_name_uc}}_AppMain() -- Application entry point and main process loop          */
/*                                                                            */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * *  * * * * **/
void {{app_name_uc}}_AppMain( void )
{
    CFE_Status_t status;
    uint32 RunStatus = CFE_ES_RunStatus_APP_RUN;

    CFE_ES_PerfLogEntry({{app_name_uc}}_PERF_ID);

    status = {{app_name_uc}}_AppInit();

    if (status != CFE_SUCCESS)
    {
        RunStatus = CFE_ES_RunStatus_APP_ERROR;
    }

    /*
    ** {{app_name_uc}} Runloop
    */
    while (CFE_ES_RunLoop(&RunStatus) == true)
    {
        CFE_ES_PerfLogExit({{app_name_uc}}_PERF_ID);

        /* Pend on receipt of command packet -- timeout set to 500 millisecs */
        status = CFE_SB_ReceiveBuffer(&SBBufPtr, {{app_name_uc}}_CommandPipe, 500);

        CFE_ES_PerfLogEntry({{app_name_uc}}_PERF_ID);

        if (status == CFE_SUCCESS)
        {
            {{app_name_uc}}_TaskPipe(SBBufPtr);
        }
    }

    CFE_ES_PerfLogExit({{app_name_uc}}_PERF_ID);

    CFE_ES_ExitApp(RunStatus);

} /* End of {{app_name_uc}}_AppMain() */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  */
/*                                                                            */
/* {{app_name_uc}}_AppInit() --  initialization                                       */
/*                                                                            */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **/
CFE_Status_t {{app_name_uc}}_AppInit(void)
{
    CFE_Status_t status;

    /* Initialize housekeeping packet (clear user data area) */
    CFE_MSG_Init(CFE_MSG_PTR({{app_name_uc}}_HkTelemetryPkt.TelemetryHeader),
                 CFE_SB_ValueToMsgId({{app_name_uc}}_HK_TLM_MID), sizeof(copilot_hk_tlm_t));

    /*
    ** Register the events
    */
    status = CFE_EVS_Register({{app_name_uc}}_EventFilters,
                     sizeof({{app_name_uc}}_EventFilters)/sizeof(CFE_EVS_BinFilter_t),
                     CFE_EVS_EventFilter_BINARY);

    if (status != CFE_SUCCESS)
    {
        CFE_ES_WriteToSysLog("{{app_name_uc}}: Error Registering Events, RC = 0x%08lX\n", (unsigned long)status);
    }
    else
    {
        /*
        ** Create the Software Bus command pipe and subscribe to housekeeping
        **  messages
        */
        status = CFE_SB_CreatePipe(&{{app_name_uc}}_CommandPipe, {{app_name_uc}}_PIPE_DEPTH,"{{app_name_uc}}_CMD_PIPE");

        if (status != CFE_SUCCESS)
        {
            CFE_EVS_SendEvent({{app_name_uc}}_CR_PIPE_ERR_EID,
                              CFE_EVS_EventType_ERROR,
                              "{{app_name_uc}}: Error creating SB Command Pipe, RC = 0x%08lX",
                              (unsigned long)status);
        }
    }

    if (status == CFE_SUCCESS)
    {
        status = CFE_SB_Subscribe(CFE_SB_ValueToMsgId({{app_name_uc}}_CMD_MID), {{app_name_uc}}_CommandPipe);

        if (status != CFE_SUCCESS)
        {
            CFE_EVS_SendEvent({{app_name_uc}}_CR_SUB_ERR_EID,
                              CFE_EVS_EventType_ERROR,
                              "{{app_name_uc}}: Error subscribing to {{app_name_uc}} CMD, RC = 0x%08lX",
                              (unsigned long)status);
        }
    }

    if (status == CFE_SUCCESS)
    {
        status = CFE_SB_Subscribe(CFE_SB_ValueToMsgId({{app_name_uc}}_SEND_HK_MID), {{app_name_uc}}_CommandPipe);

        if (status != CFE_SUCCESS)
        {
            CFE_EVS_SendEvent({{app_name_uc}}_CR_SUB_ERR_EID,
                              CFE_EVS_EventType_ERROR,
                              "{{app_name_uc}}: Error subscribing to {{app_name_uc}} SEND HK, RC = 0x%08lX",
                              (unsigned long)status);
        }
    }

    {{#msgIds}}
    if (status == CFE_SUCCESS)
    {
        status = CFE_SB_Subscribe(CFE_SB_ValueToMsgId({{.}}), {{APP_NAME}}_CommandPipe);

        if (status != CFE_SUCCESS)
        {
            CFE_EVS_SendEvent({{APP_NAME}}_CR_SUB_ERR_EID,
                              CFE_EVS_EventType_ERROR,
                              "{{app_name_uc}}: Error subscribing to necessary messages, RC = 0x%08lX",
                              (unsigned long)status);
        }
    }
    {{/msgIds}}

    if (status == CFE_SUCCESS)
    {
        status = CFE_SB_Subscribe(CFE_SB_ValueToMsgId({{APP_NAME}}_REEVAL_CMD_MID), {{APP_NAME}}_CommandPipe);

        if (status != CFE_SUCCESS)
        {
            CFE_EVS_SendEvent({{APP_NAME}}_CR_SUB_ERR_EID,
                              CFE_EVS_EventType_ERROR,
                              "{{app_name_uc}}: Error subscribing to necessary messages, RC = 0x%08lX",
                              (unsigned long)status);
        }
    }

    CFE_EVS_SendEvent ({{app_name_uc}}_STARTUP_INF_EID, CFE_EVS_EventType_INFORMATION,
               "{{app_name_uc}} App Initialized. Version %d.%d.%d.%d",
                {{app_name_uc}}_MAJOR_VERSION,
                {{app_name_uc}}_MINOR_VERSION,
                {{app_name_uc}}_REVISION,
                {{app_name_uc}}_MISSION_REV);

    return status;

} /* End of {{app_name_uc}}_AppInit() */

{{#msgHandlers}}
/**
* Make received data available to Copilot and run monitors.
*/
void {{APP_NAME}}_Process{{msgDataDesc}}(const CFE_SB_Buffer_t *BufPtr)
{
    {{#msgDataFromType}}
    {{msgDataFromType}}* msg;
    msg = ({{.}}*) BufPtr;
    {{/msgDataFromType}}
    {{^msgDataFromType}}
    {{msgDataVarType}}* msg;
    msg = ({{msgDataVarType}}*) BufPtr;
    {{/msgDataFromType}}
    {{#msgDataFromField}}
    {{msgDataVarName}} = msg->{{.}};
    {{/msgDataFromField}}
    {{^msgDataFromField}}
    {{msgDataVarName}} = *msg;
    {{/msgDataFromField}}

    {{#msgDataActive}}
    // Run all copilot monitors.
    copilot_step();
    {{/msgDataActive}}
}

{{/msgHandlers}}
{{#triggers}}
/**
 * Report copilot property violations.
 */
{{#triggerType}}
void {{triggerName}}({{.}} arg) {
{{/triggerType}}
{{^triggerType}}
void {{triggerName}}(void) {
{{/triggerType}}
    CFE_EVS_SendEvent({{APP_NAME}}_COMMANDCPVIOL_INF_EID, CFE_EVS_EventType_ERROR,
        "{{APP_NAME}}: violation: {{triggerName}}");
}
{{/triggers}}

/**
 * Reset counters
 */
void {{app_name_uc}}_ResetCounters(void)
{
    {{app_name_uc}}_HkTelemetryPkt.Payload.copilot_command_count = 0;
    {{app_name_uc}}_HkTelemetryPkt.Payload.copilot_command_error_count = 0;
}

