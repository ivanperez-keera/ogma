/************************************************************************
** File:
**   $Id: copilot_cfs_msgids.h  $
**
** Purpose:
**  Define Copilot App  Message IDs
**
** Notes:
**
**
*************************************************************************/
#ifndef _copilot_cfs_msgids_h_
#define _copilot_cfs_msgids_h_

{{#CFS_CMD_MID}}
#define COPILOT_CFS_CMD_MID        {{.}}
{{/CFS_CMD_MID}}
{{^CFS_CMD_MID}}
#define COPILOT_CFS_CMD_MID        0x1882
{{/CFS_CMD_MID}}
{{#CFS_REEVAL_CMD_MID}}
#define COPILOT_CFS_REEVAL_CMD_MID {{.}}
{{/CFS_REEVAL_CMD_MID}}
{{^CFS_REEVAL_CMD_MID}}
#define COPILOT_CFS_REEVAL_CMD_MID 0x1883
{{/CFS_REEVAL_CMD_MID}}
{{#CFS_SEND_HK_MID}}
#define COPILOT_CFS_SEND_HK_MID    {{.}}
{{/CFS_SEND_HK_MID}}
{{^CFS_SEND_HK_MID}}
#define COPILOT_CFS_SEND_HK_MID    0x1884
{{/CFS_SEND_HK_MID}}
{{#CFS_HK_TLM_MID}}
#define COPILOT_CFS_HK_TLM_MID     {{.}}
{{/CFS_HK_TLM_MID}}
{{^CFS_HK_TLM_MID}}
#define COPILOT_CFS_HK_TLM_MID     0x0883
{{/CFS_HK_TLM_MID}}

#endif /* _copilot_cfs_msgids_h_ */

/************************/
/*  End of File Comment */
/************************/
