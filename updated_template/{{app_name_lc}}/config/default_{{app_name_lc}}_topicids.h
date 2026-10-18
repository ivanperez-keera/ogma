#ifndef DEFAULT_{{app_name_uc}}_TOPICIDS_H
#define DEFAULT_{{app_name_uc}}_TOPICIDS_H

#include "{{app_name_lc}}_topicid_values.h"

/* Command topic IDs */
#define {{app_name_uc}}_MISSION_CMD_TOPICID             {{app_name_uc}}_MISSION_TIDVAL(CMD)
#define DEFAULT_{{app_name_uc}}_MISSION_CMD_TOPICID     0x82

#define {{app_name_uc}}_MISSION_SEND_HK_TOPICID         {{app_name_uc}}_MISSION_TIDVAL(SEND_HK)
#define DEFAULT_{{app_name_uc}}_MISSION_SEND_HK_TOPICID 0x83

/* Telemetry topic IDs */
#define {{app_name_uc}}_MISSION_HK_TLM_TOPICID          {{app_name_uc}}_MISSION_TIDVAL(HK_TLM)
#define DEFAULT_{{app_name_uc}}_MISSION_HK_TLM_TOPICID  0x83

#endif /* DEFAULT_{{app_name_uc}}_TOPICIDS_H */
