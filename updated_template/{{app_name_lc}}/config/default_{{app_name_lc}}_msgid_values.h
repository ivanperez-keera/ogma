#ifndef DEFAULT_{{app_name_uc}}_MSGID_VALUES_H
#define DEFAULT_{{app_name_uc}}_MSGID_VALUES_H

#include "default_{{app_name_lc}}_topicids.h"

#define {{app_name_uc}}_CMD_PLATFORM_MIDVAL(x) CFE_PLATFORM_CMD_TOPICID_TO_MIDV({{app_name_uc}}_MISSION_##x##_TOPICID)
#define {{app_name_uc}}_TLM_PLATFORM_MIDVAL(x) CFE_PLATFORM_TLM_TOPICID_TO_MIDV({{app_name_uc}}_MISSION_##x##_TOPICID)

#endif /* DEFAULT_{{app_name_uc}}_MSGID_VALUES_H */
