{{=<% %>=}}
###########################################################
#
# <%app_name_uc%> mission build setup
#
# This file is evaluated as part of the "prepare" stage
# and can be used to set up prerequisites for the build,
# such as generating header files
#
###########################################################

# The list of header files that control the <%app_name_uc%> configuration
set(<%app_name_uc%>_MISSION_CONFIG_FILE_LIST
    <%app_name_lc%>_fcncode_values.h
    <%app_name_lc%>_interface_cfg_values.h
    <%app_name_lc%>_mission_cfg.h
    <%app_name_lc%>_msgdefs.h
    <%app_name_lc%>_msg.h
    <%app_name_lc%>_msgstruct.h
    <%app_name_lc%>_perfids.h
    <%app_name_lc%>_topicid_values.h
)

generate_configfile_set(${<%app_name_uc%>_MISSION_CONFIG_FILE_LIST})
