{{=<% %>=}}
###########################################################
#
# <%app_name_uc%> platform build setup
#
# This file is evaluated as part of the "prepare" stage
# and can be used to set up prerequisites for the build,
# such as generating header files
#
###########################################################

# The list of header files that control the <%app_name_uc%> configuration
set(<%app_name_uc%>_PLATFORM_CONFIG_FILE_LIST
  <%app_name_lc%>_internal_cfg_values.h
  <%app_name_lc%>_platform_cfg.h
  <%app_name_lc%>_msgids.h
  <%app_name_lc%>_msgid_values.h
)

generate_configfile_set(${<%app_name_uc%>_PLATFORM_CONFIG_FILE_LIST})
