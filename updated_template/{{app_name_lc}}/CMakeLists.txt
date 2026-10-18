cmake_minimum_required(VERSION 2.6.4)
project(CFE_{{app_name_uc}} C)

set(APP_SRC_FILES
  fsw/src/{{app_name_lc}}.c
  fsw/src/{{app_name_lc}}_cmds.c
  fsw/src/copilot.c
)

if (CFE_EDS_ENABLED)
  list(APPEND APP_SRC_FILES
    fsw/src/{{app_name_lc}}_eds_dispatch.c
  )
else()
  list(APPEND APP_SRC_FILES
    fsw/src/{{app_name_lc}}_dispatch.c
  )
endif()

# Create the app module
add_cfe_app({{app_name_lc}} ${APP_SRC_FILES})

target_include_directories({{app_name_lc}} PUBLIC fsw/inc config)

{{#app_dependencies}}
add_cfe_app_dependency({{app_name}} {{.}})
{{/app_dependencies}}
