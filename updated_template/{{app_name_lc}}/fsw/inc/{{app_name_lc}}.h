/*******************************************************************************
** File: {{app_name_lc}}.h
**
** Purpose:
**   This file is main hdr file for the {{app_name_uc}} application.
**
**
*******************************************************************************/

#ifndef {{app_name_uc}}_H
#define {{app_name_uc}}_H

/*
** Required header files.
*/
#include "cfe.h"
#include "{{app_name_lc}}_internal_cfg.h"
#include "{{app_name_lc}}_msg.h"

/************************************************************************
** Type Definitions
*************************************************************************/

/****************************************************************************/
/*
** Local function prototypes.
**
** Note: Except for the entry point ({{app_name_uc}}_AppMain), these
**       functions are not called from any other source module.
*/
void {{app_name_uc}}_AppMain(void);
CFE_Status_t {{app_name_uc}}_AppInit(void);
{{#msgHandlers}}
void {{APP_NAME}}_Process{{msgDataDesc}}(const CFE_SB_Buffer_t *BufPtr);
{{/msgHandlers}}
void {{app_name_uc}}_ResetCounters(void);

#endif /* {{app_name_uc}}_H */
