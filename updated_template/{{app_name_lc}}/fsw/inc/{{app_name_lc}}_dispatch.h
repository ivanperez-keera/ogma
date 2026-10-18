#ifndef {{app_name_uc}}_DISPATCH_H
#define {{app_name_uc}}_DISPATCH_H

#include "cfe.h"

void {{app_name_uc}}_TaskPipe(const CFE_SB_Buffer_t *BufPtr);
void {{app_name_uc}}_ProcessGroundCommand(const CFE_SB_Buffer_t *BufPtr);

#endif /* {{app_name_uc}}_DISPATCH_H */
