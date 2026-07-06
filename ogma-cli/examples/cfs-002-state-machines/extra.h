#define STATE_MID  0x1878
#define SAMPLE_MID 0x1879

typedef struct state_msg {
   uint8_t CmdHeader[CFE_SB_CMD_HDR_SIZE];
   uint8_t payload;
} state_msg_t;

typedef struct sample_msg {
   uint8_t CmdHeader[CFE_SB_CMD_HDR_SIZE];
   int32_t payload;
} sample_msg_t;
