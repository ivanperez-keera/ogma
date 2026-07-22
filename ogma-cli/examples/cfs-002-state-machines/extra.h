#define STATE_MID  0x1878
#define SAMPLE_MID 0x1879

typedef struct state_msg {
   CFE_MSG_CommandHeader_t CmdHeader;
   uint8_t                 payload;
} state_msg_t;

typedef struct sample_msg {
   CFE_MSG_CommandHeader_t CmdHeader;
   int32_t                 payload;
} sample_msg_t;
