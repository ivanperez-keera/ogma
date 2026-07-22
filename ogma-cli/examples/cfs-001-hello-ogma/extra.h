#define SAMPLE_MID 0x1878

typedef struct sample_msg {
   CFE_MSG_CommandHeader_t CmdHeader;
   int32_t                 payload;
} sample_msg_t;
