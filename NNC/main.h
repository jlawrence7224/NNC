
#include <stdio.h>

// Command line data structures
extern char book_path[128];
extern char log_path[128];
extern char tb_path[128];
extern char rc_path[128];
extern char cmd_buffer[4096];
extern char *args[512];
extern char buffer[4096];
extern int line_length;
extern unsigned char convert_buff[8];
extern int done;

void Print(int, char *, ...);
void AlignedMalloc(void **pointer, int alignment, size_t size);

FILE* input_stream;
