#ifndef IO
#define IO

extern int flagUnroll;
extern int flagOutputPrint;
extern int flagOutputFile;

typedef struct {
  uint8_t nbs[8];
  uint8_t position;
  uint8_t size;
} outputBuffer;

outputBuffer newOutputBuffer(uint8_t size);
void initializeIO();
void finishIO();
void output(uint8_t nb);
int getInt(uint8_t nb,outputBuffer *buffer,int *value);

#endif
