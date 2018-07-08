#ifndef IO
#define IO

extern int flagUnrollStack;
extern int flagOutputPrint;
extern int flagOutputFile;

typedef struct {
  uint8_t nbs[8];
  uint8_t position;
  uint8_t size;
} outputBuffer;

void initializeIO(char* filenameOutput);
void finishIO();
void output(uint8_t nb);
int getInt(uint8_t nb,outputBuffer *buffer,int *value);
outputBuffer newOutputBuffer(uint8_t size);

void writeArray(char* name, uint64_t* data, int size);

char* createOutputFilename(char* filename, char* suffix);
char* remove_ext (char* mystr, char dot, char sep);
char* concat(const char *s1, const char *s2);

#endif
