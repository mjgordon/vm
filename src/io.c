#include <stdint.h>
#include <stdio.h>

struct {
  uint8_t nbA;
  uint8_t nbB;
  uint8_t position;
} outputBytesBuffer;

void outputBytes(uint8_t nb) {
  if (outputBytesBuffer.position == 0) {
    outputBytesBuffer.nbA = nb;
    outputBytesBuffer.position = 1;
  }
  else {
    outputBytesBuffer.nbB = nb;
    
    printf("Output Byte : %i : %i : %i\n",outputBytesBuffer.nbB,outputBytesBuffer.nbA,(outputBytesBuffer.nbB<<4) + outputBytesBuffer.nbA);
    
    outputBytesBuffer.nbA = 0;
    outputBytesBuffer.nbB = 0;
    outputBytesBuffer.position = 0;
  }
}

struct {
  uint8_t nbs[4];
  uint8_t position;
} outputInt4Buffer;

void outputInt4(uint8_t nb) {
  outputInt4Buffer.nbs[outputInt4Buffer.position] = nb;
  if (outputInt4Buffer.position == 3) {
    printf("Output int4 : %i \n",(outputInt4Buffer.nbs[3] << 12) + (outputInt4Buffer.nbs[2] <<8) + (outputInt4Buffer.nbs[1] << 4) + outputInt4Buffer.nbs[0]);
    outputInt4Buffer.position = 0;
  }
  else {
    outputInt4Buffer.position++;
  }
}
