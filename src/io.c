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
