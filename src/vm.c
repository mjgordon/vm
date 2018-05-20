#include <stdint.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "stack.h"
#include "visualizer.h"

#include "vm.h"

#define OP_COLOR    0x0
#define OP_X        0x1
#define OP_Y        0x2
#define OP_PC       0x3
#define OP_MEM      0x4
#define OP_IO       0x5
#define OP_RSTK     0x6
#define OP_LIT      0x7
#define OP_ADD      0x8
#define OP_SUB      0x9
#define OP_PUSH     0xA
#define OP_POP      0xB
#define OP_PEEK     0xC
#define OP_COND     0xD
#define OP_NOR      0xE
#define OP_MOVE     0xF

#define NB          16
#define PAGE_SIZE   1024
#define PAGE_COUNT  1024
#define MEMORY_SIZE PAGE_SIZE * PAGE_COUNT

uint8_t memory[MEMORY_SIZE];
uint8_t *program;
long programLength;

struct Stack* stack;
struct Stack* rstack;

enum mode {MODE_COLOR,MODE_X,MODE_Y,MODE_PC,MODE_MEM,MODE_IO,MODE_RSTK,MODE_LIT,MODE_ADD,MODE_SUB};

enum mode machineMode = MODE_IO;

uint16_t PC = 0;
uint8_t REG_COLOR;
uint16_t REG_X;
uint16_t REG_Y;
uint8_t FLAG;

uint16_t PEN_X = 0;
uint16_t PEN_Y = 0;

FILE *write_ptr;

uint16_t time = 0;

void setup() {
  setupSDL();
  FILE *fileptr;

  fileptr = fopen("compiler/program.hxb","rb");
  fseek(fileptr,0,SEEK_END);
  programLength = ftell(fileptr);
  rewind(fileptr);
  program = (uint8_t *)malloc((programLength)*sizeof(char));
  fread(program,1,programLength,fileptr);
  fclose(fileptr);
  
  stack = createStack(65536);
  rstack = createStack(65536);

}

void run() {
  while(PC < programLength) {
    uint8_t op = getNextOpcode();
    execute(op);
    time += 1;
    //putPixel32(context.surface,time % 1024 + 30, 10,colors[9]);
  }

  while(1) {
    randomize();
  }

  //  SDL_UpdateWindowSurface(context.win);
  //  SDL_RenderClear(context.ren);
  //  SDL_RenderCopy(context.ren,context.tex,NULL,NULL);
  //  SDL_RenderPresent(context.ren);

  getchar();
  cleanupSDL();
}

void randomize() {
  for (int i =0; i < 1000; i++) {
    int x = rand() % PAGE_SIZE;
    int y = rand() % PAGE_SIZE;
    int offset = (PAGE_SIZE * 3 * y) + (x * 3);
    pixels[offset + 0] = rand() % 256;
    pixels[offset + 1] = rand() % 256;
    pixels[offset + 2] = rand() % 256;
    pixels[offset + 3] = rand() % 256;

  }
  SDL_UpdateTexture(context.tex,NULL,&pixels[0],PAGE_SIZE * 3);
  SDL_RenderCopy(context.ren,context.tex,NULL,NULL);
  SDL_RenderPresent(context.ren);
}


void execute(uint8_t opcode) {

  switch(opcode) {
  case OP_COLOR:
    machineMode = MODE_COLOR;
    break;
  case OP_X:
    machineMode = MODE_X;
    break;
  case OP_Y:
    machineMode = MODE_Y;
    break;
  case OP_PC:
    machineMode = MODE_PC;
    break;
  case OP_MEM:
    machineMode = MODE_MEM;
    break;
  case OP_IO:
    machineMode = MODE_IO;
    break;
  case OP_RSTK:
    machineMode = MODE_RSTK;
    break;
  case OP_LIT:
    machineMode = MODE_LIT;
    break;
  case OP_ADD:
    machineMode = MODE_ADD;
    break;
  case OP_SUB:
    machineMode = MODE_SUB;
    break;
  case OP_PUSH:
    opPush();
    break;
  case OP_POP:
    opPop();
    break;
  case OP_PEEK:
    opPeek();
    break;
  case OP_COND:
    opCond();
    break;
  case OP_NOR:
    opNOR();
    break;
  case OP_MOVE:
    opMove();
    break;
  }
}

void opAdd() {
  uint8_t a = stackPop(stack);
  uint8_t b = stackPop(stack);
  uint8_t output = a + b;
  FLAG = output >= 0xF;
  output = output & 0xF;
  stackPush(stack,output);
}

void opSub() {
  uint8_t a = stackPop(stack);
  uint8_t b = stackPop(stack);
  uint8_t output = a - b;
  FLAG = output >= 0xF;
  output = output & 0xF;
  stackPush(stack,output);
}

void opPush() {
  switch(machineMode) {
  case MODE_COLOR:
    stackPush(stack,REG_COLOR);
    break;
    
  case MODE_X:
    stackPush(stack,extractNibble2(REG_X));
    stackPush(stack,extractNibble1(REG_X));
    stackPush(stack,extractNibble0(REG_X));	      
    break;
    
  case MODE_Y:
    stackPush(stack,extractNibble2(REG_Y));
    stackPush(stack,extractNibble1(REG_Y));
    stackPush(stack,extractNibble0(REG_Y));
    break;

  case MODE_PC:
    stackPush(stack,extractNibble3(PC));
    stackPush(stack,extractNibble2(PC));
    stackPush(stack,extractNibble1(PC));
    stackPush(stack,extractNibble0(PC));
    break;

  case MODE_MEM:
    stackPush(stack,memory[REG_Y * PAGE_SIZE + REG_X]);
    break;

  case MODE_IO:
    stackPush(stack,getInput());
    break;
    
  case MODE_RSTK:
    stackPush(stack,stackPop(rstack));
    break;

  case MODE_LIT:
    stackPush(stack,getNextOpcode());
    break;

  case MODE_ADD:
    stackPush(stack,FLAG);
    break;

  case MODE_SUB:
    stackPush(stack,FLAG);
    break;
  }
  
}

void opPop() {
  switch(machineMode) {
  case MODE_COLOR:
    REG_COLOR = stackPop(stack);
    break;

  case MODE_X:
    REG_X = popNibble3();
    break;

  case MODE_Y:
    REG_Y = popNibble3();
    break;

  case MODE_PC:
    PC = popNibble4();
    break;

  case MODE_MEM:
    memory[REG_Y * PAGE_SIZE + REG_X] = stackPop(stack);
    break;

  case MODE_IO:
    outputStack();
    break;

  case MODE_RSTK:
    stackPush(rstack,stackPop(stack));
    break;

  case MODE_LIT:
    execute(stackPop(stack));
    break;

  case MODE_ADD:
    opAdd();
    break;

  case MODE_SUB:
    opSub();
    break;
  }
}

void opPeek() {
  uint8_t depth = getNextOpcode();
  stackPush(stack,stackPeek(stack,depth));
}

void opCond() {
  uint16_t destination = popNibble4();
  uint8_t value = stackPop(stack);
  if (value == 0) {
    PC = destination;
  }
}

void opNOR() {
  uint8_t a = stackPop(stack);
  uint8_t b = stackPop(stack);
  uint8_t output = ~(a | b);

  output = output & 0xF;
  stackPush(stack,output);
}

void opMove() {
  uint16_t x1 = PEN_X;
  uint16_t x2 = REG_X;
  uint16_t y1 = PEN_Y;
  uint16_t y2 = REG_Y;
  if (REG_X < PEN_X) {
    uint16_t temp = x1;
    x1 = x2;
    x2 = temp;
  }
  double dx = x2 - x1;
  double dy = y2 - y1;
  double derr = abs(dy / dx);
  double error = 0;
  uint16_t y = 0;
  for (uint16_t x = x1; x1 <= x2; x ++) {
    memory[y * PAGE_SIZE + x] = REG_COLOR;
    error = error + derr;
    while (error >= 0.5) {
      y = y + (dy / abs(dy));
      error = error =- 1;
    }
  }
}



uint8_t getNextOpcode() {
  uint8_t opcode = program[PC];
  PC += 1;
  return(opcode);
}

uint8_t extractNibble0(uint16_t input) {
  return(input & 0xF);
}

uint8_t extractNibble1(uint16_t input) {
  return((input >> 4) & 0xF);
}

uint8_t extractNibble2(uint16_t input) {
  return((input >> 8) & 0xF);
}

uint8_t extractNibble3(uint16_t input) {
  return((input >> 12) & 0xF);
}

uint16_t popNibble3() {
  uint8_t nibble0 = stackPop(stack);
  uint8_t nibble1 = stackPop(stack);
  uint8_t nibble2 = stackPop(stack);

  uint16_t output = nibble2;
  output = output << 4;
  output += nibble1;
  output =  output << 4;
  output += nibble0;
  return(output);
}

uint16_t popNibble4() {
  uint8_t nibble0 = stackPop(stack);
  uint8_t nibble1 = stackPop(stack);
  uint8_t nibble2 = stackPop(stack);
  uint8_t nibble3 = stackPop(stack);

  uint16_t output = nibble3;
  output = output << 4;
  output += nibble2;
  output = output << 4;
  output += nibble1;
  output =  output << 4;
  output += nibble0;
  return(output);
}

uint8_t getInput() {
  //This is temporary
  uint8_t input;
  printf("Input Value:");
  scanf("%hhu",&input);
  return(input % 16);
}

void outputStack() {
  uint8_t output = stackPop(stack);
  fwrite(&output,1,1,write_ptr);
}


int main() {
  write_ptr = fopen("output.bin","wb+");
  
  setup();
  run();

  fclose(write_ptr);
}
