#include <stdint.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "stack.h"

#include "vm.h"

#define OP_COLOR    0x0
#define OP_X        0x1
#define OP_Y        0x2
#define OP_PC       0x3
#define OP_MEM      0x4
#define OP_IO       0x5
#define OP_DEPTH    0x6
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
#define PAGE_SIZE   NB * NB * NB
#define PAGE_COUNT  NB * NB * NB
#define MEMORY_SIZE PAGE_SIZE * PAGE_COUNT

uint8_t memory[MEMORY_SIZE];
uint8_t *program;
long programLength;
struct Stack* stack;

enum mode {MODE_COLOR,MODE_X,MODE_Y,MODE_PC,MODE_MEM,MODE_IO,MODE_DEPTH,MODE_LIT};

enum mode machineMode = MODE_IO;

uint16_t PC = 0;
uint8_t REG_COLOR;
uint16_t REG_X;
uint16_t REG_Y;
uint8_t REG_DEPTH;

void setup() {
  FILE *fileptr;

  fileptr = fopen("program.b","rb");
  fseek(fileptr,0,SEEK_END);
  programLength = ftell(fileptr);
  rewind(fileptr);
  program = (uint8_t *)malloc((programLength)*sizeof(char));
  fread(program,1,programLength,fileptr);
  fclose(fileptr);

  for (int i =0; i < programLength; i++) {
    printf("%i\n",program[i]);
  }
  
  stack = createStack(65535);
}

void run() {
  while(PC < programLength) {
    uint8_t op = getNextOpcode();

    switch(op) {
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
    case OP_DEPTH:
      machineMode = MODE_DEPTH;
      break;
    case OP_LIT:
      machineMode = MODE_LIT;
      break;
    case OP_ADD:
      opAdd();
      break;
    case OP_SUB:
      opSub();
      break;
    case OP_PUSH:
      opPush();
      break;
    case OP_POP:
      opPop();
      break;
    }

  }
}

void opAdd() {
  uint8_t a = stackPop(stack);
  uint8_t b = stackPop(stack);
  uint8_t output = (a + b) % 16;
  stackPush(stack,output);
}

void opSub() {
  uint8_t a = stackPop(stack);
  uint8_t b = stackPop(stack);
  uint8_t output = (uint8_t)(a - b) % 16;
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
    
  case MODE_DEPTH:
    stackPush(stack,REG_DEPTH);
    break;

  case MODE_LIT:
    stackPush(stack,getNextOpcode());
    break;
  }
  
}

void opPop() {
  switch(machineMode) {
  case MODE_COLOR:
    REG_COLOR = stackPop(stack);
    break;

  case MODE_X:

    break;
  }
}

uint8_t getNextOpcode() {
  uint8_t opcode = program[PC];
  PC += 1;
  printf("next: %i\n",opcode);
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
  output << 4;
  output += nibble1;
  output << 4;
  output += nibble0;
  
  
}

uint8_t getInput() {
  //This is temporary
  uint8_t input;
  printf("Input Value:");
  scanf("%hhu",&input);
  return(input % 16);
}


int main() {
  setup();
  run();
  printf("%u\n",stackPop(stack));
  printf("%u\n",stackPop(stack));
}
