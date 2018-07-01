#include <stdint.h>
#include <math.h>
#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include "io.h"
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
uint16_t programLength;

struct Stack* stack;
struct Stack* rstack;

enum mode {MODE_COLOR,MODE_X,MODE_Y,MODE_PC,MODE_MEM,MODE_IO,MODE_RSTK,MODE_LIT,MODE_ADD,MODE_SUB};

enum mode machineMode = MODE_COLOR;

uint16_t PC = 0;
uint8_t REG_COLOR;
uint16_t REG_X;
uint16_t REG_Y;
uint8_t FLAG;

uint16_t PEN_X = 0;
uint16_t PEN_Y = 0;

long cycles = 0;

long startMillis;
long endMillis;

SDL_Event event;

char* filename;

int main(int argc, char* argv[]) {
  int opt;
  while((opt = getopt(argc,argv,"upof:")) != -1) {
    switch(opt) {
    case 'u':
      flagUnroll = 1;
      break;
    case 'p':
      flagOutputPrint = 1;
    case 'o':
      flagOutputFile = 1;
    case 'f':
      filename = optarg;
      break;
    }
  }

  setup();
  startMillis = getMillis();
  run();
  endMillis = getMillis();
  finish();
}


// Loads program file, sets up stacks, visualizer, and output
void setup() {
  FILE *fileptr;
  printf("Opened program : %s\n",filename);
  fileptr = fopen(filename,"rb");
  fseek(fileptr,0,SEEK_END);
  programLength = ftell(fileptr);
  printf("Program length: %i\n",programLength);
  rewind(fileptr);
  program = (uint8_t *)malloc((programLength)*sizeof(char));
  if (fread(program,1,programLength,fileptr) == 0) {
    printf("Failed to read file");
    fclose(fileptr);
    exit(1);
  }
  fclose(fileptr);

  initializeIO();
  
  stack = createStack("data",65536);
  rstack = createStack("rstk",65536);

  setupSDL();
}


// Loops through the program and executes each opcode. Updates visuals intermittenly
void run() {
  while(PC < programLength) {
    uint8_t op = getNextOpcode();
    execute(op);
    cycles += 1;

    if (cycles % 1000 == 0) {
   
    }
  }
  setPixels();
  updateSDL();
}


// Reports analysis of program run. Closes files and cleans up SDL components
void finish() {
  finishIO();
  printf("=== RESULTS ===\n");
  printf("Execution took %li ms\n",endMillis - startMillis);
  printf("%li operations\n",cycles);
  printf("PC: %i\n",PC);
  printf("Data stack max depth: %i\n",stackGetMaxDepth(stack));
  printf("Return stack max depth: %i\n",stackGetMaxDepth(rstack));
  
  printf("Data stack final depth: %i\n",(stack -> top) + 1);
  if (flagUnroll && !stackIsEmpty(stack)) {
    printf(" Unrolling data stack:\n");
    while(!stackIsEmpty(stack)) {
      printf(" %i",stackPop(stack));
    }
    printf("\n");
  }

  printf("Return stack final depth: %i\n",(rstack -> top) + 1);
  if (flagUnroll && !stackIsEmpty(rstack)) {
    printf(" Unrolling return stack:\n");
    while(!stackIsEmpty(rstack)) {
      printf(" %i",stackPop(rstack));
    }
    printf("\n");
  }
  
  while (1) {
    if (SDL_WaitEvent(&event)) {
      if (event.type == SDL_QUIT) {
	break;
      }
    }
  }

  cleanupSDL();
}


// Calls function associated with each opcode
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
    execPush();
    break;
  case OP_POP:
    execPop();
    break;
  case OP_PEEK:
    execPeek();
    break;
  case OP_COND:
    execCond();
    break;
  case OP_NOR:
    execNOR();
    break;
  case OP_MOVE:
    execMove();
    break;
  }
}


// Pops and adds the top two nb on the data stack, pushes the result. Sets the FLAG if overflows
void execAdd() {
  uint8_t a = stackPop(stack);
  uint8_t b = stackPop(stack);
  uint8_t output = a + b;
  FLAG = output > 0xF;
  output = output & 0xF;
  stackPush(stack,output);
}


// Pops and subtracts the top two nb on the data stack, pushes the result. Sets the FLAG if undeflows
void execSub() {
  uint8_t a = stackPop(stack);
  uint8_t b = stackPop(stack);
  uint8_t output = b - a;
  FLAG = output > 0xF;
  output = output & 0xF;
  stackPush(stack,output);
}


// Pushes one or more nb to the data stack depending on the machineMode
void execPush() {
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


// Pops one or more nb from the data stack depending on the machineMode
void execPop() {
  uint16_t temp = 0;
  switch(machineMode) {
  case MODE_COLOR:
    REG_COLOR = stackPop(stack);
    break;

  case MODE_X:
    temp = popNibble3();
    if (temp >= PAGE_SIZE) temp = PAGE_SIZE - 1;
    REG_X = temp;
    break;

  case MODE_Y:
    temp = popNibble3();
    if (temp >= PAGE_COUNT) temp = PAGE_COUNT - 1;
    REG_Y = temp;
    break;

  case MODE_PC:
    PC = popNibble4();
    break;

  case MODE_MEM:
    memory[REG_Y * PAGE_SIZE + REG_X] = stackPop(stack);
    break;

  case MODE_IO:
    output(stackPop(stack));
    break;

  case MODE_RSTK:
    stackPush(rstack,stackPop(stack));
    break;

  case MODE_LIT:
    stackPop(stack);
    break;

  case MODE_ADD:
    execAdd();
    break;

  case MODE_SUB:
    execSub();
    break;
  }
}


// Copies an nb of the requested depth to the top of the data stack
void execPeek() {
  uint8_t depth = getNextOpcode();
  stackPush(stack,stackPeek(stack,depth));
}


// Jumps the PC to the requestion position if the next value on the data stack is 0
void execCond() {
  uint16_t destination = popNibble4();
  uint8_t value = stackPop(stack);
  if (value == 0) {
    PC = destination;
  }
}


// Pops and NORs the top two nb on the data stack, pushes the result
void execNOR() {
  uint8_t a = stackPop(stack);
  uint8_t b = stackPop(stack);
  uint8_t output = ~(a | b);

  output = output & 0xF;
  stackPush(stack,output);
}


// Moves the pen on the display. If the top element is not zero, draws in COLOR on the memory display
void execMove() {
  
  uint16_t x1 = PEN_X;
  uint16_t y1 = PEN_Y;
  uint16_t x2 = REG_X;
  uint16_t y2 = REG_Y;

  PEN_X = REG_X;
  PEN_Y = REG_Y;
  
  uint8_t mode = stackPop(stack);
  if (mode == 0) return;

  if (abs(x2-x1) == 0) {
    plotVertical(x1,y1,y2);
  }
  if (abs(y2-y1) == 0) {
    plotHorizontal(x1,x2,y1);
  }
  else if (abs(y2-y1) < abs(x2-x1)) {
    if (x1 > x2) plotLineLow(x2,y2,x1,y1);
    else plotLineLow(x1,y1,x2,y2);
  }
  else {
    if (y1 > y2) plotLineHigh(x2,y2,x1,y1);
    else plotLineHigh(x1,y1,x2,y2);
  }
  
}


// Draws a vertical line on the memory display
void plotVertical(uint16_t x1, uint16_t y1, uint16_t y2) {
  if (y2 < y1) {
    uint16_t temp = y2;
    y2 = y1;
    y1 = temp;
  }

  for (uint16_t y = y1; y <= y2; y++) {
    memory[y * PAGE_SIZE + x1] = REG_COLOR;
  }
}


// Draws a horizontal line on the memory display
void plotHorizontal(uint16_t x1, uint16_t x2, uint16_t y1) {
  if (x2 < x1) {
    uint16_t temp = x2;
    x2 = x1;
    x1 = temp;
  }

  for (uint16_t x = x1; x <= x2; x++) {
    memory[y1 * PAGE_SIZE + x] = REG_COLOR;
  }
}


// Draws a breshenham line on the memory display
void plotLineLow(uint16_t x1,uint16_t y1,uint16_t x2,uint16_t y2) {
  int dx = x2 - x1;
  int dy = y2 - y1;
  uint16_t yi = 1;
  if (dy < 0) {
    yi = -1;
    dy = -dy;
  }
  int D = (2 * dy) - dx;
  uint16_t y = y1;

  for (uint16_t x = x1; x <= x2; x++) {
    memory[y * PAGE_SIZE + x] = REG_COLOR;
    if (D > 0) {
      y = y + yi;
      D = D - (2 * dx);
    }
    D = D + (2 * dy);
  }
}


// Draws a breshenham line on the memory display
void plotLineHigh(uint16_t x1,uint16_t y1,uint16_t x2,uint16_t y2) {
  int dx = x2 - x1;
  int dy = y2 - y1;
  uint16_t xi = 1;
  if (dx < 0) {
    xi = -1;
    dx = -dx;
  }
  int D = (2 * dx) - dy;
  uint16_t x = x1;

  for (uint16_t y = y1; y < y2; y++) {
    memory[y * PAGE_SIZE + x] = REG_COLOR;
    if (D > 0) {
      x = x + xi;
      D = D - (2 * dy);
    }
    D = D + (2 * dx);
  }
}


// Returns the next opcode in the program, advances the PC counter
uint8_t getNextOpcode() {
  uint8_t opcode = program[PC];
  PC += 1;
  return(opcode);
}


// Helper functions to extract a nb position from a 16 bit number

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


// Helper functions to push a 12 or 16 bit number as individual nb

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


// Returns one nb of data from user input
uint8_t getInput() {
  uint8_t input = 0;
  printf("Input Color Value: ");
  //scanf("%hhu",&input);
  char st[1024];
  if (  fgets( st, sizeof(st), stdin )) {
    sscanf(st, "%hhu", &input );
  }
  return(input & 0xF);
}


// Updates the SDL pixel array from the memory array
void setPixels() {
  for (int i = 0; i < 1024 * 1024; i++) {
    int offset = i * 4;
    pixels[offset + 0] = colors[memory[i]].channels[0];
    pixels[offset + 1] = colors[memory[i]].channels[1];
    pixels[offset + 2] = colors[memory[i]].channels[2];
    pixels[offset + 3] = colors[memory[i]].channels[3];
  }
}


// Get the current program run time in millis
long getMillis() {
  long ms;
  //  time_t s;
  struct timespec spec;
  clock_gettime(CLOCK_REALTIME, &spec);
  //s = spec.tv_sec;
  ms = round(spec.tv_nsec / 1.0e6);
  return ms;
}

