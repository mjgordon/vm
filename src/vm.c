#include <stdint.h>
#include <math.h>
#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#include "io.h"
#include "stack.h"
#include "visualizer.h"

#include "vm.h"

#define OP_X        0x0
#define OP_Y        0x1
#define OP_PC       0x2
#define OP_MEM      0x3
#define OP_IO       0x4
#define OP_RSTK     0x5
#define OP_LIT      0x6
#define OP_ADD      0x7
#define OP_SUB      0x8
#define OP_PUSH     0x9
#define OP_POP      0xA
#define OP_PEEK     0xB
#define OP_COND     0xC
#define OP_NOR      0xD
#define OP_RSH      0xE
#define OP_LSH      0xF

#define NB          16
#define PAGE_SIZE   1024
#define PAGE_COUNT  1024
#define MEMORY_SIZE PAGE_SIZE * PAGE_COUNT

// Memory array. Divided implicitly into pages. Presented in a grid as an image
uint8_t memory[MEMORY_SIZE];

// Array of machine code for the loaded program
uint8_t *program;
uint16_t programLength;

// How many each opcode is executed
uint64_t heatmapOpcodes[16];

// How many times each position in the program is executed
uint64_t heatmapProgram[65536];

// Data stack and return stack
struct Stack* stack;
struct Stack* rstack;

// Processor modes define how PUSH and POP opcodes operate
enum mode {MODE_X,MODE_Y,MODE_PC,MODE_MEM,MODE_IO,MODE_RSTK,MODE_LIT,MODE_ADD,MODE_SUB};
enum mode machineMode = MODE_MEM;

// Program Counter
uint16_t PC = 0;

// Memory position of the machine
uint16_t REG_X;
uint16_t REG_Y;

// Combined carry and underflow flag for addition and subtraction operations
uint8_t FLAG;

// How many operations the VM has executed
unsigned long cycles = 0;

// Frequency in cycles for how often the 'hardware' checks for SDL events and updates the window
int updateFrequency = 1000000;

// Used for measuring the execution time of the program
unsigned long startTime;
unsigned long endTime;

SDL_Event event;

char* filename;
char* filenameOutput;
char* filenameHeatmapOpcodes;
char* filenameHeatmapProgram;

int main(int argc, char* argv[]) {
  int opt;
  while((opt = getopt(argc,argv,"spotf:u:")) != -1) {
    switch(opt) {
    case 's':
      flagUnrollStack = 1;
      break;
    case 'p':
      flagOutputPrint = 1;
      break;
    case 'o':
      flagOutputFile = 1;
      break;
    case 't':
      flagTest = 1;
      break;
    case 'f':
      filename = optarg;
      break;
    case 'u':
      updateFrequency = atoi(optarg);
      break;
    }
  }
  
  setup();
  startTime = getMillis();
  run();
  endTime = getMillis();
  finish();
}


/* setup
 * Loads program file, sets up stacks, visualizer, and output
 */
void setup() {
  filenameOutput = createOutputFilename(filename,"-output.hxo");
  filenameHeatmapOpcodes = createOutputFilename(filename,"-hmOpcodes.hxo");
  filenameHeatmapProgram = createOutputFilename(filename,"-hmProgram.hxo");
  initializeIO(filenameOutput);

  FILE *fileptr;
  if (!flagTest) printf("Opened program : %s\n",filename);
  fileptr = fopen(filename,"rb");
  fseek(fileptr,0,SEEK_END);
  programLength = ftell(fileptr);
  if (!flagTest) printf("Program length: %i\n",programLength);
  rewind(fileptr);
  program = (uint8_t *)malloc((programLength)*sizeof(char));
  if (fread(program,1,programLength,fileptr) == 0) {
    printf("Failed to read file");
    fclose(fileptr);
    exit(1);
  }
  fclose(fileptr);

  stack = createStack("data",65536);
  rstack = createStack("rstk",65536);

  setupSDL();

  for (int i = 0; i < 16; i++) {
    heatmapOpcodes[i] = 0;
  }

  for (int i = 0; i < 65535; i++) {
    heatmapProgram[i] = 0;
  }
  
}


/* run
 * Loops through the program and executes each opcode. Updates visuals intermittenly
 */
void run() {
  while(PC < programLength) {
    uint16_t currentPC = PC;
    uint8_t op = getNextOpcode();
    execute(op);
    cycles += 1;

    heatmapOpcodes[op] += 1;
    heatmapProgram[currentPC] += 1;

    
    if ((updateFrequency != -1) && cycles % updateFrequency == 0) {
        setPixels();
	updateSDL();
	if (SDL_PollEvent(&event)) {
	  if (event.type == SDL_QUIT) {
	    exit(1);
	  }
	}
    }
  }
  setPixels();
  updateSDL();

  saveScreen();
}


/* finish
 * Reports analysis of program run. Closes files and cleans up SDL components
 */
void finish() {
  writeArray(filenameHeatmapOpcodes,heatmapOpcodes,16);
  writeArray(filenameHeatmapProgram,heatmapProgram,programLength);
  finishIO();
  free(filenameOutput);
  free(filenameHeatmapOpcodes);
  free(filenameHeatmapProgram);
  // If being run normally (not from an automatic testing script)
  if (! flagTest) {
    printf("=== RESULTS ===\n");
    printf("Execution took %lu ms\n",endTime - startTime);
    printf("%li operations\n",cycles);
    printf("PC: %i\n",PC);
    printf("Data stack max depth: %i\n",stackGetMaxDepth(stack));
    printf("Return stack max depth: %i\n",stackGetMaxDepth(rstack));
  

    // Report on data stack
    printf("Data stack final depth: %i\n",(stack -> top) + 1);
    if (flagUnrollStack && !stackIsEmpty(stack)) {
      printf(" Unrolling data stack:\n");
      while(!stackIsEmpty(stack)) {
	printf(" %i",stackPop(stack));
      }
      printf("\n");
    }
    
    // Report on return stack
    printf("Return stack final depth: %i\n",(rstack -> top) + 1);
    if (flagUnrollStack && !stackIsEmpty(rstack)) {
      printf(" Unrolling return stack:\n");
      while(!stackIsEmpty(rstack)) {
	printf(" %i",stackPop(rstack));
      }
      printf("\n");
    }
  }
  // Testing script, only print data stack (should only contain return value of main function)
  else {
    while(!stackIsEmpty(stack)) {
      printf(" %i",stackPop(stack));
    }
    printf("\n");
  }
  // Loop forever after execution is complete; respond to quit events and redraw the window as needed
  // Don't loop if in test mode
  if (!flagTest) {
    while (1) {
      if (SDL_WaitEvent(&event)) {
	if (event.type == SDL_QUIT) {
	  break;
	}
	else if (event.type == SDL_WINDOWEVENT) {
	  if (event.window.event == SDL_WINDOWEVENT_MOVED) {
	    updateSDL();
	  }
	}
      }
    }
  }
  cleanupSDL();
}


/* execute
 * Calls function associated with each opcode
 */
void execute(uint8_t opcode) {

  switch(opcode) {
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
  case OP_RSH:
    execRSH();
    break;
  case OP_LSH:
    execLSH();
    break;
  }
}


/* execAdd
 * Opcode ADD : Pops and adds the top two nb on the data stack, pushes the result. Sets the FLAG if overflows
 */
void execAdd() {
  uint8_t a = stackPop(stack);
  uint8_t b = stackPop(stack);
  uint8_t output = a + b;
  FLAG = output > 0xF;
  output = output & 0xF;
  stackPush(stack,output);
}


/* execSub
 * Opcode SUB : Pops and subtracts the top two nb on the data stack, pushes the result. Sets the FLAG if undeflows
 */
void execSub() {
  uint8_t a = stackPop(stack);
  uint8_t b = stackPop(stack);
  uint8_t output = b - a;
  FLAG = output > 0xF;
  output = output & 0xF;
  stackPush(stack,output);
}


/* execPush
 * Opcode PUSH : Pushes one or more nb to the data stack depending on the machineMode
 */
void execPush() {
  switch(machineMode) {
    
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


/* execPop
 * Opcode POP : Pops one or more nb from the data stack depending on the machineMode
 */
void execPop() {
  uint16_t temp = 0;
  switch(machineMode) {

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


/* execPeek
 * Opcode PEEK : Copies the second item on the stack to the top of the stack
 */
void execPeek() {
  stackPush(stack,stackPeek(stack,1));
}


/* execCond
 * Opcode COND : Jumps the PC to the requestion position if the next value on the data stack is 0
 */
void execCond() {
  uint16_t destination = popNibble4();
  uint8_t value = stackPop(stack);
  if (value == 0) {
    PC = destination;
  }
}


/* execNOR
 * Opcode NOR : Pops and NORs the top two nb on the data stack, pushes the result
 */
void execNOR() {
  uint8_t a = stackPop(stack);
  uint8_t b = stackPop(stack);
  uint8_t output = ~(a | b);

  output = output & 0xF;
  stackPush(stack,output);
}


/* execRSH
 * Opcode RSH : Binary shift the top of the stack one place to the right
 * Currently does not set flags if bits wrap, will need to test
 */

void execRSH() {
  uint8_t value = stackPop(stack);
  value = value >> 1;
  value = value & 0xF;
  stackPush(stack,value);
}


/* exec LSH
 * Opcode LSH : Binary shift the top of the stack one place to the left
 */

void execLSH() {
  uint8_t value = stackPop(stack);
  value = value << 1;
  value = value & 0xF;
  stackPush(stack,value);
}


/* getNextOpcode
 * Returns the next opcode in the program, advances the PC counter
 */
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


/* getInput
 * Returns one nb of data from user input
 */
uint8_t getInput() {
  uint8_t input = 0;
  printf("Input 1 hex digit: ");
  //scanf("%hhu",&input);
  char st[1024];
  if (  fgets( st, sizeof(st), stdin )) {
    sscanf(st, "%hhu", &input );
  }
  return(input & 0xF);
}


/* setPixels
 * Updates the SDL pixel array from the memory array
 */
void setPixels() {
  for (int i = 0; i < 1024 * 1024; i++) {
    int offset = i * 4;
    pixels[offset + 0] = colors[memory[i]].channels[0];
    pixels[offset + 1] = colors[memory[i]].channels[1];
    pixels[offset + 2] = colors[memory[i]].channels[2];
    pixels[offset + 3] = colors[memory[i]].channels[3];
  }
}


/* getMillis
 * Returns the current time in millis
 */
unsigned long getMillis() {
  struct timeval tv;
  gettimeofday(&tv,NULL);
  return (tv.tv_sec * 1000) + (tv.tv_usec / 1000);
}
