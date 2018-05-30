#ifndef VM
#define VM

void setup();
void run();

void setPixels();

void execute(uint8_t opcode);

void opAdd();
void opSub();
void opPush();
void opPop();
void opPeek();
void opCond();
void opNOR();
void opMove();

void plotVertical(uint16_t x1, uint16_t y1, uint16_t y2);
void plotHorizontal(uint16_t x1, uint16_t x2, uint16_t y1);
void plotLineLow(uint16_t x1, uint16_t y1, uint16_t x2, uint16_t y2);
void plotLineHigh(uint16_t x1, uint16_t y1, uint16_t x2, uint16_t y2);

uint8_t getNextOpcode();

uint8_t extractNibble0(uint16_t input);
uint8_t extractNibble1(uint16_t input);
uint8_t extractNibble2(uint16_t input);
uint8_t extractNibble3(uint16_t input);

uint16_t popNibble3();
uint16_t popNibble4();

uint8_t getInput();

void outputStack();

int main();

#endif
