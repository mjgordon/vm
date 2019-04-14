#ifndef VM
#define VM

int main();

void setup();
void run();
void finish();

void execute(uint8_t opcode);

void execAdd();
void execSub();
void execPush();
void execPop();
void execPeek();
void execCond();
void execNOR();
void execRSH();
void execLSH();

uint8_t getNextOpcode();

uint8_t extractNibble0(uint16_t input);
uint8_t extractNibble1(uint16_t input);
uint8_t extractNibble2(uint16_t input);
uint8_t extractNibble3(uint16_t input);

uint16_t popNibble3();
uint16_t popNibble4();

uint8_t getInput();

void setPixels();

unsigned long getMillis();

#endif
