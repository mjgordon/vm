#ifndef VM
#define VM

void setup();
void run();

void randomize();

void execute(uint8_t opcode);

void opAdd();
void opSub();
void opPush();
void opPop();
void opPeek();
void opCond();
void opNOR();
void opMove();

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
