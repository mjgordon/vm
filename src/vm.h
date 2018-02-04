#ifndef VM
#define VM

void setup();
void run();

void opAdd();
void opSub();
void opPush();
void opPop();

uint8_t getNextOpcode();

uint8_t extractNibble0(uint16_t input);
uint8_t extractNibble1(uint16_t input);
uint8_t extractNibble2(uint16_t input);
uint8_t extractNibble3(uint16_t input);

uint16_t popNibble3();

uint8_t getInput();

int main();

#endif
