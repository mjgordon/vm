#ifndef STACK
#define STACK

// A structure to represent a stack
struct Stack {
  int top;
  unsigned capacity;
  uint8_t* array;
};

struct Stack* createStack(unsigned capacity);
int stackIsFull(struct Stack* stack);
int stackIsEmpty(struct Stack* stack);
void stackPush(struct Stack* stack, uint8_t item);
uint8_t stackPop(struct Stack* stack);
uint8_t stackPeek(struct Stack* stack,int position);


#endif
