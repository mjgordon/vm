// C program for array implementation of stack
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include "stack.h"


struct Stack* createStack(char* name,unsigned capacity) {
  struct Stack* stack = (struct Stack*) malloc(sizeof(struct Stack));
  stack->capacity = capacity;
  stack->top = -1;
  stack->array = (uint8_t*) malloc(stack->capacity * sizeof(char));
  stack->name = name;
  for (int i = 0; i < capacity; i++) {
    stack->array[i] = 255;
  }
  return stack;
}


int stackIsFull(struct Stack* stack) {
  return stack->top == stack->capacity - 1;
}


int stackIsEmpty(struct Stack* stack) {
  return stack->top == -1;
}


void stackPush(struct Stack* stack, uint8_t item) {
  if (stackIsFull(stack)) 
    return;
  stack->array[++stack->top] = item;
}


uint8_t stackPop(struct Stack* stack) {
  if (stackIsEmpty(stack)) {
    printf("%s : tried to pop empty stack\n",stack->name);
    return 0;
  }
  return stack->array[stack->top--];
}


uint8_t stackPeek(struct Stack* stack,int position) {
  if (stackIsEmpty(stack))
    return 0;
  return stack->array[stack->top - position];
}

/**
   Every position in stack is seeded with 255 to begin. Because legitimate values should never be more than 15, 
   We know that the first 255 we come across will be the point where top hasn't touched
   Thus, no extra work is taken during run time.
 */
int stackGetMaxDepth(struct Stack* stack) {
  for (int i = 0; i < stack->capacity; i++) {
    if (stack->array[i] == 255) return(i);
  }
  return(-1);
}
