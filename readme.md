A 4-bit stack-based toy VM with a graphical output, and associated assembler.  
Instructions are one 4-bit word. The destination and source of push and pop commands to the stack are controlled by the processor's mode, set by opcodes 0-9.
Opcodes A-F handle stack operations, program flow, and drawing to the graphical display.

## Opcodes

### PUSH/POP targets
| Opcode | NB Popped,Pushed | Name  | Description                  |
| ------ | ---------------- |:----  |:-----------                  |
| **0**  | 1,1              | COLOR | Color of drawn lines         |
| **1**  | 3,3              | X     | Memory position with a block |
| **2**  | 3,3              | Y     | Selected memory block        |
| **3**  | 4,4              | PC    | Program Counter position     |
| **4**  | 1,1              | MEM   | Read / set memory            |
| **5**  | 1,1              | IO    | POP outputs an nb to a file, etc, PUSH pushes user input            |
| **6**  | 1,1              | RSTK  | Return stack                                                        |
| **7**  | 1,1              | LIT   | Load a number to the stack                                          |
| **8**  | 2,1              | ADD   | POP adds the top two stack positions, PUSH pushes the carry bit     |
| **9**  | 2,1              | SUB   | POP subs the top two stack positions, PUSH pushes the underflow bit |

### Commands
| Opcode | Stack effect | Name | Description |
| ------ | ------------:|:---- |:----------- |
| **A** | X  | PUSH | Pushes X nb to the stack based on the current mode |
| **B** | X  | POP | Pops X nb from the stack, may push some back, based on the current mode |
| **C** | +1 | PEEK | Takes the next program value N as an argument, copies the N'th stack item to the top of the stack |
| **D** | -5 | COND | Moves the PC to the address described in the top 4 stack positions if the 5th stack item is 0 |
| **E** | -1 | NOR | Pops the top two stack items and pushes their binary NOR. 
| **F** | -1 | MOVE | Moves the pen to the current X and Y positions, drawing a line if the top stack position is not 0 |

