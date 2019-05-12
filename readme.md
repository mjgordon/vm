A 4-bit stack-based toy VM with a graphical output, and associated assembler.  
Instructions are one 4-bit word. The destination and source of push and pop commands to the stack are controlled by the processor's mode, set by opcodes 0-9.
Opcodes A-F handle stack operations, program flow, and drawing to the graphical display.

## Opcodes

### PUSH/POP targets
| Opcode | NB Popped,Pushed | Name  | Description                  |
| ------ | ---------------- |:----  |:-----------                  |
| **0**  | 3,3              | X     | Memory position with a block |
| **1**  | 3,3              | Y     | Selected memory block        |
| **2**  | 4,4              | PC    | Program Counter position     |
| **3**  | 1,1              | MEM   | Read / set memory            |
| **4**  | 1,1              | IO    | POP outputs an nb to a file, etc, PUSH pushes user input            |
| **5**  | 1,1              | RSTK  | Return stack                                                        |
| **6**  | 1,1              | LIT   | Load a number to the stack, DROP the top of the stack if POP'd      |
| **7**  | 2,1              | ADD   | POP adds the top two stack positions, PUSH pushes the carry bit     |
| **8**  | 2,1              | SUB   | POP subs the top two stack positions, PUSH pushes the underflow bit |

### Commands
| Opcode | Stack Effect | Name | Description |
| ------ | ------------:|:---- |:----------- |
| **9** | X  | PUSH | Pushes X nb to the stack based on the current mode |
| **A** | X  | POP | Pops X nb from the stack, may push some back, based on the current mode |
| **B** | +1 | PEEK | Takes the next program value N as an argument, copies the N'th stack item to the top of the stack |
| **C** | -5 | COND | Moves the PC to the address described in the top 4 stack positions if the 5th stack item is 0 |
| **D** | -1 | NOR | Pops the top two stack items and pushes their binary NOR. |
| **E** | -1 | RSH | Performs a right bit shift on the top of the stack |
| **F** | -1 | LSH | Performs a left bit shift on the top of the stack |

