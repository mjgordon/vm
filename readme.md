TODO:

convert depth to Flag


# Opcodes

## 'Register' Selection
- 0 : 1 : COLOR
- 1 : 3 : X
- 2 : 3 : Y
- 3 : 4 : PC
- 4 : 1 : MEM
- 5 : 1 : IO
- 6 : 1 : FLAG (PUSH = Examine Flag) (POP = DROP) 
- 7 : 1 : LIT (PUSH = Add data)(POP = Execute)

## Commands
- 8 : +
- 9 : -
- A : PUSH
- B : POP
- C : PEEK
- D : COND
- E : NOR
- F : MOVE


# Folds

- *
- /
- %
- AND
- OR
- NOT
- DROP

# Need

- NOOP?
- 