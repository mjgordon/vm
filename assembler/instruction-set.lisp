(cl:in-package :opcodes)

(cl:defun get-bytecodes-raw()
  "Returns a tree of the basic opcodes and their bytecode counterparts"
  '((OPCODES::X #x0)
    (OPCODES::Y #x1)
    (OPCODES::PC #x2)
    (OPCODES::MEM #x3)
    (OPCODES::IO #x4)
    (OPCODES::RSTK #x5)
    (OPCODES::LIT #x6)
    (OPCODES::ADD #x7)
    (OPCODES::SUB #x8)
    (OPCODES::PUSH #x9)
    (OPCODES::POP #xA)
    (OPCODES::PEEK #xB)
    (OPCODES::COND #xC)
    (OPCODES::NOR #xD)
    (OPCODES::RSH #xE)
    (OPCODES::LSH #xF)))


(cl:defun get-dictionary ()
  "Returns the raw data of the expansion dictionary. Gets closured into the dictionary expander function"
  ;; DEFINITIONS
  '(
    ;; STACK OPERATIONS
    (DROP
     (LIT POP))
    (DROP1
     (RSTK POP DROP RSTK PUSH))
    (DUP
     (LIT PUSH 0 PEEK DROP1))
    (SWAP
     (PEEK RSTK POP POP DROP RSTK PUSH PUSH))
    (SWAP_4_8
     (PEEK2 RSTK POP POP POP DROP RSTK PUSH PUSH PUSH))
    (SWAP_4_12
     (PEEK3 RSTK POP POP POP POP DROP RSTK PUSH PUSH PUSH PUSH))
    (CONST_12_0
     (LIT PUSH 0 PUSH 0 PUSH 0))
    (CONST_16_0
     (LIT PUSH 0 PUSH 0 PUSH 0 PUSH 0))
    (CONST_12_1
     (LIT PUSH 0 PUSH 0 PUSH 1))
    (CONST_16_1
     (LIT PUSH 0 PUSH 0 PUSH 0 PUSH 1))
    (PEEK2
     (RSTK POP PEEK RSTK PUSH SWAP))
    (PEEK3
     (RSTK POP POP PEEK RSTK PUSH SWAP RSTK PUSH SWAP))
    (PUSH_8
     (LIT PUSH)
     next-to-nb 2)
    (PUSH_12
     (LIT PUSH)
     next-to-nb 3)
    (PUSH_16
     (LIT PUSH)
     next-to-nb 4)
    ;; LOGIC
    (TRUE
     (LIT PUSH >0 COND
      LIT PUSH 15
      GOTO >1
      %0
      LIT PUSH 0
      %1)
     local-labels 2)
    (BIN
     (LIT PUSH >0 COND
      LIT PUSH 1
      GOTO >1
      %0
      LIT PUSH 0
      %1)
     local-labels 2)
    (AND
     (RSTK POP DUP NOR PUSH DUP NOR NOR))
    (OR
     (NOR DUP NOR))
    (NOT
     (DUP NOR))
    (ZERO_8
     (ADD POP PUSH POP))
    
    ;; MATH : COMPARISON
    (EQUAL_12
     (PEEK3 SUB POP TRUE NOT LIT PUSH >0 COND
      PEEK3 SUB POP TRUE NOT LIT PUSH >1 COND
      PEEK3 SUB POP TRUE NOT LIT PUSH >2 COND
      DROP DROP DROP LIT PUSH 15 GOTO >3
      %0 DROP %1 DROP %2 DROP DROP DROP LIT PUSH 0
      %3)
     local-labels 4)

    (CMP_LT_4_4
     (SUB POP DROP SUB PUSH))

    (CMP_GT_4_4
     (SWAP CMP_LT_4_4))

    (CMP_LTE_4_4
     (SUB POP PUSH RSTK POP LIT PUSH 1 SUB POP DROP SUB PUSH RSTK PUSH ADD POP))

    (CMP_GTE_4_4
     (SWAP CMP_LTE_4_4))

    (CMP_EQ_4_4
     (SUB POP LIT PUSH 1 SUB POP DROP SUB PUSH))

    (CMP_NEQ_4_4
     (CMP_EQ_4_4 TRUE NOT BIN))
    
    ;; MATH : ADDITION
    (ADDU_4_4
     (ADD POP RSTK POP ADD PUSH))
    (ADD_4_4
     (ADD POP RSTK POP ADD PUSH RSTK PUSH))
    (ADD_8_4
     (ADDU_4_4
      ADD POP RSTK PUSH))
    (ADD_8_8
     (SWAP RSTK POP ADD_8_4 RSTK PUSH SWAP RSTK POP ADD POP RSTK PUSH))
    (ADD_12_4
     (ADDU_4_4 ADDU_4_4
      ADD POP RSTK PUSH PUSH))
    (ADDC_12_1
     (LIT PUSH 1 ADD_12_4))
    (ADDC_16_1
     (LIT PUSH 1 ADDU_4_4 ADDU_4_4 ADDU_4_4
      ADD POP RSTK PUSH PUSH PUSH))

    ;; MATH : SUBTRACTION
    (SUB_4_4
     (SUB POP))
    (SUB_8_4
     (SUB POP RSTK POP SUB PUSH SUB POP RSTK PUSH))
    (SUB_8_8
     (SWAP RSTK POP SUB_8_4 RSTK PUSH SWAP SUB PUSH RSTK POP POP SUB POP RSTK PUSH LIT PUSH 0
      SUB PUSH RSTK PUSH ADD POP SUB POP DROP))
    (SUB_12_4
     (SUB POP RSTK POP SUB PUSH
      SUB POP RSTK POP SUB PUSH
      SUB POP RSTK PUSH PUSH))
    (SUB_12_12
     (PEEK2 RSTK POP POP POP DROP RSTK PUSH PUSH SUB_8_8
      RSTK PUSH PEEK2 PEEK2 RSTK POP POP POP DROP DROP SUB PUSH POP RSTK PUSH SUB POP RSTK PUSH PUSH))
    ;; MATH : MULTIPLICATION
    (MULT_4_4
     (RSTK POP POP LIT PUSH 0 PUSH 0
      %0 RSTK PUSH DUP RSTK PUSH DUP LIT PUSH >1 COND
      LIT PUSH 1 SUB POP RSTK POP POP ADD POP RSTK POP ADD PUSH POP RSTK PUSH GOTO >0
      %1 DROP DROP DROP)
     local-labels 2)
    (MULT_8_4
     (RSTK POP SWAP RSTK PUSH DUP RSTK POP MULT_4_4
      PEEK2 RSTK POP POP POP DROP RSTK PUSH PUSH PUSH PUSH MULT_4_4
      RSTK POP ADD POP
      RSTK POP ADD PUSH
      ADD POP RSTK PUSH PUSH))
    (MULT_8_8
     (RSTK POP PEEK2 PEEK2 RSTK POP POP MULT_8_4
      RSTK PUSH PUSH PUSH MULT_8_4
      RSTK POP POP SWAP RSTK POP ADD_8_4
      RSTK PUSH PUSH ADD_8_4
      RSTK PUSH))
    ;; MATH : DIVISION
    (DIV_4_4
     (DUP LIT PUSH >1 COND
      DUP RSTK POP POP POP LIT PUSH 0
      RSTK PUSH PUSH
      %0
      SUB POP RSTK POP SUB PUSH TRUE NOT LIT PUSH >2 COND
      LIT PUSH 1 ADD POP RSTK PUSH PUSH DUP RSTK POP GOTO >0
      %1
      DROP DROP LIT PUSH 0 PUSH 0 PUSH 1 GOTO >3
      %2
      RSTK PUSH PUSH DROP DROP LIT PUSH 1 PUSH 0
      %3
      SUB POP DROP)
     local-labels 4)
    (DIV_8_4
     (DUP LIT PUSH >1 COND
      DUP RSTK POP POP POP POP LIT PUSH 0 PUSH 0 RSTK PUSH PUSH PUSH
      %0
      SUB_8_4 RSTK POP POP SUB PUSH TRUE NOT LIT PUSH >2 COND
      LIT PUSH 1 ADD_8_4 RSTK PUSH PUSH PUSH DUP RSTK POP GOTO >0
      %1
      DROP DROP DROP LIT PUSH 0 PUSH 0 PUSH 0 PUSH 1 GOTO >3
      %2
      RSTK PUSH PUSH PUSH DROP DROP DROP LIT PUSH 1 PUSH 0
      %3
      SUB POP DROP)
     local-labels 4)
    (DIV_8_8
     (PEEK PEEK ZERO_8 LIT PUSH >1 COND
      PEEK PEEK RSTK POP POP POP POP POP POP LIT PUSH 0 PUSH 0 RSTK PUSH PUSH PUSH PUSH
      %0
      SUB_8_8 RSTK POP POP SUB PUSH TRUE NOT LIT PUSH >2 COND
      LIT PUSH 1 ADD_8_4 RSTK PUSH PUSH PUSH PUSH PEEK PEEK RSTK POP POP GOTO >0
      %1
      DROP DROP DROP DROP
      LIT PUSH 0 PUSH 0 PUSH 0 PUSH 1 GOTO >3
      %2
      RSTK PUSH PUSH PUSH PUSH DROP DROP DROP DROP LIT PUSH 1 PUSH 0
      %3
      SUB POP DROP)
     local-labels 4)
    (DIV_12_4
     (DUP LIT PUSH >1 COND
      DUP RSTK POP POP POP POP POP CONST_12_0 RSTK PUSH PUSH PUSH PUSH
      %0
      SUB_12_4 RSTK POP POP POP SUB PUSH TRUE NOT LIT PUSH >2 COND
      ADDC_12_1 RSTK PUSH PUSH PUSH PUSH DUP RSTK POP GOTO >0
      %1
      DROP DROP DROP DROP LIT PUSH 0 CONST_16_1 GOTO >3
      %2
      RSTK PUSH PUSH PUSH PUSH DROP DROP DROP DROP LIT PUSH 1 PUSH 0
      %3
      SUB POP DROP)
     local-labels 4)
    ;; MATH : MODULO
    (MOD
     (DUP LIT PUSH >1 COND
      DUP RSTK POP
      %0
      SUB POP SUB PUSH TRUE NOT LIT PUSH >2 COND
      RSTK PUSH DUP RSTK POP GOTO >0
      %1 RSTK POP
      %2 RSTK PUSH ADD POP)
     local-labels 3)

    (MOD_8_4
     (DUP LIT PUSH >1 COND
      DUP RSTK POP
      %0
      SUB_8_4 SUB PUSH TRUE NOT LIT PUSH >2 COND
      RSTK PUSH DUP RSTK POP GOTO >0
      %1 RSTK POP
      %2 RSTK PUSH ADD_8_4)
     local-labels 3)
    
    (MOD_8_8
     (PEEK PEEK ZERO_8 LIT PUSH >1 COND
      PEEK PEEK RSTK POP POP
      %0
      SUB_8_8 SUB PUSH TRUE NOT LIT PUSH >2 COND
      RSTK PUSH PUSH PEEK PEEK RSTK POP POP GOTO >0
      %1 RSTK POP POP
      %2 RSTK PUSH PUSH ADD_8_8)
     local-labels 3)
    (MOD_12_4
     (DUP LIT PUSH >1 COND
      DUP RSTK POP
      %0
      SUB_12_4 SUB PUSH TRUE NOT LIT PUSH >2 COND
      RSTK PUSH DUP RSTK POP GOTO >0
      %1 RSTK POP
      %2 RSTK PUSH ADD_12_4)
     local-labels 3)
    ;; MATH : NEGATION
    (NEG_4
     (NOT LIT PUSH 1 ADD POP))
    (NEG_8
     (RSTK POP NOT
      RSTK PUSH NOT
      LIT PUSH 1 ADD_8_4))
    (NEG_12
     (RSTK POP POP NOT
      RSTK PUSH NOT
      RSTK PUSH NOT
      ADDC_12_1))
    (NEG_16
     (RSTK POP POP POP NOT
      RSTK PUSH NOT
      RSTK PUSH NOT
      RSTK PUSH NOT
      ADDC_16_1))
    ;; PROGRAM FLOW
    (GOTO
     (LIT PUSH)
     next-token
     (PC POP))
    (CALL
     (LIT PUSH)
     offset-label 23
     (RSTK POP POP POP POP GOTO))	      
    (RET
     (RSTK PUSH PUSH PUSH PUSH PC POP))
    ;; IO
    (OUT
     (LIT PUSH 1 IO POP POP))
    (OUT_8
     (LIT PUSH 2 IO POP POP POP))
    (OUT_12
     (LIT PUSH 3 IO POP POP POP POP))
    (OUT_16
     (LIT PUSH 4 IO POP POP POP POP POP))
    ))
