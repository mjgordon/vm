(in-package :tests)


(defun test-macros-stack ()
  "Run tests on the macros for stack operations"
  (with-hxa "test-macros-stack"
    (diag "Testing stack macros:")
    (mnemonic-test "Macro DROP" ((:nb 1 1)))
    (mnemonic-test "Macro DROP1" ((:nb 1 1)))
    (mnemonic-test "Macro DUP" ((:nb 1 1)))
    (mnemonic-test "Macro SWAP" ((:nb 1 2)))
    (mnemonic-test "Macro CONST" ((:int12 0) (:int16 0) (:int12 1) (:int16 1)))
    (mnemonic-test "Macro PEEK" ((:nb 0 2 1 0) (:nb 0 3 2 1 0)))))

(defun test-macros-logic ()
  "Run tests on the macros for logic operations"
  (with-hxa "test-macros-logic"
    (diag "Testing logic macros:")
    (mnemonic-test "Macro AND" ((:nb 0 0 1 2 0 0 15)))
    (mnemonic-test "Macro OR" ((:nb 0 1 1 3 6 15 15)))
    (mnemonic-test "Macro NOT" ((:nb 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)))
    (mnemonic-test "Macro TRUE" ((:nb 15 15 15 0)))
    (mnemonic-test "Macro BIN" ((:nb 1 1 1 0)))
    (mnemonic-test "Macro ZERO2" ((:nb 0 1 1 1 1 1 1)))))

(defun test-macros-math ()
  "Run tests on the macros for math operations"
  (with-hxa "test-macros-math"
    (diag "Testing math macros:")
    (mnemonic-test "Macro EQUAL3:" ((:nb 15 15 15 0 0 15)))
    (mnemonic-test "Macro ADDC4_1:" ((:int16 1 2 4097 0)))
    (mnemonic-test "Macro SUB21:" ((:int8 31 30 26 2 15 255)))
    (mnemonic-test "Macro SUB22:" ((:int8 0) (:nb 0)
				   (:int8 15) (:nb 0)
				   (:int8 12) (:nb 0)
				   (:int8 16) (:nb 0)
				   (:int8 13) (:nb 0)
				   (:int8 254) (:nb 1)
				   (:int8 4) (:nb 1)
				   (:int8 5) (:nb 0)
				   (:int8 255) (:nb 1)
				   (:int8 128) (:nb 0)))
    (mnemonic-test "Macro SUB31:" ((:int12 0 4094 4095 2588 14)))
    (mnemonic-test "Macro SUB33:" ((:int12 15 14 0 4095 434)))
    (mnemonic-test "Macro MULT:" ((:int8 0 0 0 1 6 16 225)))
    (mnemonic-test "Macro MULT21 & MULT22:" ((:int16 0 0 0 1 6 30 30 225 512 11592 65025)))
    (mnemonic-test "Macro DIV:" ((:nb 15 1 4 2 1 0 0 0 0)))
    (mnemonic-test "Macro DIV21:" ((:int8 0 1 255 0 0 8 5 17 21)))
    (mnemonic-test "Macro DIV22:" ((:int8 0 0 1 15 240 5 1)))
    (mnemonic-test "Macro DIV31:" ((:int12 15 0 7 1 273 1365 46)))
    (mnemonic-test "Macro MOD:" ((:nb 15 0 1 0 3 0 3 1 7 6 5 4 3 2 1 0 4)))
    (mnemonic-test "Macro MOD21:" ((:int8 15 0 0 1 3 2 7 3)))
    (mnemonic-test "Macro MOD22:" ((:int8 0 1 14 3 15)))
    (mnemonic-test "Macro MOD31:" ((:int12 0 15 1 0 1 0)))))


(defun test-macros ()
  "Run all macro tests in this file"
  (setf prove:*default-reporter* :dot)
  (test-macros-stack)
  (test-macros-logic)
  (test-macros-math))
