(in-package :tests)


(defun test-macros-stack ()
  (with-hxa "test-macros-stack"
    (diag "Testing stack macros:")
    (mnemonic-test "Macro DROP" ((:nb 1 1)))
    (mnemonic-test "Macro DROP1" ((:nb 1 1)))
    (mnemonic-test "Macro DUP" ((:nb 1 1)))
    (mnemonic-test "Macro SWAP" ((:nb 1 2)))
    (mnemonic-test "Macro CONST" ((:int12 0) (:int16 0) (:int12 1) (:int16 1)))
    (mnemonic-test "Macro PEEK" ((:nb 0 2 1 0) (:nb 0 3 2 1 0)))))

(defun test-macros-logic ()
  (with-hxa "test-macros-logic"
    (diag "Testing logic macros:")
    (mnemonic-test "Macro AND" ((:nb 0 0 1 2 0 0 15)))
    (mnemonic-test "Macro OR" ((:nb 0 1 1 3 6 15 15)))
    (mnemonic-test "Macro NOT" ((:nb 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)))
    (mnemonic-test "Macro TRUE" ((:nb 15 15 15 0)))
    (mnemonic-test "Macro BIN" ((:nb 1 1 1 0)))
    (mnemonic-test "Macro ZERO2" ((:nb 0 1 1 1 1 1 1)))))


(defun test-macros ()
  ;(setf prove:*default-reporter* :fiveam)
  (test-macros-stack)
  (test-macros-logic))
