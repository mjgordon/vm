(in-package :tests)

(defun test-opcodes ()
  "Run tests on the basic opcodes"
  (with-hxa "test-opcodes"
    (diag "Testing Opcodes:")
    (mnemonic-test "Opcode PC" ((:nb 1)))
    (mnemonic-test "Opcode MEM" ((:nb 1 2)))
    (subtest "Opcode IO"
      (plan 1)
      (ok (get-result results))
      (prove:finalize))
    (mnemonic-test "Opcode ADD" ((:nb 5 0 0 1)))
    (mnemonic-test "Opcode SUB" ((:nb 2 0 0 0) (:nb 15 1 15 1 1 1 )))
    (mnemonic-test "Opcode PEEK" ((:nb 1 2 1 0)))
    (mnemonic-test "Opcode COND" ((:nb 1 1)))
    (mnemonic-test "Opcode NOR" ((:nb 15 0 12 3)))
    (mnemonic-test "Opcode RSH" ((:nb 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7)))
    (mnemonic-test "Opcode LSH" ((:nb 0 2 4 6 8 10 12 14 0 2 4 6 8 10 12 14)))))
  


