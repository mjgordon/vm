(in-package :assembler)

;; Set of all label symbols used in the file				       
(defparameter *label-set* ())

;; Hash table of label symbols to their final operation number in the file
(defparameter *label-table* (make-hash-table :test 'eq))

;; Set of all label reference symbols used in the file
(defparameter *ref-set* ())

;; Hash table of label reference symbols to the label symbols they point to
(defparameter *ref-table* (make-hash-table :test 'eq))

;; Hash table of the gensym in each call expansion to its final operation numbe + offset in the file
(defparameter *return-table* (make-hash-table :test 'eq))

;; A mapping between the output file and the input file. A list of token numbers for each source token in the input.
(defparameter *output-map* ())

;; Tracks errors in assembly
(defparameter *error-flag* 0)

;; Whether to print messages during assembly
(defparameter *verbose-assembly* t)
