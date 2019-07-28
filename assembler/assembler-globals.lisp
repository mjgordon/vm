(in-package :assembler)

(defparameter *label-set* ()
  "Set of all label symbols used in the file")


(defparameter *label-table* (make-hash-table :test 'eq)
  "Hash table of label symbols to their final operation number in the file")


(defparameter *ref-set* ()
  "Set of all label reference symbols used in the file")


(defparameter *ref-table* (make-hash-table :test 'eq)
  "Hash table of label reference symbols to the label symbols they point to")


(defparameter *return-table* (make-hash-table :test 'eq)
  "Hash table of the gensym in each call expansion to its final operation number + offset in the file")


(defparameter *error-flag* 0
  "Tracks errors in assembly")


(defparameter *verbose-assembly* t
  "Whether to print messages during assembly")
