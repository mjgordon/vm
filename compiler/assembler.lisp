;;;;; assembler.lisp

(load "~/lisp/quicklisp/setup.lisp")
(load "assembler-dictionary")
(load "util-assembler")

(ql:quickload "split-sequence" :silent t)


;;; Global symbol lists

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


;;; Main assembly pipeline functions

(defun get-file (filename)
  "Load a hxa assembly file as a list of strings"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))


(defun generate-tables (lines)
  "Removes comments and blank lines, splits lines into one list of words. Converts to symbol tokens, 
while creating lists of labels and references."
  (clear-tables)
  (setf lines
	(mapcan (lambda (line)
		  (split-sequence:split-sequence #\Space line))
		(remove-lines lines)))

  (mapcar (lambda (word)
	    (let ((sym (intern word)) (ch0 (char word 0)))
	      (cond ((char= ch0 #\@) (insert-label-set sym))
		    ((char= ch0 #\>) (progn (insert-ref-table sym (intern (substitute #\@ #\> word)))
					    (insert-ref-set sym)))
		    ((char= ch0 #\+) (let ((offset 0))
				       (progn (setf sym (gensym))
					      (when (> (length word) 1)
						(setf offset (parse-integer (subseq word 1) :junk-allowed t)))
					      (insert-return-table sym offset))))
		    ((numberp (parse-integer word :junk-allowed t)) (setf sym (parse-integer word :junk-allowed t)))
		    (t sym))
	      sym))
	  lines))


(defun expand-tokens (tokens &optional (expander (get-dictionary-expander)) (depth 1))
  "Recursively expands folded opcodes into their normal forms"
  (let ((expansion (expand-pass tokens expander)))
    (if (car expansion)
	(expand-tokens (rest expansion) expander (incf depth))
	(progn
	  (format t "Expansion took ~s passes~%" depth)
	  (rest expansion)))))


(defun strip-redundant-modes (tokens)
  "Removes mode-change opcodes that will have no effect, which may be present for readability or expansion completeness"
  (let ((mode-tokens '(COLOR X Y PC MEM IO RSTK LIT ADD SUB))
	(strip-count 0)
	(current-token nil))
    (setf tokens (remove-if (lambda (token)
			      (cond ((or (member token *label-set*) (member token *ref-set*))
				     (setf current-token nil))
				    ((member token mode-tokens)
				     (if current-token
					 (if (eq token current-token)
					     (incf strip-count)
					     (progn (setf current-token token) nil))
					 (progn (setf current-token token) nil)))))
			    tokens))
    (format t "~a redundant tokens stripped~%" strip-count))
  tokens)
			   

(defun resolve-labels (tokens)
  "Steps through the file and determines final operation number for each label. Resolves references with these locations"
  (let ((count 0))
    (setf tokens
	  (remove-if (lambda (token)
		       (cond ((member token *label-set*) (progn (insert-label-table token count)
								 t))
			     ((gethash token *return-table*) (progn (incf (gethash token *return-table*) count)
								    (incf count 7)
								    nil))
			     ((member token *ref-set*) (progn (incf count 7)
							       nil))
			     (t (progn (incf count)
				       nil))))
		     tokens)))
  (mapcan (lambda (token)
	    (cond ((member token *ref-set*) (convert-address (gethash (gethash token *ref-table*) *label-table*)))
		  ((gethash token *return-table*) (convert-address (gethash token *return-table*)))
		  (t (list token))))
	  tokens))
		       

(defun write-bytecode (tokens &optional (filename "program.hxb"))
  "Converts list of opcode-tokens to list of associated bytes, and writes these to the output hxb binary file"
  (let ((bytecodes (get-bytecodes))
	(bytecode-counts (make-array 16)))
    (with-open-file (stream filename
			    :direction :output
			    :element-type 'unsigned-byte
			    :if-exists :supersede)
      (format t "Assembled ~a tokens~%" (length tokens))
      (mapcar (lambda (token)
		(when (symbolp token)
		    (let ((byte (gethash token bytecodes)))
		      (if byte
			  (setf token byte)
			  (format t "Bad token: ~a ~%" token))))
		(incf (aref bytecode-counts token))
		(write-byte token stream))
	      tokens))
    (with-open-file (stream "../heatmapUsage"
			    :direction :output
			    :element-type '(unsigned-byte 64)
			    :if-exists :supersede)
      (loop for n in (map 'list #'identity bytecode-counts) do
	   ;;(format t "~a~%" n)
	   (write-byte n stream)))))


(defun assemble-hex (filename)
  "Composites main assembly pipeline functions. Reports any feedback"
  (let ((output-filename (concatenate 'string (subseq filename 0 (search ".hxa" filename)) ".hxb")))
    (time (write-bytecode (resolve-labels (strip-redundant-modes (expand-tokens (generate-tables (get-file filename))))) output-filename)))
  nil)


;;; Only called when run from bash

(assemble-hex (cadr sb-ext:*posix-argv*))

