;;;;; assembler-main.lisp
(in-package :assembler)


;;; Main assembly pipeline functions

(defun get-file (filename)
  "Load a hxa assembly file as a list of strings"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))


(defun generate-tables (lines)
  "Removes comments and blank lines, splits lines into one list of words. Converts to tokens, 
while creating lists of labels and references."
  ;; Remove comments and blank lines, split lines
  (clear-tables)
  (setf lines
	(mapcan (lambda (line)
		  (split-sequence:split-sequence #\Space line))
		(remove-lines lines)))
  ;; Ignore trailing whitespace and other things that result in "" tokens
  (setf lines
	(remove-if (lambda (word) (string= word "")) lines))
  
  (mapcar (lambda (word id)
	    (let ((sym (intern word :opcodes))
		  (ch0 (char word 0)))
	      (cond ((char= ch0 #\@) (insert-label-set sym))
		    ((char= ch0 #\>) (progn (insert-ref-table sym)
					    (insert-ref-set sym)))
;;		    ((char= ch0 #\+) (let ((offset 0))
;;				       (progn (setf sym (gensym))
;;					      (when (> (length word) 1)
;;						(setf offset (parse-integer (subseq word 1) :junk-allowed t)))
;;					      (insert-return-table sym offset))))
		    ((numberp (parse-integer word :junk-allowed t)) (setf sym (parse-integer word :junk-allowed t))))
	      (make-token :value sym
			  :source-id id)))
	  lines
	  (iota (length lines))))


(defun expand-tokens (tokens &optional (depth 1))
  "Recursively expands folded opcodes into their normal forms"

  ;; Recursively call expand-tokens until no new expansions reported
  (let ((expansion (expand-pass tokens)))
    (if (car expansion)
	(expand-tokens (rest expansion) (incf depth))
	(progn
	  (when *verbose-assembly* (format t "Expansion took ~s passes~%" depth))
	  (rest expansion)))))


(defun strip-redundant-modes (tokens)
  "Removes mode-change opcodes that will have no effect, which may be present for readability or expansion completeness"  
  (let ((mode-ops '(X Y PC MEM IO RSTK LIT ADD SUB))
	(strip-count 0)
	(current-mode nil)
	(output-tokens ()))
    (mapcar (lambda (token)
	      (let ((val (token-value token)))
	      (unless (cond ((or (member val *label-set*) (member val *ref-set*))
			     (setf current-mode nil))
			    ((member val mode-ops)
			     (if (eq val current-mode)
				 (incf strip-count)
				 (progn (setf current-mode val) nil))))
		(setf output-tokens (cons token output-tokens)))))
	    tokens)
    (when *verbose-assembly* (format t "~a redundant tokens stripped~%" strip-count))

    (reverse output-tokens)))
    
(defun resolve-labels (tokens)
  "Steps through the file and determines final operation number for each label. Resolves references with these locations"
  (let ((count 0)
	(tokens-new ()))
    (mapcar (lambda (token)
	      (let ((val (token-value token))
		    (use-flag t))
		(cond
		  ;; Sets the label op-number
		  ((member val *label-set*)
		   (insert-label-table val count)
		   (setf use-flag nil))
		  ;; Adds the current op-number to the existing offset
		  ((gethash val *return-table*)
		   (incf (gethash val *return-table*) count)
		   (incf count 7))
		  ;; Checks for bad label references
		  ((member val *ref-set*)
		   (if (member (gethash val *ref-table*) *label-set*)
		       (incf count 7)
		       (progn (format t "Bad Reference : ~a~%" val)
			      (setf *error-flag* 1)
			      (setf use-flag nil))))
		  (t (incf count)))
		(when use-flag
		  (setf tokens-new (cons token tokens-new)))))
	    tokens)
    (setf tokens (reverse tokens-new)))

  ;; Expands references and call returns to their correct addresses
  (mapcan (lambda (token)
	    (let ((val (token-value token)))
	      (cond ((member val *ref-set*) (convert-address (gethash (gethash val *ref-table*) *label-table*)))
		    ((gethash val *return-table*) (convert-address (gethash val *return-table*)))
		    (t (list token)))))
	  tokens))
		     
		        


(defun write-bytecode (tokens &optional (filename "program.hxb") (filename-heatmap "heatmap.hxo") (filename-mapping "mapping.hxo"))
  "Converts list of opcode-tokens to list of associated bytes, and writes these to the output hxb binary file"
  (let ((bytecode-counts (make-array 16)))
    ;; Export the binary file
    (with-open-file (stream filename
			    :direction :output
			    :element-type 'unsigned-byte
			    :if-exists :supersede)
      (when *verbose-assembly* (format t "Assembled ~a tokens~%" (length tokens)))
      (mapcar (lambda (token)
		(let ((val (token-value token)))
		  (when (symbolp val)
		    (let ((byte (get-bytecode val)))
		      (if byte
			  (setf val byte)
			  (progn
			    (format t "Bad token: ~a ~a  ~%" val (symbol-package val))
			    (setf val 0)
			    (setf *error-flag* 1)))))
		(incf (aref bytecode-counts val))
		(write-byte val stream)))
	      tokens))
    ;; Export the mapping analysis
;;    (with-open-file (stream filename-mapping
;;			    :direction :output
;;			    :element-type '(unsigned-byte 32)
;;			    :if-exists :supersede)
;;      (loop for n in *output-map* do
;;	   (write-byte n stream)))

    ;; Export the heatmap analysis
    (with-open-file (stream filename-heatmap
			    :direction :output
			    :element-type '(unsigned-byte 64)
			    :if-exists :supersede)
      (loop for n in (map 'list #'identity bytecode-counts) do
	   (write-byte n stream)))))

(defun assemble-hex-internal (filename output-binary output-heatmap output-mapping)
  "Composites main assembly pipeline functions. Reports any feedback"
  (write-bytecode (resolve-labels (strip-redundant-modes (expand-tokens (generate-tables (get-file filename)))))
		  output-binary
		  output-heatmap
		  output-mapping))


(defun assemble-hex (filename &key (verbose t))
  "Sets up outputs and assembles differently based on arguments"
  (setf *verbose-assembly* verbose)
  (setf *error-flag* 0)
  (let* ((path-divisor (search "/" filename :from-end t))
	 (filename-stripped (subseq filename path-divisor (search ".hxa" filename)))
	 (filepath (subseq filename 0 path-divisor))
	 (output-binary (concatenate 'string filepath filename-stripped ".hxb"))
	 (output-heatmap (concatenate 'string
				      filepath
				      filename-stripped
				      "-analysis"
				      filename-stripped
				      "-hmBytecode.hxo"))
	 (output-mapping (concatenate 'string
				      filepath
				      filename-stripped
				      "-analysis"
				      filename-stripped
				      "-mapping.hxo")))
    (ensure-directories-exist (concatenate 'string filepath filename-stripped "-analysis/"))
    (if *verbose-assembly*
	(time (assemble-hex-internal filename output-binary output-heatmap output-mapping))
	(assemble-hex-internal filename output-binary output-heatmap output-mapping)))
  *error-flag*)


