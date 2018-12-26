;;;;; assembler.lisp
(in-package :assembler)

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
	    (let ((sym (intern word 'opcodes)) (ch0 (char word 0)))
	      (cond ((char= ch0 #\@) (insert-label-set sym))
		    ((char= ch0 #\>) (progn (insert-ref-table sym (intern (substitute #\@ #\> word) 'opcodes))
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
  ;; If the top call, initialize the output map
  (when (= depth 1)
    (setf *output-map* (loop for i from 0 upto (- (length tokens) 1) collect i)))

  
  ;; Recursively call expand-tokens until no new expansions reported
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
	(current-token nil)
	(output-tokens ())
	(output-map ()))
    (mapcar (lambda (token map-item)
	      (unless (cond ((or (member token *label-set*) (member token *ref-set*))
			     (setf current-token nil))
			    ((member token mode-tokens)
			     (if (eq token current-token)
				 (incf strip-count)
				 (progn (setf current-token token) nil))))
		(setf output-tokens (cons token output-tokens))
		(setf output-map (cons map-item output-map))))
	    tokens *output-map*)
    (format t "~a redundant tokens stripped~%" strip-count)
    (setf *output-map* (reverse output-map))
    (reverse output-tokens)))
    
(defun resolve-labels (tokens)
  "Steps through the file and determines final operation number for each label. Resolves references with these locations"
  (let ((count 0)
	(tokens-new ())
	(map-new ()))
    (mapcar (lambda (token map-item)
	      (unless (cond ((member token *label-set*) (progn (insert-label-table token count)
							       t))
			    ((gethash token *return-table*) (progn (incf (gethash token *return-table*) count)
								   (incf count 7)
								   nil))
			    ((member token *ref-set*) (if (member (gethash token *ref-table*) *label-set*)
							  (progn (incf count 7)
								 nil)
							  (progn (format t "Bad Reference : ~a~%" token)
								 (setf *error-flag* 1)
								 t)))
			    (t (progn (incf count)
				      nil)))
		(setf tokens-new (cons token tokens-new))
		(setf map-new (cons map-item map-new))))
	    tokens *output-map*)
    (setf tokens (reverse tokens-new))
    (setf *output-map* (reverse map-new)))
  (let* ((map-new ())
	 (tokens-new (mapcan (lambda (token map-item)
			       (cond ((member token *ref-set*) (progn
								 (setf map-new (append map-new (make-list 7 :initial-element map-item)))
								 (convert-address (gethash (gethash token *ref-table*) *label-table*))))
				     ((gethash token *return-table*) (progn
								       (setf map-new (append map-new (make-list 7 :initial-element map-item)))
								       (convert-address (gethash token *return-table*))))
				     (t (progn
					  (setf map-new (append map-new (list map-item)))
					  (list token)))))
			     tokens *output-map*)))
    (setf *output-map* map-new)
    tokens-new))
    


(defun write-bytecode (tokens &optional (filename "program.hxb") (filename-heatmap "heatmap.hxo") (filename-mapping "mapping.hxo"))
  "Converts list of opcode-tokens to list of associated bytes, and writes these to the output hxb binary file"
  (let ((bytecodes (get-bytecodes))
	(bytecode-counts (make-array 16)))
    ;; Export the binary file
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
			  (progn
			    (format t "Bad token: ~a ~a  ~%" token (symbol-package token))
			    (setf token 0)
			    (setf *error-flag* 1)))))
		(incf (aref bytecode-counts token))
		(write-byte token stream))
	      tokens))
    ;; Export the mapping analysis
    (with-open-file (stream filename-mapping
			    :direction :output
			    :element-type '(unsigned-byte 32)
			    :if-exists :supersede)
      (loop for n in *output-map* do
	   (write-byte n stream)))

    ;; Export the heatmap analysis
    (with-open-file (stream filename-heatmap
			    :direction :output
			    :element-type '(unsigned-byte 64)
			    :if-exists :supersede)
      (loop for n in (map 'list #'identity bytecode-counts) do
	   (write-byte n stream)))))


(defun assemble-hex (filename)
  "Composites main assembly pipeline functions. Reports any feedback"
  (let* ((path-divisor (search "/" filename :from-end t))
	 (filename-stripped (subseq filename path-divisor (search ".hxa" filename)))
	 (filepath (subseq filename 0 path-divisor))
	 (output-filename (concatenate 'string filepath filename-stripped ".hxb"))
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
    (time (write-bytecode (resolve-labels (strip-redundant-modes (expand-tokens (generate-tables (get-file filename)))))
			  output-filename
			  output-heatmap
			  output-mapping)))
  *error-flag*)


