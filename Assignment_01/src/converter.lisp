(defun read_file (filename)											;; read file recursively
  (format t "~%Reading from file: ~A~%" filename)
  (with-open-file (stream filename :if-does-not-exist nil)
    (when stream
      (labels ((read-lines-recursively (stream)
                 (let ((line (read-line stream nil nil)))
                   (if line
                       (cons line (read-lines-recursively stream))
                       nil))))
        (read-lines-recursively stream)))))

(defun write_file (filename lines)										;; write to file recursively
  (format t "~%Writing to file: ~A~%" filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (labels ((write-lines-recursively (lines)
               (when lines
                 (write-line (first lines) stream)
                 (write-lines-recursively (rest lines)))))
      (write-lines-recursively lines))))

(defun line_type (line)
  (cond
    ((search "if" line) 'if)											;; if statement
    ((search "for" line) 'for)											;; for statement
    ((search "while" line) 'while)										;; while statement
    ((search "return" line) 'return)										;; return statement
    ((or (search "++" line) (search "--" line)) 'increment_decrement)						;; Increment or decrement
    ((or (search "&&" line) (search "||" line)) 'logical_operation)						;; Logical operations
    ((and (search "=" line) (not (search "int" line))) 'assignment)						;; Assignment without declaration
    ((search "printf" line) 'print)										;; Print statement
    ((and (search "int" line) (search "=" line)) 'declaration)							;; Variable declaration with or without function call
    ((and (search "int" line) (search "(" line) (search ")" line) (search ";" line)) 'function_prototype)	;; Function prototype
    ((and (search "int" line) (search "(" line) (search ")" line)) 'function_definition)			;; Function definition
    (t 'unknown)))

(defun conversion_foo (line_type)
  (cond
    ((eq line_type 'function_prototype) 'convert_function_prototype)	;; For Function prototype
    ((eq line_type 'function_definition) 'convert_function_definition)	;; For Function definition
    ((eq line_type 'if) 'convert_if)					;; For if
    ((eq line_type 'for) 'convert_for)					;; For "for" loop
    ((eq line_type 'while) 'convert_while)				;; For while loop
    ((eq line_type 'return) 'convert_return)				;; For return
    ((eq line_type 'declaration) 'convert_declaration)			;; For declarations
    ((eq line_type 'assignment) 'convert_assignment)			;; For assignments
    ((eq line_type 'print) 'convert_print)				;; For print
    ((eq line_type 'increment_decrement) 'convert_increment_decrement)	;; For ++ and --
    ((eq line_type 'logical_operation) 'convert_logical_operation)	;; For logical expressions
    (t 'convert_unknown)))

(defun convert (line)
  (let ((trimmed-line (string-trim '(#\Space #\Tab #\Newline) line)))
    (cond
      ((or (string= trimmed-line "") (string= trimmed-line "}"))
       " ")
      (t
       (let ((line_type (line_type line)))
         (funcall (conversion_foo line_type) line))))))


;;Conversion Functions

(defun convert_function_prototype (line)						;; To handle function prototypes > declaim
  (let* ((return-type (first (split_by_char #\space line)))
         (name-and-params (subseq line (+ (search " " line) 1) (search "(" line)))
         (name (string-trim '(#\Space) name-and-params))
         (params (subseq line (1+ (search "(" line)) (search ")" line)))
         (param-types (mapcar (lambda (param)
                                (let ((param-type (first (split_by_char #\space (string-trim '(#\Space #\Tab) param)))))
                                  (cond
                                   ((string= param-type "int") 'integer)
                                   ((string= param-type "double") 'double)
                                   ((string= param-type "float") 'float)
                                   (t 't))))
                              (split_by_char #\, params)))
         (lisp-return-type (cond
                            ((string= return-type "int") 'integer)
                            ((string= return-type "double") 'double)
                            ((string= return-type "float") 'float)
                            (t 't))))
    (string-downcase
     (format nil "(declaim (ftype (function (~{~a~^ ~}) ~a) ~a))"
             param-types lisp-return-type name))))

(defun convert_function_definition (line)						;; To handle functin definitions > defun
  (let* ((name-and-params (subseq line 0 (search "(" line)))
         (name (second (split_by_char #\space name-and-params)))
         (params (subseq line (1+ (search "(" line)) (search ")" line)))
         (param-names (if (string= params "")
                          '()
                          (mapcar (lambda (param)
                                    (second (split_by_char #\space (string-trim '(#\Space #\Tab) param))))
                                  (split_by_char #\, params)))))
    (format nil "(defun ~a (~{~a~^ ~})" name param-names)))

(defun convert_if (line inner_lines)							;; To handle if statements
  (let* ((condition (subseq line (1+ (search "(" line)) (search ")" line)))
         (formatted-condition (convert_condition condition))
         (converted_inner_lines (mapcar #'convert inner_lines))
         (inner-body (format nil "(progn~%    ~{~a~%    ~})" converted_inner_lines)))
    (format nil "(if ~a~%  ~a)" formatted-condition inner-body)))

(defun convert_for (line inner_lines)							;; To handle for loops
  (let* ((loop-body (subseq line (search "(" line) (search ")" line)))
         (parts (split_by_char #\; loop-body))
         (init (first parts))
         (condition (second parts))
         (increment (third parts))
         (var-name (second (split_by_char #\space init)))
         (start-value (string-trim '(#\Space #\Tab) (subseq init (1+ (search "=" init)))))
         (target-value (string-trim '(#\Space #\Tab) (subseq condition (1+ (search "<" condition)))))
         (converted_inner_lines (mapcar #'convert inner_lines))
         (inner-body (format nil "(progn~%    ~{~a~%    ~})" converted_inner_lines)))
    (format nil "(loop for ~a from ~a below ~a do~%  ~a)" var-name start-value target-value inner-body)))

(defun convert_while (line inner_lines)							;; To handle while loops
  (let* ((condition (subseq line (1+ (search "(" line)) (search ")" line)))
         (formatted-condition (convert_condition condition))
         (converted_inner_lines (mapcar #'convert inner_lines))
         (inner-body (format nil "(progn~%    ~{~a~%    ~})" converted_inner_lines)))
    (format nil "(loop while ~a do~%  ~a)" formatted-condition inner-body)))

(defun convert_return (line)								;; To handle return statements
  (let* ((return-value (string-trim '(#\Space #\Tab)
                                    (subseq line (+ (search "return" line) 7) (search ";" line))))
         (operator (find-if (lambda (op) (search op return-value)) '("+" "-" "*" "/"))))
    (if operator
        (let* ((parts (split_by_char (char operator 0) return-value))
               (formatted-parts (mapcar (lambda (part) (string-trim '(#\Space #\Tab) part)) parts)))
          (format nil "  (~a ~a ~a))" operator (first formatted-parts) (second formatted-parts)))
        (format nil "  )~%~a)" return-value))))

(defun convert_declaration (line)							;; To handle declarations
  (let* ((trimmed-line (string-trim '(#\Space #\Tab) line))
         (normalized-line (replace_regexp_in_string " *= *" "=" trimmed-line))
         (parts (split_by_char #\= normalized-line))
         (var-part (first parts))
         (value-part (second parts))
         (var-name (second (split_by_char #\space var-part)))
         (cleaned-value (string-trim '(#\Space #\Tab #\;) value-part))
         (formatted-value (if (search "(" cleaned-value)
                            (format_function_call cleaned-value)
                            cleaned-value)))
    (format nil "(~a ~a)" var-name formatted-value)))

(defun convert_assignment (line)							;; To handle assignment > setf
  (let* ((parts (split_by_char #\= (string-trim '(#\Space #\Tab) line)))
         (var-name (string-trim '(#\Space #\Tab) (first parts)))
         (value (string-trim '(#\Space #\Tab #\;) (second parts)))
         (formatted-value (if (search "(" value)
                            (format_function_call value)
                            value)))
    (format nil "(setf ~a ~a)" var-name formatted-value)))

(defun convert_print (line)								;; To handle print > format
  (let* ((start (search "\"" line))
         (end (search "\"" line :start2 (1+ start)))
         (format-string (subseq line (1+ start) end))
         (args (string-trim " " (subseq line (+ 2 end) (1- (length line)))))
         (lisp-format-string (replace_c_format_specifiers format-string)))
    (if (string= args "")
        (format nil "(format t \"~a\")" lisp-format-string)
        (format nil "(format t \"~a\" ~a" lisp-format-string args))))

(defun convert_increment_decrement (line)									;; To handle (x++, x--) > incf decf
  (let ((var-name (string-trim '(#\Space #\Tab) (subseq line 0 (or (search "++" line) (search "--" line))))))
    (if (search "++" line)
        (format nil "(incf ~a)" var-name)
        (format nil "(decf ~a)" var-name))))

(defun convert_logical_operation (line)					;; To handle logical operations
  (let* ((formatted-line (replace_c_logical_operators line)))
    formatted-line))

(defun convert_unknown (line)						;; To handle unknown lines
  (format nil "  ;; unknown construct: ~a" line))


;;Helper Functions

(defun replace_c_format_specifiers (format-string)			;; Helper function for print to c specifiers to lisp specifiers
  (labels ((replace_recursively (str replacements)
             (if (null replacements)
                 str
                 (let* ((from (caar replacements))
                        (to (cdar replacements))
                        (pos (search from str)))
                   (if pos
                       (concatenate 'string
                                    (subseq str 0 pos)
                                    to
                                    (replace_recursively (subseq str (+ pos (length from))) replacements))
                       (replace_recursively str (cdr replacements)))))))
    (replace_recursively format-string '(("%d" . "~d")
                                         ("%s" . "~a")
                                         ("\\n" . "~%")))))

(defun replace_c_logical_operators (expr)						;; Helper function for logical operations
  (replace_regexp_in_string "&&" "and" (replace_regexp_in_string "||" "or" expr)))

(defun replace_regexp_in_string (regexp replacement string)				;; Helper function regular expressions
  (labels ((replace_recursively (str start)
             (let ((pos (search regexp str :start2 start)))
               (if pos
                   (concatenate 'string
                                (subseq str 0 pos)
                                replacement
                                (replace_recursively str (+ pos (length regexp))))
                   str))))
    (replace_recursively string 0)))

(defun convert_condition (condition)							;; Helper function for if and while functions infix to prefix
  (let* ((operators '(">" "<" ">=" "<=" "==" "!="))
         (operator (find-if (lambda (op) (search op condition)) operators)))
    (if operator
        (let* ((parts (split_by_char (char operator 0) condition))
               (left (string-trim '(#\Space #\Tab) (first parts)))
               (right (string-trim '(#\Space #\Tab) (second parts))))
          (format nil "(~a ~a ~a)" operator left right))
        condition)))

(defun convert_declaration_for_let (line)						;; Helper function for declaration line to a let binding form
  (let* ((trimmed-line (string-trim '(#\Space #\Tab) line))
         (normalized-line (replace_regexp_in_string " *= *" "=" trimmed-line))
         (parts (split_by_char #\= normalized-line))
         (var-part (first parts))
         (value-part (second parts))
         (var-name (second (split_by_char #\space var-part)))
         (cleaned-value (string-trim '(#\Space #\Tab #\;) value-part))
         (formatted-value (if (search "(" cleaned-value)
                            (format_function_call cleaned-value)
                            cleaned-value)))
    (list var-name formatted-value)))

(defun format_function_call (expr)						;; Format function calls by separating the function name and arguments
  (let* ((func_name_end (search "(" expr))
         (func_name (string-trim '(#\Space #\Tab) (subseq expr 0 func_name_end)))
         (args_str (string-trim '(#\Space #\Tab #\)) 
                               (subseq expr (1+ func_name_end))))
         (args (split_by_char #\, args_str))
         (formatted_args (mapcar (lambda (arg) 
                                 (string-trim '(#\Space #\Tab) arg))
                               args)))
    (format nil "(~a ~{~a~^ ~})" func_name formatted_args))

(defun format_let_block (declarations)						;; Format multiple declarations as a let block
  (format nil "(let (~{    (~{~a~^ ~})~^~%~})" declarations))

(defun split_by_char (char str)							;; Splits strings by a given character
  (labels ((split_by_char_rec (start)
             (let ((pos (position char str :start start)))
               (if pos
                   (cons (subseq str start pos)
                         (split_by_char_rec (1+ pos)))
                   (list (subseq str start))))))
    (split_by_char_rec 0)))

(defun c_to_lisp_recursive (lines)									;; Recursively convert C lines to Lisp.
  (when lines
    (let ((line (first lines))
          (rest_lines (rest lines)))
      (cond
        ((and (search "int" line) (search "(" line) (not (search ";" line)))				;; For function definition
         (cons (convert_function_definition line) (c_to_lisp_recursive rest_lines)))
         
        ((and (search "for" line) (search "{" line))							;; For "for" loop
         (multiple-value-bind (inner_lines remaining-lines) (collect_inner_lines rest_lines)
           (cons (convert_for line inner_lines) (c_to_lisp_recursive remaining-lines))))

        ((and (search "if" line) (search "{" line))							;; For if block
         (multiple-value-bind (inner_lines remaining-lines) (collect_inner_lines rest_lines)
           (cons (convert_if line inner_lines) (c_to_lisp_recursive remaining-lines))))

        ((and (search "int" line) (search "=" line))							;; For let block by collecting consecutive variable declarations
         (multiple-value-bind (declarations remaining-lines) (collect_declarations lines)
           (cons (format_let_block declarations) (c_to_lisp_recursive remaining-lines))))

        ((and (search "while" line) (search "{" line))							;; For while block
         (multiple-value-bind (inner_lines remaining-lines) (collect_inner_lines rest_lines)
           (cons (convert_while line inner_lines) (c_to_lisp_recursive remaining-lines))))

        (t (cons (convert line) (c_to_lisp_recursive rest_lines)))))))					;; For other or unknown lines

(defun collect_inner_lines (lines)					;; Collects lines within a code block 
  (if (null lines)
      (values nil nil)
      (let ((line (first lines))
            (rest_lines (rest lines)))
        (if (search "}" line)
            (values '() rest_lines)  ; End of block found
            (multiple-value-bind (inner_lines remaining-lines) (collect_inner_lines rest_lines)
              (values (cons line inner_lines) remaining-lines))))))

(defun collect_declarations (lines)					;; Collects consecutive variable declarations to format them into a single let block
  (labels ((collect_rec (lines accum)
             (if (or (null lines)
                     (not (and (search "int" (first lines))
                              (search "=" (first lines)))))
                 (values (nreverse accum) lines)
                 (let ((declaration (convert_declaration_for_let (first lines))))
                   (collect_rec (rest lines) (cons declaration accum))))))
    (collect_rec lines nil)))

(defun c_to_lisp (input_file output_file)				;; Convert C to Lisp
  (let ((lines (read_file input_file)))
    (if lines
        (let ((converted-lines (c_to_lisp_recursive lines)))
          (write_file output_file converted-lines)
          (format t "~%Conversion completed successfully~%"))
        (format t "~%[DEBUG] No lines read from input file~%"))))

(let ((args ext:*args*))						;; collect command-line arguments.
  (if (= (length args) 2)
      (c_to_lisp (first args) (second args))
      (format t "Usage: clisp converter.lisp input.c output.lisp~%")))


