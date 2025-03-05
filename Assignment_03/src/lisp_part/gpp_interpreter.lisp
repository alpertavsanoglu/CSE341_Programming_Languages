(defvar keywords (list "and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "print" "true" "false"))
(defvar printkeywords (list "KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_PRINT" "KW_TRUE" "KW_FALSE"))
(defvar operators (list "+" "-" "/" "*" "(" ")" "," ";;"))
(defvar printoperators (list "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_OP" "OP_CP" "OP_COMMA" "COMMENT"))
(defvar ignored (list (string #\linefeed) "\t" " " "" (string #\return) "\r\n")) 
(defvar lines nil)						;; gettin from file and add to this array
(defvar splitted_word nil)
(defvar canpass nil)						;;for comment line(;;)
(defvar alt_f4 nil)						;;for exiting the program
(defvar lexer_list nil)
(defvar alt_ff4 nil)
(defvar literals nil)
(defvar result1 nil)
(defvar result2 nil)
(defvar resultnum nil)
(defvar resultden nil)
(defvar op_plus (list "OP_OP" "OP_PLUS" "VALUEF" "VALUEF" "OP_CP"))
(defvar op_plus2 (list "OP_OP" "OP_PLUS" "IDENTIFIER" "VALUEF" "OP_CP"))
(defvar op_plus3 (list "OP_OP" "OP_PLUS" "VALUEF" "IDENTIFIER" "OP_CP"))
(defvar op_plus4 (list "OP_OP" "OP_PLUS" "IDENTIFIER" "IDENTIFIER" "OP_CP"))
(defvar op_minus (list "OP_OP" "OP_MINUS" "VALUEF" "VALUEF" "OP_CP"))
(defvar op_minus2 (list "OP_OP" "OP_MINUS" "IDENTIFIER" "VALUEF" "OP_CP"))
(defvar op_minus3 (list "OP_OP" "OP_MINUS" "VALUEF" "IDENTIFIER" "OP_CP"))
(defvar op_minus4 (list "OP_OP" "OP_MINUS" "IDENTIFIER" "IDENTIFIER" "OP_CP"))
(defvar op_mul (list "OP_OP" "OP_MULT" "VALUEF" "VALUEF" "OP_CP"))
(defvar op_mul2 (list "OP_OP" "OP_MULT" "IDENTIFIER" "VALUEF" "OP_CP"))
(defvar op_mul3 (list "OP_OP" "OP_MULT" "VALUEF" "IDENTIFIER" "OP_CP"))
(defvar op_mul4 (list "OP_OP" "OP_MULT" "IDENTIFIER" "IDENTIFIER" "OP_CP"))
(defvar op_div (list "OP_OP" "OP_DIV" "VALUEF" "VALUEF" "OP_CP"))
(defvar op_div2 (list "OP_OP" "OP_DIV" "IDENTIFIER" "VALUEF" "OP_CP"))
(defvar op_div3 (list "OP_OP" "OP_DIV" "VALUEF" "IDENTIFIER" "OP_CP"))
(defvar op_div4 (list "OP_OP" "OP_DIV" "IDENTIFIER" "IDENTIFIER" "OP_CP"))

(defun can_ignore (word)					;;ignoring ignored list
    (some (lambda (x) (string= x word)) ignored)
)


(defun split_line (line)					;;splitting line
    (if (string= line "")
        nil
        (let* ((pos (position #\Space line))
            (word (if pos (subseq line 0 pos) line))
            (remainder (if pos (subseq line (1+ pos)) "")))
            (if (string= word "")
                (split_line remainder)
                (cons word (split_line remainder))
            )
        )
    )
)

(defun read-file-line-by-line (file-path)			;;reading line by line from file
    (with-open-file (stream file-path :direction :input)
        (loop for line = (read-line stream nil)
          while line
          collect line
        )
    )
)

(defun existoperator (word)					;;if operator exist then true
    (if (some (lambda (operator) (search operator word)) operators)
      T
      NIL)
)

(defun can_print_operator (char)				;;printing exist operator
    (if (member char operators :test #'string=)
        (let ((index (position char operators :test #'string=)))
            (setq lexer_list (append lexer_list (list (nth index printoperators))))
            ;(format t "~a~%" (nth index printoperators))
            T
        )
        nil
    )
)

(defun valid_identifier (word)					;;if valid identifier found then return true
    (and
        (<= 1 (length word))
        (alpha-char-p (char word 0)) 
        (every #'(lambda (c) (or (alpha-char-p c) (digit-char-p c) (char= c #\_))) (subseq word 1))
    )
)

(defun existkeyword (word)					;;if keyword found then print and return true
    (let ((index (position word keywords :test #'string=)))
        (if index
            (progn
                (setq lexer_list (append lexer_list (list (nth index printkeywords))))
                ;(format t "~a~%" (nth index printkeywords))
                T
            )
            nil
        )
    )
)


(defun isvaluef (word)							;;valuef checker like 1f3, 4f5, 33f5
  (let ((pos (position #\f word)))
    (and pos
         (> pos 0)								;; Must be one digit before f
         (< pos (1- (length word)))						;; Must be one digit after f
         (every #'digit-char-p (subseq word 0 pos))				;; Checking digits before f
         (every #'digit-char-p (subseq word (1+ pos)))				;; Checking digits after f
         (not (char= (char word 0) #\0))					;; Checking leading zero before f
         (not (char= (char (subseq word (1+ pos)) 0) #\0))			;; Checking leading zero after f
     )
   )
)


(defun split-word (word)					;;separates adjacent words
    (let ((result '())
        (substring-start 0)
        )
        (loop for i from 0 below (length word)
            for char = (char word i)
            do (if (member (string char) operators :test #'string=)
                    (progn
                        (when (< substring-start i)
                            (push (subseq word substring-start i) result)
                        )
                        (push (string char) result)
                        (setf substring-start (1+ i))
                    )
                )
        )
        (when (< substring-start (length word))
            (push (subseq word substring-start) result)
        )
        (nreverse result)
    )
)

(defun get_args () 						;;getting args from terminal
    (let ((args *args*))
       (let ((file-path (car args)))
        (if (>= (length file-path ) 1)
            file-path
            nil
        )
       )
    )
)

(defun check_elements_for_exit (lst)
   (labels ((check-order (elements expected-order)
             (if (null elements)
                 (null expected-order)
                 (if (equal (car elements) (car expected-order))
                     (check-order (cdr elements) (cdr expected-order))
                     nil))))
    (check-order lst '("OP_OP" "KW_EXIT" "OP_CP")))

)

(defun check_elements_syntax_error (lst)
  (if (some (lambda (x) (string= x "SYNTAX ERROR!")) lst)
      T
      nil)
)

(defun check_elements_for_plus (lst)
  (labels ((check-order (elements expected-order)
             (if (null elements)
                 (null expected-order)
                 (if (equal (car elements) (car expected-order))
                     (check-order (cdr elements) (cdr expected-order))
                     nil))))
    (check-order lst op_plus))
)

(defun check_elements_for_minus (lst)
  (labels ((check-order (elements expected-order)
             (if (null elements)
                 (null expected-order)
                 (if (equal (car elements) (car expected-order))
                     (check-order (cdr elements) (cdr expected-order))
                     nil))))
    (check-order lst op_minus))
)

(defun check_elements_for_mult (lst)
  (labels ((check-order (elements expected-order)
             (if (null elements)
                 (null expected-order)
                 (if (equal (car elements) (car expected-order))
                     (check-order (cdr elements) (cdr expected-order))
                     nil))))
    (check-order lst op_mul))
)

(defun check_elements_for_div (lst)
  (labels ((check-order (elements expected-order)
             (if (null elements)
                 (null expected-order)
                 (if (equal (car elements) (car expected-order))
                     (check-order (cdr elements) (cdr expected-order))
                     nil))))
    (check-order lst op_div))
)

(defun parse_literal (literal)
  "Parse a literal in the form 'numeratorbdenominator' and return a cons cell with the numerator and denominator."
  (let ((separator-pos (position #\f literal)))
    (if separator-pos
        (cons (parse-integer (subseq literal 0 separator-pos))
              (parse-integer (subseq literal (1+ separator-pos))))
        (error "Invalid literal format: %s" literal)))
)

(defun check_elements_for_zeroexp (lst)
  (labels ((check-order (elements expected-order)
             (if (null elements)
                 (null expected-order)
                 (if (listp (car expected-order))
                     (and (check-order elements (car expected-order))
                          (check-order (cdr elements) (cdr expected-order)))
                     (if (equal (car elements) (car expected-order))
                         (check-order (cdr elements) (cdr expected-order))
                         nil)))))
    (or (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" ,@op_plus "OP_CP"))  (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" ,@op_minus "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" ,@op_mul "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" ,@op_div "OP_CP"))))
)

(defun check_elements_for_oneexp (lst)
  (labels ((check-order (elements expected-order)
             (if (null elements)
                 (null expected-order)
                 (if (listp (car expected-order))
                     (and (check-order elements (car expected-order))
                          (check-order (cdr elements) (cdr expected-order)))
                     (if (equal (car elements) (car expected-order))
                         (check-order (cdr elements) (cdr expected-order))
                         nil)))))
    (or (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" ,@op_plus "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" ,@op_plus2 "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" ,@op_plus3 "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" ,@op_minus "OP_CP"))  (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" ,@op_minus2 "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" ,@op_minus3 "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" ,@op_mul "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" ,@op_mul2 "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" ,@op_mul3 "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" ,@op_div "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" ,@op_div2 "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" ,@op_div3 "OP_CP"))))
)


(defun check_elements_for_twoexp (lst)
  (labels ((check-order (elements expected-order)
             (if (null elements)
                 (null expected-order)
                 (if (listp (car expected-order))
                     (and (check-order elements (car expected-order))
                          (check-order (cdr elements) (cdr expected-order)))
                     (if (equal (car elements) (car expected-order))
                         (check-order (cdr elements) (cdr expected-order))
                         nil)))))
    (or (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" "IDENTIFIER"  ,@op_plus "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" "IDENTIFIER" ,@op_plus2 "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" "IDENTIFIER" ,@op_plus3 "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" "IDENTIFIER" ,@op_plus4 "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" "IDENTIFIER" ,@op_minus "OP_CP"))  (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" "IDENTIFIER" ,@op_minus2 "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" "IDENTIFIER" ,@op_minus3 "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" "IDENTIFIER" ,@op_minus4 "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" "IDENTIFIER" ,@op_mul "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" "IDENTIFIER" ,@op_mul2 "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" "IDENTIFIER" ,@op_mul3 "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" "IDENTIFIER" ,@op_mul4 "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" "IDENTIFIER" ,@op_div "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" "IDENTIFIER" ,@op_div2 "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" "IDENTIFIER" ,@op_div3 "OP_CP")) (check-order lst `("OP_OP" "KW_DEFFUN" "IDENTIFIER" "IDENTIFIER" "IDENTIFIER" ,@op_div4 "OP_CP"))))
)


(defun gppinterpreter (&optional file_name)			;;main function!!!
    (loop
        (if file_name
            (let ((file-path file_name))				;;reading file if not exist read from terminal
                (setq lines (read-file-line-by-line file-path))
            )
            (progn
                (format t "~%Enter:(qqq for exit)~%")
                (force-output)
                (let ((user_input (read-line *standard-input*)))
                    (if (string= user_input "qqq")				;;if user enter qqq exit
                        (return)
                    )
                    (push user_input lines)
                )
            )
        )
        (setq file_name nil)
        (dolist (line lines)
            (let ((words (split_line line)))		;;lexical analysis start!!!
                (dolist (word words)
                    (cond 
                        ((existoperator word)
                            (progn
                                (setq splitted_word (split-word word))
                                (dolist (i splitted_word)
                                    (if (>= (length i) 2)
                                        (if (string= ";;" (subseq i 0 2))
                                            (progn
                                            (setq lexer_list (append lexer_list (list "COMMENT")))
                                                ;;(format t "COMMENT~&")				;; comment check
                                                (setq canpass T)
                                                (return)
                                            )
                                        )
                                    )
                                    (cond
                                        ((can_print_operator i))
                                        ((existkeyword i))
                                        ((isvaluef i)
                                        (setq literals (append literals (list i)))
                                            (setq lexer_list (append lexer_list (list "VALUEF")))
                                            ;;(format t "VALUEF~&")				;;valuef check
                                        )
                                        ((valid_identifier i)
                                        (setq lexer_list (append lexer_list (list "IDENTIFIER")))
                                            ;;(format t "IDENTIFIER~&")				;;identifier check
                                        )
                                        ((can_ignore i)
                                            (return)
                                        )
                                        (T
                                        (setq lexer_list (append lexer_list (list "SYNTAX ERROR!")))
                                           ;; (format t "SYNTAX_ERROR: ~a cannot be tokenized~%" i)			;;error check
                                            (setq alt_f4 T)						;;if error then stop
                                            (return)
                                        )
                                    )
                                )
                                (if canpass
                                    (progn
                                        (setq canpass nil)
                                        (return)
                                    )
                                )
                                (when alt_f4
                                    (return)
                                )
                            )
                        )
                        ((can_print_operator word))
                        ((existkeyword word))
                        ((isvaluef word)
                        		(setq literals (append literals (list word)))
                                        (setq lexer_list (append lexer_list (list "VALUEF")))
                            ;;(format t "VALUEF~&")
                        )
                        ((valid_identifier word)
                        (setq lexer_list (append lexer_list (list "IDENTIFIER")))
                            ;;(format t "IDENTIFIER~&")
                        )
                        (T
                        (setq lexer_list (append lexer_list (list "SYNTAX ERROR!")))
                            ;;(format t "SYNTAX_ERROR: ~a cannot be tokenized~%" word)			;;error check
                            (setq alt_f4 T)								;;if error then stop
                            (return)
                        )
                    )
                )
                
                (cond
                    ((= (length lexer_list) 0)
                        
                    )
                    ( (check_elements_syntax_error lexer_list)
                        (print "SYNTAX ERROR!")
                        (setq alt_ff4 T)
                        (return)
                    )
                    ( (check_elements_for_exit lexer_list)
                        (setq alt_ff4 T)
                        (return)
                    )
                    ( (check_elements_for_plus lexer_list)
                        (setf result1 (parse_literal (car literals)))
                        (setf result2 (parse_literal (cadr literals)))
                        (setf resultnum (+ (* (car result1) (cdr result2) ) (* (cdr result1) (car result2)) ))
                        (setf resultden (* (cdr result2)  (cdr result1)))
                        (format t "~af~a~%" resultnum resultden)
                    )
                    ( (check_elements_for_minus lexer_list)
                        (setf result1 (parse_literal (car literals)))
                        (setf result2 (parse_literal (cadr literals)))
                        (setf resultnum (- (* (car result1) (cdr result2) ) (* (cdr result1) (car result2)) ))
                        (setf resultden (* (cdr result2)  (cdr result1)))
                        (format t "~af~a~%" resultnum resultden)
                    )
                    ( (check_elements_for_mult lexer_list)
                        (setf result1 (parse_literal (car literals)))
                        (setf result2 (parse_literal (cadr literals)))
                        (setf resultnum (* (car result1) (car result2)))
                        (setf resultden (* (cdr result2)  (cdr result1)))
                        (format t "~af~a~%" resultnum resultden)
                    )
                    ( (check_elements_for_div lexer_list)
                        (setf result1 (parse_literal (car literals)))
                        (setf result2 (parse_literal (cadr literals)))
                        (setf resultnum (* (car result1) (cdr result2)))
                        (setf resultden (* (car result2)  (cdr result1)))
                        (format t "~af~a~%" resultnum resultden)
                    )
                    ( (check_elements_for_zeroexp lexer_list)
                        (format t "#function~%")
                    )
                    ( (check_elements_for_oneexp lexer_list)
                        (format t "#function~%")
                    )
                    ( (check_elements_for_twoexp lexer_list)
                        (format t "#function~%")
                    )
                    (T
                        (print "SYNTAX ERROR!")
                        (setf alt_ff4 T)
                    )
                )
                (when alt_ff4
                    (return)
                )
                 (setq lexer_list nil)
                (setq literals nil)
                
                
               
                
            )
        )
        (when alt_ff4
                    (return)
                )
        (setq lines nil)
    )
)

(gppinterpreter (get_args))

