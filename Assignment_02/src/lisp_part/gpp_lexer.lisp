(defvar keywords (list "and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "print" "true" "false"))
(defvar printkeywords (list "KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_PRINT" "KW_TRUE" "KW_FALSE"))
(defvar operators (list "+" "-" "/" "*" "(" ")" "," ";;"))
(defvar printoperators (list "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_OP" "OP_CP" "OP_COMMA" "COMMENT"))
(defvar ignored (list (string #\linefeed) "\t" " " "" (string #\return) "\r\n")) 
(defvar lines nil)						;; gettin from file and add to this array
(defvar splitted_word nil)
(defvar canpass nil)						;;for comment line(;;)
(defvar alt_f4 nil)						;;for exiting the program

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
            (format t "~a~%" (nth index printoperators))
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
                (format t "~a~%" (nth index printkeywords))
                T
            )
            nil
        )
    )
)

(defun isvaluei (word)							;; valuei checker like 1,2,..99...
  (every #'digit-char-p word))

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


(defun gppinterpreter (&optional file_name)			;;main function!!!
    (loop
        (if file_name
            (let ((file-path file_name))				;;reading file if not exist read from terminal
                (setq lines (read-file-line-by-line file-path))
            )
            (progn
                (format t "Enter:(qqq for exit) ")
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
                                                (format t "COMMENT~&")				;; comment check
                                                (setq canpass T)
                                                (return)
                                            )
                                        )
                                    )
                                    (cond
                                        ((can_print_operator i))
                                        ((existkeyword i))
                                        ((isvaluef i)
                                            (format t "VALUEF~&")				;;valuef check
                                        )
                                        ((isvaluei i)
                			(format t "VALUEI~&")					;;valuei check
                			)
                                        ((valid_identifier i)
                                            (format t "IDENTIFIER~&")				;;identifier check
                                        )
                                        ((can_ignore i)
                                            (return)
                                        )
                                        (T
                                            (format t "SYNTAX_ERROR: ~a cannot be tokenized~%" i)			;;error check
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
                            (format t "VALUEF~&")
                        )
                        ((isvaluei word)
                	    (format t "VALUEI~&")
                	)
                        ((valid_identifier word)
                            (format t "IDENTIFIER~&")
                        )
                        (T
                            (format t "SYNTAX_ERROR: ~a cannot be tokenized~%" word)			;;error check
                            (setq alt_f4 T)								;;if error then stop
                            (return)
                        )
                    )
                )
                (when alt_f4
                    (return)
                )
                
            )
        )
        (setq lines nil)
    )
)

(gppinterpreter (get_args))
