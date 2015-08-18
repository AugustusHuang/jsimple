
;;;; Javascript syntax and grammar in ECMA-262:
;;;; input-element-div ::
;;;;   whitespace | line-terminator | comment | common-token |
;;;;   div-punctuator |  right-brace-punctuator
;;;; whitespace ::
;;;;   TAB | VT | FF | SP | NBSP | ZWNBSP | USP
;;;; line-terminator ::
;;;;   LF | CR | LS | PS
;;;; comment ::
;;;;   multi-line-comment | single-line-comment
;;;; multi-line-comment ::
;;;;   /* (multi-line-comment-chars) */
;;;; multi-line-comment-chars ::
;;;;   multi-line-not-asterisk-char (multi-line-comment-chars) |
;;;;   * (post-asterisk-comment-chars)
;;;; post-asterisk-comment-chars ::
;;;;   multi-line-not-forward-slash-or-asterisk-char (multi-line-comment-chars) |
;;;;   * (post-asterisk-comment-chars)
;;;; multi-line-not-asterisk-char ::
;;;;   source-character \ *
;;;; multi-line-not-forward-slash-or-asterish-char ::
;;;;   source-character \ [* | /]
;;;; single-line-comment ::
;;;;   // (single-line-comment-chars)
;;;; single-line-comment-chars ::
;;;;   single-line-comment-char (single-line-comment-chars)
;;;; single-line-comment-char ::
;;;;   source-character \ line-terminator
;;;; common-token ::
;;;;   id | punctuator | number | string | template
;;;; 

;;;; Parser package.
(in-package :jsimple-parser)

(defvar *lexer-position* 0)
;; Maybe useful, declare here!
(defvar *parser-position* 0)

(defun jsimple-lexer (stream)
  "Simple lexer function, will return the token and its type to upper level."
  (let ((c (read-char stream nil nil))
	(in-number-p nil)
	(in-name-p nil))
    (cond
      ((null c)
       (progn
	 (setf *lexer-position* 0)
	 (values nil nil)))
      ((member c '(#\Space #\Tab #\Newline))
       (progn
	 (incf *lexer-position*)
	 (jsimple-lexer stream)))
      ;; +, -, *, /, % are arithmetic operators,
      ;; (, ), [, ], {, } are containers.
      ;; TODO: Handle // and /* */ comments!
      ((member c '(#\+ #\- #\* #\/ #\( #\) #\% #\[ #\] #\{ #\}))
       (let ((v (intern (string c))))
	 (incf *lexer-position*)
         (values v v)))
      ;; TODO: Read the number as long as possible! e.g. 1234e-5.
      ;; TODO: The javascript floating point numbers follow the IEEE 754
      ;; but how could we realize it in Lisp-land?
      ((digitp c)
       (let ((buffer (make-array 10 :element-type 'character
                                 :fill-pointer 0)))
	 ;; When we read the number in this loop, IN-NUMBER-P will always
	 ;; be T, so we don't need to check it since it will always be NIL
	 ;; if we get out of this loop.
	 (setf in-number-p t)
         (do* ((c c (read-char stream nil nil))
	       (*lexer-position* *lexer-position* (incf *lexer-position*)))
	      ((or (null c) (not (or (digitp c)
				     ;; ., -, e are allowed inside.
				     ;; FIXME: But only once!
				     ;; Check it and ensure IN-NUMBER-P
				     ;; is true, if over once set it to
				     ;; false and UNREAD one char.
				     (member c '(#\. #\- #\e)))))
	       (unless (null c) (unread-char c stream))
	       ;; In javascript all numbers are 64-bit float!
	       (values 'number (read-from-string buffer)))
           (vector-push-extend c buffer))))
      ;; TODO: Read the id as long as possible! e.g. this_symbol.
      ((alpha-char-p c)
       (let ((buffer (make-array 10 :element-type 'character
                                 :fill-pointer 0)))
	 (setf in-name-p t)
         (do* ((c c (read-char stream nil nil))
	       (*lexer-position* *lexer-position* (incf *lexer-position*)))
	      ((or (null c) (not (or (alphanumericp c)
				     (char= c #\_))))
	       (unless (null c) (unread-char c stream))
	       (values 'id (copy-seq buffer)))
	   (incf *lexer-position*)
           (vector-push-extend c buffer))))
      ;; TODO: Handle string!
      ()
      (t (error 'lexer-error c *lexer-position*)))))

;;; Since Lisp is written with prefix operators, we have to convert it into
;;; javascript-land style.
(defun infix-to-prefix (a op b)
  (list op a b))

;;; Similarly, a++ will be translated into (INCF A) with no optimization.
(defun suffix-to-prefix (a op)
  (list op a))

;;; Select the second argument.
(defun snd (f s t)
  (declare (ignore f t))
  s)

;;; Grammar:
;;; TODO:
(define-parser *jsimple-parser*
  (:start-symbol statements)
  (:terminals
   (number
    id
    + ++ - --
    * / %
    += -= *= /= %= = == === != !==
    > < >= <=
    |(| |)|))
  (:precedence ((:left * / %) (:left + -)))

  (statements
   (statements statement)
   expression)

  (statement
   while-statement
   iteration-statement
   )

  (expression
   (expression + expression #'infix-to-prefix)
   (expression - expression #'infix-to-prefix)
   (expression * expression #'infix-to-prefix)
   (expression / expression #'infix-to-prefix)
   (expression % expression #'infix-to-prefix)
   (expression == expression #'infix-to-prefix)
   (expression === expression #'infix-to-prefix)
   (expression != expression #'infix-to-prefix)
   (expression !== expression #'infix-to-prefix)
   (expression < expression #'infix-to-prefix)
   (expression <= expression #'infix-to-prefix)
   (expression > expression #'infix-to-prefix)
   (expression >= expression #'infix-to-prefix)
   (expression = expression #'infix-to-prefix)
   (expression += expression #'infix-to-prefix)
   (expression -= expression #'infix-to-prefix)
   (expression *= expression #'infix-to-prefix)
   (expression /= expression #'infix-to-prefix)
   (expression %= expression #'infix-to-prefix)
   (expression ++ #'suffix-to-prefix)
   (expression -- #'suffix-to-prefix)
   term)

  (term
   number
   id
   (- term)
   (|(| expression |)| #'snd)))

;;; Test function, read a valid javascript clip and return a prefix-ed
;;; Lispish-script.
(defun parse (code)
  (parse-with-lexer (jsimple-lexer code)
		    *jsimple-parser*))

