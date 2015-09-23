;;;; The MIT License (MIT)

;;;; Copyright (c) 2015 Huang Xuxing

;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:

;;;; The above copyright notice and this permission notice shall be included
;;;; in all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

;;;; Copyright (c) Marijn Haverbeke, marijnh@gmail.com

;;;; This software is provided 'as-is', without any express or implied
;;;; warranty. In no event will the authors be held liable for any
;;;; damages arising from the use of this software.

;;;; Permission is granted to anyone to use this software for any
;;;; purpose, including commercial applications, and to alter it and
;;;; redistribute it freely, subject to the following restrictions:

;;;; 1. The origin of this software must not be misrepresented; you must
;;;;    not claim that you wrote the original software. If you use this
;;;;    software in a product, an acknowledgment in the product
;;;;    documentation would be appreciated but is not required.

;;;; 2. Altered source versions must be plainly marked as such, and must
;;;;    not be misrepresented as being the original software.

;;;; 3. This notice may not be removed or altered from any source
;;;;    distribution.

(in-package :lesp-parser)

;;; Now javascript has a new 6th version standard, adjust PARSE-JS lib,
;;; make it compatible with new features. Also make some modifications
;;; on the coding style. More comments.
(defmacro with-defs (&body body)
  (loop for form in body
     if (and (eq (car form) 'def) (< (length form) 4))
     collect (cadr form) into vars and
     if (caddr form) collect `(setf ,(cadr form) ,(caddr form)) into body end
     else if (eq (car form) 'def)
     collect (cdr form) into funcs
     else
     collect form into body
     finally (return `(let ,vars (labels ,funcs ,@body)))))

(defmacro defun/defs (name args &body body)
  `(defun ,name ,args (with-defs ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct token type value line char pos newline-before comments-before))

;;; These functions are tiny, make them inline.
(declaim (inline tokenp token-type-p token-id))
(defun tokenp (token type value)
  (and (eq (token-type token) type)
       (eql (token-value token) value)))
(defun token-type-p (token type)
  (eq (token-type token) type))
(defun token-id (token)
  (token-value token))

;;; Those three variables are used to record where now we are, and how we
;;; report the error position correctly.
(defvar *line*)
(defvar *char*)
(defvar *position*)

(define-condition lexer-error (general-error)
  ((char :initarg :char
	 :initform lesp-parser:*char* :reader lexer-error-char)
   (line :initarg :line
	 :initform lesp-parser:*line* :reader lexer-error-line))
  (:documentation "Lesp lexer error."))

;;; Simplify the error print.
(defmethod print-object ((err lexer-error) stream)
  (call-next-method)
  (format stream ":~A:~D:"
	  (lexer-error-char err)
	  (lexer-error-line err)))

;;; Every error will have a wrapper function to make it specific.
(defun lexer-error (control &rest args)
  (error 'lexer-error :format-control control :format-arguments args))

;;; NOTE: All constant parameters should be surronded by +.
(defparameter +operator-chars+ "+-*&%=<>!?|~^")

;;; Add arrow function, OF operator and ... spread operator.
(defparameter +operators+
  (let ((ops (make-hash-table :test 'equal)))
    (dolist (op '(:in :instanceof :of :typeof :new :void :delete
		  :++ :-- :+ :- :! :~ :& :|\|| :^ :* :/ :%
                  :>> :<< :>>> :< :> :<= :>= :== :=== :!= :!==
		  :? := :+= :-= :/= :*= :%= :>>= :<<= :=>
                  :>>>= :~= :%= :|\|=| :^= :&= :&& :|\|\|| :...))
      (setf (gethash (string-downcase (string op)) ops) op))
    ops))

(defparameter +whitespace-chars+
  (concatenate 'string (list #\space #\tab #.(code-char 11) #\page #\return
			     #\newline (code-char #xa0) (code-char #x2028)
			     (code-char #x2029))))

(defparameter +line-terminators+
  (concatenate 'string (list #\newline #\return
			     (code-char #x2028) (code-char #x2029))))

;;; NOTE: :AS and :FROM are used when we need to use module, this is
;;; enabled by default in our compiler, and they are not mentioned as keywords
;;; in ECMA-262 version 6, so in this compiler all files which have 'as'
;;; and 'from' as some of their variable names won't run.
(defparameter +keywords+
  (let ((keywords (make-hash-table :test 'equal)))
    (dolist (word '(:as :break :case :catch :class :const :continue :debugger
		    :default :delete :do :else :export :extends :false
		    :finally :for :from :function :if :implements :import :in
		    :instanceof :interface :let :new :null :package :private
		    :protected :public :return :static :super :switch :this
		    :throw :true :try :typeof :var :void :while :with :yield))
      (setf (gethash (string-downcase (string word)) keywords) word))
    keywords))

(defparameter +keywords-before-expression+
  '(:return :new :delete :throw :else :case))

(defparameter +keywords-strict+
  '(:let :static :implements :interface :package :private :protected :public
    :yield))

(defparameter +atom-keywords+
  '(:false :null :true :undefined))

;;; There won't be too many reserved words in ECMA 6, use list instead.
(defparameter +reserved-words-ecma-6+
  '(:enum :await))

(defparameter *check-for-reserved-words* nil)

(defun read-js-number (stream &key junk-allowed)
  "Wrapper of READ-JS-NUMBER-1, read a number with specific radix."
  (flet ((peek-1 () (peek-char nil stream nil nil))
         (next-1 () (read-char stream nil nil)))
    (read-js-number-1 #'peek-1 #'next-1 :junk-allowed junk-allowed)))

;;; EQ returns T in SBCL when handling same keyword.
(defun read-js-number-1 (peek next &key junk-allowed)
  "Read a number with specific radix."
  (labels ((digits (radix)
             (with-output-to-string (out)
               (loop for ch = (funcall peek)
		  while (and ch (digit-char-p ch radix)) do
		    (write-char (funcall next) out)))))
    (let ((minus (case (funcall peek)
		   (#\+ (funcall next) nil) (#\- (funcall next) t)))
          (body (digits 10))
          (*read-default-float-format* 'double-float))
      (flet ((ret (x)
               (return-from read-js-number-1
                 (and x (or junk-allowed (eq (funcall peek) nil))
		      (if minus (if (eq x :infinity) :-infinity (- x)) x)))))
        (cond ((and (equal body "0") (find (funcall peek) "xX") (funcall next))
               (ret (parse-integer (digits 16) :junk-allowed t :radix 16)))
	      ((and (equal body "0") (find (funcall peek) "oO") (funcall next))
	       (ret (parse-integer (digits 8) :junk-allowed t :radix 8)))
	      ((and (equal body "0") (find (funcall peek) "bB") (funcall next))
	       (ret (parse-integer (digits 2) :junk-allowed t :radix 2)))
              ((find (funcall peek) ".eE")
               (let ((base (if (string= body "") 0 (parse-integer body)))
                     (expt 0) (expt-neg nil))
                 (if (and (eql (funcall peek) #\.) (funcall next))
                     (let ((digs (digits 10)))
                       (if (string= digs "")
                           (when (string= body "") (ret nil))
                           (loop (handler-case
                                     (return (incf base (/ (parse-integer digs)
							   (expt 10d0 (length digs)))))
                                   (floating-point-overflow ()
				     (setf digs (subseq digs 0 (1- (length digs)))))))))
                     (when (equal body "") (ret nil)))
                 (when (and (find (funcall peek) "eE") (funcall next))
                   (setf expt-neg (and (find (funcall peek) "+-") (eql (funcall next) #\-)))
                   (let ((digs (digits 10)))
                     (when (equal digs "") (ret nil))
                     (setf expt (parse-integer digs))))
                 (handler-case (ret (* base (expt 10d0 (if expt-neg (- expt) expt))))
                   (floating-point-overflow () (ret :infinity))
                   (floating-point-underflow () (ret 0d0)))))
              ((equal body "") (ret nil))
              ((and (char= (char body 0) #\0)
                    (loop for i from 1 below (length body) do
			 (unless (digit-char-p (char body i)) (return nil))
                       finally (return t)))
               (ret (parse-integer body :radix 8)))
              ((equal body "") (ret nil))
              (t (ret (parse-integer body))))))))

(defun/defs lex-js (stream &key include-comments)
  (def expression-allowed t)
  (def newline-before nil)
  (def line 1)
  (def char 0)
  (def position 0)
  (def comments-before nil)

  (def start-token ()
    (setf *line* line
          *char* char
          *position* position))
  
  (def token (type value)
    (setf expression-allowed
          (or (and (eq type :operator)
                   (not (member value '("++" "--") :test #'string=)))
              (and (eq type :keyword)
                   (member value +keywords-before-expression+))
              (and (eq type :punc)
                   (find value "[{(,.;:"))))
    (prog1 (make-token :type type :value value :line *line* :char *char*
		       :pos *position*
                       :newline-before newline-before
                       :comments-before (reverse comments-before))
      (setf newline-before nil)
      (setf comments-before nil)))

  (def peek ()
    (peek-char nil stream nil))
  
  (def next (&optional eof-error in-string)
    (let ((ch (read-char stream eof-error)))
      (when ch
        (incf position)
        (if (find ch +line-terminators+)
            (progn
              (setf line (1+ line) char 0)
              (unless in-string (setf newline-before t)))
            (incf char)))
      ch))

  (def skip-whitespace ()
    (loop for ch = (peek)
       while (and ch (find ch +whitespace-chars+))
       do (next)))
  
  (def read-while (pred)
    (with-output-to-string (*standard-output*)
      (loop for ch = (peek)
	 while (and ch (funcall pred ch))
	 do (princ (next)))))

  (def read-num (&optional start)
    (let ((num (or (read-js-number-1 (lambda () (if start start (peek)))
                                     (lambda ()
				       (if start
					   (prog1 start (setf start nil))
					   (next)))
                                     :junk-allowed t)
                   (lexer-error "Invalid number syntax"))))
      (token :num num)))

  (def handle-dot ()
    (next)
    (if (digit-char-p (peek))
        (read-num #\.)
        (token :punc #\.)))

  (def hex-bytes (n char)
    (loop with num = 0
       for pos from (1- n) downto 0
       do (let ((digit (digit-char-p (next t) 16)))
	    (if digit
		(incf num (* digit (expt 16 pos)))
		(lexer-error "Invalid \\~A escape pattern" char)))
       finally (return num)))
  
  (def read-escaped-char (&optional in-string)
    (let ((ch (next t in-string)))
      (case ch
        (#\n #\newline) (#\r #\return) (#\t #\tab)
        (#\b #\backspace) (#\v #.(code-char 11)) (#\f #\page) (#\0 #\null)
        (#\x (code-char (hex-bytes 2 #\x)))
        (#\u (code-char (hex-bytes 4 #\u)))
        (#\newline nil)
        (t (let ((num (digit-char-p ch 8)))
             (if num
                 (loop for nx = (digit-char-p (peek) 8) do
		      (when (or (not nx) (>= num 32)) (return (code-char num)))
		      (next)
		      (setf num (+ nx (* num 8))))
                 ch))))))

  (def read-template ()
    ;; Eat a backquote.
    (let ((in-interpolate nil))
      (next)
      (handler-case
	  (token :template
		 (with-output-to-string (*standard-output*)
		   (loop (let ((ch (next t)))
			   (cond ((eql ch #\$)
				  (if (eql (next t) #\{)
				      (progn
					(setf in-interpolate t)
					(write-char #\$)
					(write-char #\{))))
				 ((and (eql ch #\}) in-interpolate)
				  (setf in-interpolate nil)
				  (write-char ch))
				 ;; Templates allow multiple line input.
				 ((and (eql ch #\`) (null in-interpolate))
				  (return))
				 ((and (eql ch #\`) in-interpolate)
				  (lexer-error "Unterminated template literal"))
				 (t (write-char ch)))))))
	(end-of-file () (lexer-error "Unterminated template")))))
  
  (def read-string ()
    (let ((quote (next)))
      (handler-case
          (token :string
                 (with-output-to-string (*standard-output*)
                   (loop (let ((ch (next t)))
                           (cond ((eql ch #\\)
				  (let ((ch (read-escaped-char t)))
				    (when ch (write-char ch))))
                                 ((find ch +line-terminators+)
				  (lexer-error "Line terminator inside string"))
                                 ((eql ch quote) (return))
                                 (t (write-char ch)))))))
        (end-of-file () (lexer-error "Unterminated string")))))

  (def add-comment (type c)
    (when include-comments
      ;; doing this instead of calling (token) as we don't want
      ;; to put comments-before into a comment token
      (push (make-token :type type
                        :value c
                        :line *line*
                        :char *char*
                        :pos *position*
                        :newline-before newline-before)
            comments-before)))

  (def read-line-comment ()
    (next)
    (if include-comments
        (add-comment :short-comment
                     (with-output-to-string (out)
                       (loop for ch = (next)
                          until (or (find ch +line-terminators+) (not ch))
                          do (write-char ch out))))
        (loop for ch = (next)
           until (or (find ch +line-terminators+) (not ch)))))

  (def read-multiline-comment ()
    (next)
    (if include-comments
        (add-comment :long-comment
                     (with-output-to-string (out)
                       (loop with star = nil
                          for ch = (or (next)
				       (lexer-error "Unterminated comment"))
                          until (and star (eql ch #\/))
                          do (setf star (eql ch #\*)) (write-char ch out))))
        (loop with star = nil
           for ch = (or (next)
			(lexer-error "Unterminated comment"))
           until (and star (eql ch #\/))
           do (setf star (eql ch #\*)))))

  ;; TODO: how about unicode string/regex?
  (def read-regexp ()
    (handler-case
        (token :regexp
               (cons
                (with-output-to-string (*standard-output*)
                  (loop with backslash = nil with inset = nil
		     for ch = (next t)
		     until (and (not backslash) (not inset) (eql ch #\/)) do
		       (unless backslash
			 (when (eql ch #\[) (setf inset t))
			 (when (and inset (not backslash) (eql ch #\]))
			   (setf inset nil)))
		       (setf backslash (and (eql ch #\\) (not backslash)))
		     ;; Handle \u sequences, since CL-PPCRE does not understand them.
		       (if (and backslash (eql (peek) #\u))
			   (let* ((code (progn
					  (setf backslash nil)
					  (next)
					  (hex-bytes 4 #\u)))
				  (ch (code-char code)))
			     (if ch
				 (write-char ch)
				 ;; Will be deleted on SBCL.
				 ;; (format t "\\u~4,'0X" code)
				 ))
			   (write-char ch))))
                (read-while #'identifier-char-p)))
      (end-of-file () (lexer-error "Unterminated regex"))))

  (def read-operator (&optional start)
    (labels ((grow (str)
               (let ((bigger (concatenate 'string str (string (peek)))))
                 (if (gethash bigger +operators+)
                     (progn (next) (grow bigger))
                     (token :operator (gethash str +operators+))))))
      (grow (or start (string (next))))))

  (def handle-slash ()
    (next)
    (case (peek)
      (#\/ (read-line-comment)
           (next-token))
      (#\* (read-multiline-comment)
           (next-token))
      (t (if expression-allowed
             (read-regexp)
             (read-operator "/")))))

  (def identifier-char-p (ch)
    (or (and (alphanumericp ch) (not (find ch +whitespace-chars+)))
	(eql ch #\$)
	(eql ch #\_)))

  ;; FIXME: Add strict mode!
  ;; When encounter use strict, go into strict mode and get out when this
  ;; scope ends, but how to recognize scope? Use a scope counter or what?
  ;; --- Augustus, 24 Aug 2015.
  (def read-word ()
    (let* ((unicode-escape nil)
           (word (with-output-to-string (*standard-output*)
                   (loop for ch = (peek) do
			(cond ((eql ch #\\)
			       (next)
			       (unless (eql (next) #\u)
				 (lexer-error "Unrecognized escape in id"))
			       (write-char (code-char (hex-bytes 4 #\u)))
			       (setf unicode-escape t))
			      ((and ch (identifier-char-p ch)) (write-char (next)))
			      (t (return))))))
           (keyword (and (not unicode-escape) (gethash word +keywords+))))
      (cond ((and *check-for-reserved-words* (not unicode-escape)
                  (gethash word	+reserved-words-ecma-6+))
             (lexer-error "Reserved word ~A" word))
            ((not keyword) (token :name word))
            ((gethash word +operators+) (token :operator keyword))
            ((member keyword +atom-keywords+) (token :atom keyword))
            (t (token :keyword keyword)))))

  (def next-token (&optional force-regexp)
    (if force-regexp
        (read-regexp)
        (progn
          (skip-whitespace)
          (start-token)
          (let ((next (peek)))
            (cond ((not next) (token :eof "EOF"))
                  ((digit-char-p next) (read-num))
                  ((find next "'\"") (read-string))
		  ((eql next #\`) (read-template))
                  ((eql next #\.) (handle-dot))
                  ((find next "[]{}(),;:") (token :punc (next)))
                  ((eql next #\/) (handle-slash))
                  ((find next +operator-chars+) (read-operator))
                  ((or (identifier-char-p next) (eql next #\\)) (read-word))
                  (t (lexer-error "Unexpected char '~A'" next)))))))
  
  #'next-token)

