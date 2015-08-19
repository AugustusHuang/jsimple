(in-package :jsimple-parser)

;;; Can we utilize the readtable?
;;; (FIRST *lexer-position*) -> the current row,
;;; (SECOND *lexer-position*) -> the current position.
(defvar *lexer-position* '(1, 0))

(defparameter +decimal-digits+ "0123456789")
(defparameter +none-0-decimal-digits+ "123456789")
(defparameter +octo-digits+ "01234567")
(defparameter +hex-digits+ "0123456789abcdefABCDEF")

(defun plus-sign-p (char)
  (eql char #\+))

(defun minus-sign-p (char)
  (eql char #\-))

(defun point-p (char)
  (eql char #\.))

(defun jsimple-lexer (stream)
  "Simple lexer function, will return the token and its type to upper level."
  (let ((c (read-char stream nil nil))
	;; It's ugly, but use it for now.
	(in-decimal-number-p nil)
	(in-binary-number-p nil)
	(in-octo-number-p nil)
	(in-hex-number-p nil)
	(in-id-p nil)
	(in-short-comment-p nil)
	(in-long-comment-p nil)
	(in-long-comment-star-p nil) ; We are facing /* *, and expect /.
	(in-double-string-p nil)
	(in-single-string-p nil))
    (cond
      ;; EOF
      ((null c)
       (progn
	 (setf *lexer-position* '(1, 0))
	 (values nil nil)))
      ;; space or tab, update the position
      ((member c '(#\Space #\Tab))
       (progn
	 (incf (second *lexer-position*))
	 (jsimple-lexer stream)))
      ;; newline, update the position
      ((eql c #\Newline)
       (progn
	 (if in-short-comment-p
	     (setf in-short-comment-p nil))
	 (incf (first *lexer-position*))
	 (jsimple-lexer stream)))
      ;; /, can lead to //, /* and /=, or evaluate to /, 
      ((eql c #\/)
       (if in-long-comment-star-p
	   (progn
	     (setf in-long-comment-star-p nil
		   in-long-comment-p nil)
	     (incf (first *lexer-position*))
	     (jsimple-lexer-stream))
	   ()))
      ;; operators that can have suffices.
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
