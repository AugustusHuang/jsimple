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

;;;; Number related routines and constants.
(in-package :jsimple-builtin)

;;; In SBCL 32-bit version, it's OK.
(defconstant +number-max-safe-integer+ (- (expt 2 53) 1))
(defconstant +number-max-value+ most-positive-double-float)
(defconstant +number-min-safe-integer+ (- (- (expt 2 53) 1)))
(defconstant +number-min-value+ least-positive-double-float)
;;; NOTE: It seems that there's a little mistake... Now ignore it.
(defconstant +number-epsilon+ (* 2 double-float-epsilon))

(defconstant +math-e+ (exp 1.0d0))
(defconstant +math-ln10+ (log 10.0d0))
(defconstant +math-ln2+ (log 2.0d0))
(defconstant +math-log10e+ (log +math-e+ 10))
(defconstant +math-log2e+ (log +math-e+ 2))
(defconstant +math-pi+ pi)
(defconstant +math-sqrt-1/2 (sqrt (/ 1.0d0 2.0d0)))
(defconstant +math-sqrt-2 (sqrt 2.0d0))

;;; Get rid of complains, use DEFPARAMETER...
(defparameter +decimal-digits+ "0123456789")
(defparameter +exponent-indicator+ "eE")

(deftype number-raw ()
  `(and double-float (member :nan :infinity :-infinity)))

(defclass -number ()
  ((constructor :reader constructor :type function
		:initarg :constructor :initform #'-new-number
		:allocation class)
   (data :accessor data :type js-number-raw
	 :initarg :data :initform 0.0d0))
  (:documentation "Builtin Number prototype."))

(defun -new-number (value)
  (-object-number value))

;;; Helper function to parse a general number...
(defun parse-number (string)
  ;; Firstly check the first two chars, if they match 0x/0X, 0o/0O, 0b/0B,
  ;; use corresponding radix PARSE-INTEGER. Or handle decimal values.
  (declare (type string string))
  (let ((sign 1))
    (when (> (length string) 2)
      (let ((first-char (char string 0))
	    (second-char (char string 1)))
	(when (char= first-char #\0)
	  (case second-char
	    ((#\x #\X)
	     (return-from parse-number
	       (parse-integer string :start 2 :radix 16)))
	    ((#\o #\O)
	     (return-from parse-number
	       (parse-integer string :start 2 :radix 8)))
	    ((#\b #\B)
	     (return-from parse-number
	       (parse-integer string :start 2 :radix 2)))))))
    ;; Now we must be parsing a decimal, or NaN.
    (let ((integer-part 0)
	  (decimal-part 0.0d0)
	  (saw-integer-digits nil)
	  (saw-decimal-digits nil)
	  (saw-decimal-point nil)
	  (exponent #\E)
	  (exponent-sign 1)
	  (exponent-value 0.0d0)
	  (position 0))
      ;; Underlying methods are taken from Mezzano's reader.
      (declare (type integer integer-part)
	       (type double-float decimal-part exponent-value))
      (flet ((peek ()
	       (when (< position (length string))
		 (char string position)))
	     (consume ()
	       (prog1 (char string position)
		 (incf position))))
	;; Check for a leading sign.
	(case (peek)
	  (#\- (consume)
	       (setf sign -1))
	  (#\+ (consume)))
	;; Remaining string must not be empty.
	(when (null (peek))
	  (return-from parse-number (values :nan 0)))
	;; Parse the integer portion.
	(loop
	   (let ((weight (position (peek) +decimal-digits+)))
	     (when (not weight) (return))
	     (consume)
	     (setf saw-integer-digits t)
	     (setf integer-part (+ (* integer-part 10) weight))))
	;; Parse the decimal portion.
	(when (char= #\. (peek))
	  (setf saw-decimal-point t)
	  (consume)
	  ;; If there was an integer part, then the next character
	  ;; must be either a decimal-digit or an exponent marker.
	  ;; If there was no integer part, it must be a decimal-digit.
	  (when (and (not (or (not saw-integer-digits)
			      (find (peek) +exponent-indicator+)))
		     (not (find (peek) +decimal-digits+)))
	    (return-from parse-number (values :nan 0)))
	  ;; Accumulate decimal digits.
	  (let ((first-decimal position))
	    (loop
	       (when (not (find (peek) +decimal-digits+))
		 (return))
	       (setf saw-decimal-digits t)
	       (consume))
	    ;; Now works backwards and build the decimal part.
	    (dotimes (i (- position first-decimal))
	      (incf decimal-part (digit-char-p (char string (- position i 1))))
	      (setf decimal-part (/ decimal-part 10.0d0)))))
	;; And look for an exponent.
	(when (find (peek) +exponent-indicator+)
	  (setf exponent (consume))
	  (case (peek)
	    (#\- (consume)
		 (setf exponent-sign -1))
	    (#\+ (consume)))
	  ;; Must be at least one digit in the exponent
	  ;; and one digit in the integer part
	  (when (or (not (find (peek) +decimal-digits+))
		    (not saw-integer-digits))
	    (return-from parse-number (values :nan 0)))
	  ;; Read exponent part.
	  (loop (when (not (find (peek) +decimal-digits+))
		  (return))
	     (setf exponent-value (+ (* exponent-value 10.0d0)
				     (digit-char-p (consume))))))
	;; Must be at the end.
	(when (peek)
	  (return-from parse-number (values :nan 0)))
	(* sign
	   (+ integer-part decimal-part)
	   (expt 10.0d0 (* exponent-sign exponent-value)))))))

(defun -to-number (value)
  (typecase value
    (-undefined
     (make-instance '-number :data :nan))
    (-null
     (make-instance '-number :data 0.0d0))
    (-boolean
     (make-instance '-number :data (if (data value)
				       1.0d0
				       0.0d0)))
    (-number
     value)
    (-string
     (make-instance '-number :data (parse-number (data value))))
    (-symbol
     (error '-type-error))
    (-object
     (make-instance '-number :data (parse-number
				    (slot-value value 'primitive-value))))))

