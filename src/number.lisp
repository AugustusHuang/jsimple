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
(defconstant +math-sqrt-1/2+ (sqrt (/ 1.0d0 2.0d0)))
(defconstant +math-sqrt-2+ (sqrt 2.0d0))

;;; Get rid of complains, use DEFPARAMETER...
(defparameter +decimal-digits+ "0123456789")
(defparameter +exponent-indicator+ "eE")

(deftype number-raw ()
  `(and double-float (member :nan :infinity :-infinity)))

(defclass -number-prototype (-object-prototype)
  ((-prototype :initform '-object-prototype :allocation :class)
   (-number-data :type number-raw :accessor -number-data
		 :initarg :-number-data)
   (constructor :initform '-number :allocation :class)
   (properties
    :initform
    (append (fetch-properties (find-class '-object-prototype))
	    '((to-exponential . (make-property :value 'to-exponential))
	      (to-fixed . (make-property :value 'to-fixed))
	      (to-precision . (make-property :value 'to-precision))))
    :allocation :class))
  (:documentation "Number prototype, provides inherited properties."))

(defclass -number (-function-prototype)
  ((-prototype :initform '-function-prototype :allocation :class)
   (length :initform (make-property :value 1) :allocation :class)
   (prototype :type (or property -null) :accessor prototype :allocation :class
	      :initarg :prototype
	      :initform (make-property :value '-number-prototype))
   (properties
    :initform
    (append (fetch-properties (find-class '-function-prototype))
	    '((epsilon . (make-property :value +number-epsilon+))
	      (is-finite . (make-property :value 'is-finite))
	      (is-integer . (make-property :value 'is-integer))
	      (is-nan . (make-property :value 'is-nan))
	      (is-safe-integer . (make-property :value 'is-safe-integer))
	      (max-safe-integer . (make-property :value +number-max-safe-integer+))
	      (max-value . (make-property :value +number-max-value+))
	      (min-safe-integer . (make-property :value +number-min-safe-integer+))
	      (min-value . (make-property :value +number-min-value+))
	      (nan . (make-property :value :nan))
	      (negative-infinity . (make-property :value :-infinity))
	      (parse-float . (make-property :value 'parse-float))
	      (parse-int . (make-property :value 'parse-int))
	      (positive-infinity . (make-property :value :infinity))))
    :allocation :class))
  (:documentation "Number constructor, used with new operator."))

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

(defmethod fetch-properties ((this -number-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -number))
  (properties (make-instance (class-name this))))

(defmethod to-exponential ((this -number-prototype) digits)
  )

(defmethod to-fixed ((this -number-prototype) digits)
  )

(defmethod to-locale-string ((this -number-prototype))
  )

(defmethod to-precision ((this -number-prototype) precision)
  )

(defmethod to-string ((this -number-prototype) &optional radix)
  )

(defmethod value-of ((this -number-prototype))
  )

;;; Math object definitions. Since Math is not a function and can't be called
;;; or constructed, it's only a wrapper.
(defclass -number (-object-prototype)
  ((-prototype :initform '-object-prototype :allocation :class)
   (properties
    :initform
    (append (fetch-properties (find-class '-object-prototype))
	    '((e . (make-properties :value +math-e+))
	      (ln10 . (make-properties :value +math-ln10+))
	      (ln2 . (make-properties :value +math-ln2+))
	      (log10e . (make-properties :value +math-log10e+))
	      (log2e . (make-properties :value +math-log2e+))
	      (pi . (make-properties :value +math-pi+))
	      (sqrt1-2 . (make-properties :value +math-sqrt-1/2+))
	      (sqrt2 . (make-properties :value +math-sqrt-2+))
	      (to-string-tag . (make-properties :value "Math"
				:configurable :true))
	      (abs . (make-properties :value 'abs))
	      (acos . (make-properties :value 'acos))
	      (acosh . (make-properties :value 'acosh))
	      (asin . (make-properties :value 'asin))
	      (asinh . (make-properties :value 'asinh))
	      (atan . (make-properties :value 'atan))
	      (atanh . (make-properties :value 'atanh))
	      (atan2 . (make-properties :value 'atan2))
	      (cbrt . (make-properties :value 'cbrt))
	      (ceil . (make-properties :value 'ceil))
	      (clz32 . (make-properties :value 'clz32))
	      (cos . (make-properties :value 'cos))
	      (cosh . (make-properties :value 'cosh))
	      (exp . (make-properties :value 'exp))
	      (expm1 . (make-properties :value 'expm1))
	      (floor . (make-properties :value 'floor))
	      (fround . (make-properties :value 'fround))
	      (hypot . (make-properties :value 'hypot))
	      (imul . (make-properties :value 'imul))
	      (log . (make-properties :value 'log))
	      (log1p . (make-properties :value 'log1p))
	      (log10 . (make-properties :value 'log10))
	      (log2 . (make-properties :value 'log2))
	      (max . (make-properties :value 'max))
	      (min . (make-properties :value 'min))
	      (pow . (make-properties :value 'pow))
	      (random . (make-properties :value 'random))
	      (round . (make-properties :value 'round))
	      (sign . (make-properties :value 'sign))
	      (sin . (make-properties :value 'sin))
	      (sinh . (make-properties :value 'sinh))
	      (sqrt . (make-properties :value 'sqrt))
	      (tan . (make-properties :value 'tan))
	      (tanh . (make-properties :value 'tanh))
	      (trunc . (make-properties :value 'trunc))))))
  (:documentation "Number object, used as a reference type."))

;;; Some wrapper functions.
;;; FIXME: Some of them are not done, note the :NAN and :INFINITY!
(declaim (inline atan2 cbrt ceil clz32 expm1 hypot imul log1p log10 log2
		 pow sign trunc))

(defun atan2 (y x)
  (atan y x))

(defun cbrt (x)
  (case x
    (:nan
     :nan)
    (:infinity
     :infinity)
    (:-infinity
     :-infinity)
    (t
     (expt x (coerce 1/3 'double-float)))))

(defun ceil (x)
  (case x
    (:nan
     :nan)
    (:infinity
     :infinity)
    (:-infinity
     :-infinity)
    (t
     (ceiling x))))

(defun clz32 (x)
  (- 32 (integer-length x)))

(defun expm1 (x)
  (- (exp x) 1))

(defun hypot (value1 value2 &rest values)
  )

(defun imul (x y)
  )

(defun log1p (x)
  (log (1+ x)))

(defun log10 (x)
  (log x 10))

(defun log2 (x)
  (log x 2))

(defun pow (x y)
  (expt x y))

(defun sign (x)
  (case x
    (:nan
     :nan)
    ((:infinity :-infinity)
     :nan)
    (t
     (signum x))))

(defun trunc (x)
  (case x
    (:nan
     :nan)
    (:infinity
     :infinity)
    (:-infinity
     :-infinity)
    (t
     (truncate x))))
