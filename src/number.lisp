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
(in-package :lesp-builtin)

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

(defclass -number-proto (-object-proto)
  ((-prototype :initform (find-class '-object-proto))
   (-number-data :type number-raw :accessor -number-data
		 :initarg :-number-data)
   (constructor :initform (make-property :value -number) :allocation :class)
   (to-exponential :type property :allocation :class
		   :initform (make-property :value !to-exponential))
   (to-fixed :type property :allocation :class
	     :initform (make-property :value !to-fixed))
   (to-precision :type property :allocation :class
		 :initform (make-property :value !to-precision)))
  (:documentation "Number prototype, provides inherited properties."))

(defun -to-number (arg)
  (typecase arg
    (undefined-raw
     (-number :nan))
    (null-raw
     (-number 0))
    (-boolean-proto
     (if (eql (slot-value arg '-boolean-data) :true)
	 (-number 1)
	 (-number 0)))
    (-number-proto
     arg)
    (-string-proto
     (-number (string-to-number arg)))
    (-symbol-proto
     (error (find-property message '-type-error-proto)))
    (-object-proto
     (-number (-to-primitive arg :hint 'number)))))

(defun -to-integer (arg)
  (let* ((number (-to-number arg))
	 (data (slot-value number '-number-data)))
    (case data
      (:nan
       (-number 0))
      ((0 :infinity :-infinity)
       number)
      (t
       ((-number (* (signum data)
		    (floor (abs data)))))))))

(defun -to-int32 (arg)
  (let ((data (slot-value (-to-number arg) '-number-data)))
    (case data
      ((:nan 0 :infinity :-infinity)
       (-number 0))
      (t
       (let ((int-32bit (mod (* (signum data)
				(floor (abs data))) (expt 2 32))))
	 (if (>= int-32bit (expt 2 31))
	     (-number (- int-32bit (expt 2 32)))
	     (-number int-32bit)))))))

(defun -to-uint32 (arg)
  (let ((data (slot-value (-to-number arg) '-number-data)))
    (case data
      ((:nan 0 :infinity :-infinity)
       (-number 0))
      (t
       (let ((int-32bit (mod (* (signum data)
				(floor (abs data))) (expt 2 32))))
	 (-number int-32bit))))))

(defun -to-int16 (arg)
  (let ((data (slot-value (-to-number arg) '-number-data)))
    (case data
      ((:nan 0 :infinity :-infinity)
       (-number 0))
      (t
       (let ((int-16bit (mod (* (signum data)
				(floor (abs data))) (expt 2 16))))
	 (if (>= int-16bit (expt 2 15))
	     (-number (- int-16bit (expt 2 16)))
	     (-number int-16bit)))))))

(defun -to-uint16 (arg)
  (let ((data (slot-value (-to-number arg) '-number-data)))
    (case data
      ((:nan 0 :infinity :-infinity)
       (-number 0))
      (t
       (let ((int-16bit (mod (* (signum data)
				(floor (abs data))) (expt 2 16))))
	 (-number int-16bit))))))

(defmethod fetch-properties ((this -number-proto))
  (properties (make-instance (class-name this))))

(defmethod to-exponential ((this -number-proto) digits)
  )

(defmethod to-fixed ((this -number-proto) digits)
  )

(defmethod to-locale-string ((this -number-proto))
  )

(defmethod to-precision ((this -number-proto) precision)
  )

(defmethod to-string ((this -number-proto) &optional radix)
  )

(defmethod value-of ((this -number-proto))
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
