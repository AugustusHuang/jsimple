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
(eval-when (:compile-toplevel :load-toplevel :execute)
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
  (defconstant +math-sqrt-2+ (sqrt 2.0d0)))

;;; Get rid of complains, use DEFPARAMETER...
(defparameter +decimal-digits+ "0123456789")
(defparameter +exponent-indicator+ "eE")

(defclass -number-proto (-object-proto)
  ((proto :initform (find-class '-object-proto))
   (-prototype :initform (find-class '-number-proto))
   (-number-data :type number-raw :initarg :-number-data)
   (constructor :initform (make-property :value '!number) :allocation :class)
   (to-exponential :type property :allocation :class
		   :initform (make-property :value '!to-exponential))
   (to-fixed :type property :allocation :class
	     :initform (make-property :value '!to-fixed))
   (to-precision :type property :allocation :class
		 :initform (make-property :value '!to-precision)))
  (:documentation "Number prototype, provides inherited properties."))

;;; Make special number instance global.
(defparameter *number-nan* (!number :nan))
(defparameter *number-infinity* (!number :infinity))
(defparameter *number--infinity* (!number :-infinity))
(defparameter *number-0* (!number 0))
(defparameter *number--0* (!number :-0))

(defun -to-number (arg)
  (typecase arg
    (undefined-type
     *number-nan*)
    (null-type
     *number-0*)
    (boolean-type
     (if (eql (slot-value arg '-boolean-data) :true)
	 (!number 1)
	 *number-0*))
    (number-type
     arg)
    (string-type
     (!number (string-to-number arg)))
    (symbol-type
     (error "Can't convert symbol to number"))
    (object-type
     (!number (-to-primitive arg :hint 'number)))))

(defun -to-integer (arg)
  (let* ((number (-to-number arg))
	 (data (slot-value number '-number-data)))
    (case data
      (:nan
       *number-0*)
      ((0 :infinity :-infinity :-0)
       number)
      (t
       ((!number (* (signum data)
		    (floor (abs data)))))))))

(defun -to-int32 (arg)
  (let ((data (slot-value (-to-number arg) '-number-data)))
    (case data
      ((:nan 0 :infinity :-infinity :-0)
       *number-0*)
      (t
       (let ((int-32bit (mod (* (signum data)
				(floor (abs data))) (expt 2 32))))
	 (if (>= int-32bit (expt 2 31))
	     (!number (- int-32bit (expt 2 32)))
	     (!number int-32bit)))))))

(defun -to-uint32 (arg)
  (let ((data (slot-value (-to-number arg) '-number-data)))
    (case data
      ((:nan 0 :infinity :-infinity :-0)
       *number-0*)
      (t
       (let ((int-32bit (mod (* (signum data)
				(floor (abs data))) (expt 2 32))))
	 (!number int-32bit))))))

(defun -to-int16 (arg)
  (let ((data (slot-value (-to-number arg) '-number-data)))
    (case data
      ((:nan 0 :infinity :-infinity :-0)
       *number-0*)
      (t
       (let ((int-16bit (mod (* (signum data)
				(floor (abs data))) (expt 2 16))))
	 (if (>= int-16bit (expt 2 15))
	     (!number (- int-16bit (expt 2 16)))
	     (!number int-16bit)))))))

(defun -to-uint16 (arg)
  (let ((data (slot-value (-to-number arg) '-number-data)))
    (case data
      ((:nan 0 :infinity :-infinity :-0)
       *number-0*)
      (t
       (let ((int-16bit (mod (* (signum data)
				(floor (abs data))) (expt 2 16))))
	 (!number int-16bit))))))

(defun -to-int8 (arg)
  (let ((data (slot-value (-to-number arg) '-number-data)))
    (case data
      ((:nan 0 :infinity :-infinity :-0)
       *number-0*)
      (let ((int-8bit (mod (* (signum data)
			      (floor (abs data))) (expt 2 8))))
	(if (>= int-8bit (expt 2 7))
	    (!number (- int-8bit (expt 2 8)))
	    (!number int-8bit))))))

(defun -to-uint8 (arg)
  (let ((data (slot-value (-to-number arg) '-number-data)))
    (case data
      ((:nan 0 :infinity :-infinity :-0)
       *number-0*)
      (let ((int-8bit (mod (* (signum data)
			      (floor (abs data))) (expt 2 8))))
	(!number int-8bit)))))

(defun -to-uint8-clamp (arg)
  (let ((data (slot-value (-to-number arg) '-number-data)))
    (cond ((eql data :nan)
	   *number-0*)
	  ((or (<= data 0) (eql data :-infinity) (eql data :-0))
	   *number-0*)
	  ((or (>= data 255) (eql data :infinity))
	   (!number 255))
	  (t
	   (let ((f (floor data)))
	     (if (< (+ f 0.5d0) data)
		 (!number (1+ f))
		 (if (> (+ f 0.5d0) data)
		     (!number f)
		     (if (oddp f)
			 (!number (1+ f))
			 (!number f)))))))))

(defun -number.is-finite (number)
  (cond ((not (eql (-type number) 'number-type))
	 *boolean-false*)
	((or (eql number *number-nan*)
	     (eql number *number-infinity*)
	     (eql number *number--infinity*))
	 *boolean-false*)
	(t
	 *boolean-true*)))

(defun -number.is-integer (number)
  (cond ((not (eql (-type number) 'number-type))
	 *boolean-false*)
	((or (eql number *number-nan*)
	     (eql number *number-infinity*)
	     (eql number *number--infinity*))
	 *boolean-false*)
	((not (= (-to-integer number) number))
	 *boolean-false*)
	(t
	 *boolean-true*)))

(defun -number.is-nan (number)
  (cond ((not (eql (-type number) 'number-type))
	 *boolean-false*)
	((eql number *number-nan*)
	 *boolean-true*)
	(t
	 *boolean-false*)))

(defun -number.is-safe-integer (number)
  (let ((int (-to-integer number)))
    (cond ((not (eql (-type number) 'number-type))
	   *boolean-false*)
	  ((or (eql number *number-nan*)
	       (eql number *number-infinity*)
	       (eql number *number--infinity*))
	   *boolean-false*)
	  ((not (= (slot-value int '-number-data)
		   (slot-value number '-number-data)))
	   *boolean-false*)
	  ((<= (slot-value (!abs int) '-number-data) +number-max-safe-integer+)
	   *boolean-true*)
	  (t
	   *boolean-false*))))

(defun -number.parse-float (string)
  (.parse-float string))

(defun -number.parse-int (string radix)
  (.parse-int string radix))

(defmethod %to-exponential ((this -number-proto) &optional digits)
  (let ((f (if (null digits)
	       0
	       (slot-value (-to-integer digits) '-number-data)))
	(x (slot-value this '-number-data)))
    (case x
      (:nan
       (!string "NaN"))
      (:infinity
       (!string "Infinity"))
      (:-infinity
       (!string "-Infinity"))
      (:-0
       (!string (format nil (concatenate "~," (write-to-string f) "E" 0))))
      (t
       (!string (format nil (concatenate 'string "~," (write-to-string f) "E" x)))))))

(defmethod %to-fixed ((this -number-proto) digits)
  (let ((x (slot-value this '-number-data))
	(f (if (null digits) 0
	       (slot-value (-to-integer digits) '-number-data))))
    (case x
      (:nan
       (!string "NaN"))
      (:infinity
       (!string "Infinity"))
      (:-infinity
       (!string "-Infinity"))
      (:-0
       (!string (format nil (concatenate 'string "~," (write-to-string f) "F" 0))))
      (t
       (!string (format nil (concatenate 'string "~," (write-to-string f) "F" x)))))))

(defmethod %to-locale-string ((this -number-proto))
  (%to-string this))

(defmethod %to-precision ((this -number-proto) precision)
  (let ((x (slot-value this '-number-data)))
    (if (null precision)
	(return-from %to-precision (!string (-to-string x)))
	(let ((p (slot-value (-to-integer precision) '-number-data)))
	  (case x
	    (:nan
	     (!string "NaN"))
	    (:infinity
	     (!string "Infinity"))
	    (:-infinity
	     (!string "-Infinity"))
	    (:-0
	     (!string (format nil
			      (concatenate 'string "~," (write-to-string (1- p)) "F") 0)))
	    (t
	     (labels ((count-digits (num acc)
			(let ((tr (truncate num 10)))
			  (if (zerop tr)
			      acc
			      (count-digits tr (1+ acc))))))
	       (let ((digits (count-digits x 0)))
		 (if (> digits p)
		     (!string (format nil (concatenate 'string "~," (write-to-string (- digits p)) "F") x))
		     (if (< digits p)
			 (!string (format nil (concatenate 'string "~," (write-to-string p)) "E") x)
			 (!string (format nil "~D" x))))))))))))

(defmethod %to-string ((this -number-proto) &optional radix)
  (let ((x (slot-value this '-number-data))
	(radix-num (if (null radix)
		       10
		       (slot-value (-to-integer radix) '-number-data))))
    (when (or (< radix-num 2) (> radix-num 36))
      (error "Range error."))
    (if (= radix-num 10)
	(-to-string x)
	(!string (format nil (concatenate 'string "~" (write-to-string radix-num) "R") x)))))

(defmethod %value-of ((this -number-proto))
  this)

;;; Math object definitions. Since Math is not a function and can't be called
;;; or constructed, it's only a wrapper.
(defclass -math (-object-proto)
  ((proto :initform (find-class '-object-proto))
   (-prototype :initform (find-class '-math))
   (e :type property :allocation :class
      :initform (make-property :value +math-e+))
   (ln10 :type property :allocation :class
	 :initform (make-property :value +math-ln10+))
   (ln2 :type property :allocation :class
	:initform (make-property :value +math-ln2+))
   (log10e :type property :allocation :class
	   :initform (make-property :value +math-log10e+))
   (log2e :type property :allocation :class
	  :initform (make-property :value +math-log2e+))
   (pi :type property :allocation :class
       :initform (make-property :value +math-pi+))
   (sqrt1-2 :type property :allocation :class
	    :initform (make-property :value +math-sqrt-1/2+))
   (sqrt2 :type property :allocation :class
	  :initform (make-property :value +math-sqrt-2+))
   (to-string-tag :type property :allocation :class
		  :initform (make-property :value "Math"
					   :configurable :true))
   (abs :type property :allocation :class
	:initform (make-property :value '!abs))
   (acos :type property :allocation :class
	 :initform (make-property :value '!acos))
   (acosh :type property :allocation :class
	  :initform (make-propert :value '!acosh))
   (asin :type property :allocation :class
	 :initform (make-property :value '!asin))
   (asinh :type property :allocation :class
	  :initform (make-property :value '!asinh))
   (atan :type property :allocation :class
	 :initform (make-property :value '!atan))
   (atanh :type property :allocation :class
	  :initform (make-property :value '!atanh))
   (atan2 :type property :allocation :class
	  :initform (make-property :value '!atan2))
   (cbrt :type property :allocation :class
	 :initform (make-property :value '!cbrt))
   (ceil :type property :allocation :class
	 :initform (make-property :value '!ceil))
   (clz32 :type property :allocation :class
	  :initform (make-property :value '!clz32))
   (cos :type property :allocation :class
	:initform (make-property :value '!cos))
   (cosh :type property :allocation :class
	 :initform (make-property :value '!cosh))
   (exp :type property :allocation :class
	:initform (make-property :value '!exp))
   (expm1 :type property :allocation :class
	  :initform (make-property :value '!expm1))
   (floor :type property :allocation :class
	  :initform (make-property :value '!floor))
   (fround :type property :allocation :class
	   :initform (make-property :value '!fround))
   (hypot :type property :allocation :class
	  :initform (make-property :value '!hypot))
   (imul :type property :allocation :class
	 :initform (make-property :value '!imul))
   (log :type property :allocation :class
	:initform (make-property :value '!log))
   (log1p :type property :allocation :class
	  :initform (make-property :value '!log1p))
   (log10 :type property :allocation :class
	  :initform (make-property :value '!log10))
   (log2 :type property :allocation :class
	 :initform (make-property :value '!log2))
   (max :type property :allocation :class
	:initform (make-property :value '!max))
   (min :type property :allocation :class
	:initform (make-property :value '!min))
   (pow :type property :allocation :class
	:initform (make-property :value '!pow))
   (random :type property :allocation :class
	   :initform (make-property :value '!random))
   (round :type property :allocation :class
	  :initform (make-property :value '!round))
   (sign :type property :allocation :class
	 :initform (make-property :value '!sign))
   (sin :type property :allocation :class
	:initform (make-property :value '!sin))
   (sinh :type property :allocation :class
	 :initform (make-property :value '!sinh))
   (sqrt :type property :allocation :class
	 :initform (make-property :value '!sqrt))
   (tan :type property :allocation :class
	:initform (make-property :value '!tan))
   (tanh :type property :allocation :class
	 :initform (make-property :value '!tanh))
   (trunc :type property :allocation :class
	  :initform (make-property :value '!trunc)))
  (:documentation "Math object, used as a reference type."))

;;; Some wrapper functions.
(declaim (inline -math.abs -math.acos -math.acosh -math.asin -math.asinh
		 -math.atan -math.atanh -math.atan2 -math.cbrt -math.ceil
		 -math.clz32 -math.cos -math.cosh -math.exp -math.expm1
		 -math.floor -math.fround -math.hypot -math.imul -math.log
		 -math.log1p -math.log10 -math.log2 -math.max -math.min
		 -math.pow -math.random -math.round -math.sign -math.sin
		 -math.sinh -math.sqrt -math.tan -math.tanh -math.trunc))

;;; All declarations shall be replaced by a implicit type conversion,
;;; which converts and handles the type error.
(defun -math.abs (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (case n
      (:nan
       *number-nan*)
      (:-infinity
       *number-infinity*)
      (:-0
       *number-0*)
      (t
       x))))

(defun -math.acos (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (cond ((eql n :nan)
	   *number-nan*)
	  ((or (> n 1) (eql n :infinity))
	   *number-nan*)
	  ((or (< n -1) (eql n :-infinity))
	   *number-nan*)
	  ((= x 1)
	   *number-0*)
	  ((eql n :-0)
	   (!number (acos 0)))
	  (t
	   (!number (acos n))))))

(defun -math.acosh (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (cond ((eql n :nan)
	   *number-nan*)
	  ((or (< n 1) (eql n :-infinity))
	   *number-nan*)
	  ((= n 1)
	   *number-0*)
	  ((eql n :infinity)
	   *number-infinity*)
	  ((eql n :-0)
	   (!number (acosh 0)))
	  (t
	   (!number (acosh n))))))

(defun -math.asin (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (cond ((eql n :nan)
	   *number-nan*)
	  ((or (> n 1) (eql n :infinity))
	   *number-nan*)
	  ((or (< n -1) (eql n :-infinity))
	   *number-nan*)
	  ((eql n :-0)
	   *number--0*)
	  ((zerop n)
	   *number-0*)
	  (t
	   (!number (asin n))))))

(defun -math.asinh (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (cond ((eql n :nan)
	   *number-nan*)
	  ((zerop n)
	   *number-0*)
	  ((eql n :-0)
	   *number--0*)
	  ((eql n :infinity)
	   *number-infinity*)
	  ((eql n :-infinity)
	   *number--infinity*)
	  (t
	   (!number (asinh n))))))

(defun -math.atan (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (cond ((eql n :nan)
	   *number-nan*)
	  ((zerop n)
	   *number-0*)
	  ((eql n :-0)
	   *number--0*)
	  ((eql n :infinity)
	   (!number (/ +number-pi+ 2)))
	  ((eql n :-infinity)
	   (!number (- (/ +number-pi+ 2))))
	  (t
	   (!number (atan x))))))

(defun -math.atanh (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (cond ((eql n :nan)
	   *number-nan*)
	  ((or (< n -1) (eql n :-infinity) (> n 1) (eql n :infinity))
	   *number-nan*)
	  ((= n -1)
	   *number--infinity*)
	  ((= n -1)
	   *number-infinity*)
	  ((zerop n)
	   *number-0*)
	  ((eql n :-0)
	   *number--0*)
	  (t
	   (!number (atanh n))))))

;;; Add -0 as a case.
(defun -math.atan2 (y x)
  (declare (type number-type y x))
  (let ((n (slot-value y '-number-data))
	(m (slot-value x '-number-data)))
    (cond ((or (eql n :nan) (eql m :nan))
	   *number-nan*)
	  ((and (> n 0) (or (zerop m) (eql m :-0)))
	   (!number (/ +number-pi+ 2)))
	  ((and (> m 0) (zerop n))
	   *number-0*)
	  ((and (zerop n) (zerop m))
	   *number-0*)
	  ((and (or (< m 0) (eql m :-0)) (zerop n))
	   (!number +number-pi+))
	  ((and (eql n :-0) (>= x 0))
	   *number--0*)
	  ((and (eql n :-0) (or (< x 0) (eql m :-0)))
	   (!number (- +number-pi+)))
	  ((and (< n 0) (or (eql m :-0) (zerop m)))
	   (!number (/ +number-pi+ 2)))
	  ((and (> n 0) (eql m :infinity))
	   *number-0*)
	  ((and (> n 0) (eql m :-infinity))
	   (!number +number-pi+))
	  ((and (< n 0) (eql m :infinity))
	   *number--0*)
	  ((and (< n 0) (eql m :-infinity))
	   (!number (- +number-pi+)))
	  ((and (eql n :infinity) (eql m :infinity))
	   (!number (/ +number-pi+ 4)))
	  ((and (eql n :infinity) (eql m :-infinity))
	   (!number (/ (* 3 +number-pi+) 4)))
	  ((and (eql n :-infinity) (eql m :infinity))
	   (!number (- (/ +number-pi+ 4))))
	  ((and (eql n :-infinity) (eql m :-infinity))
	   (!number (- (/ (* 3 +number-pi+) 4))))
	  ((eql n :infinity)
	   (!number (/ +number-pi+ 2)))
	  ((eql n :-infinity)
	   (!number (- (/ +number-pi+ 2))))
	  (t
	   (!number (atan y x))))))

(defun -math.cbrt (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (case n
      (:nan
       *number-nan*)
      (0
       *number-0*)
      (:-0
       *number--0*)
      (:infinity
       *number-infinity*)
      (:-infinity
       *number--infinity*)
      (t
       (!number (expt n (coerce 1/3 'double-float)))))))

(defun -math.ceil (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (cond ((eql n :nan)
	   *number-nan*)
	  ((zerop n)
	   *number-0*)
	  ((eql n :-0)
	   *number--0*)
	  ((eql n :infinity)
	   *number-infinity*)
	  ((eql n :-infinity)
	   *number--infinity*)
	  ((< -1 x 0)
	   *number--0*)
	  (t
	   (!number (ceiling x))))))

(defun -math.clz32 (x)
  (declare (type number-type x))
  (let ((n (slot-value (-to-uint32 x) '-number-data)))
    (!number (- 32 (integer-length n)))))

(defun -math.cos (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (case n
      (:nan
       *number-nan*)
      ((0 :-0)
       (!number 1))
      ((:infinity :-infinity)
       *number-nan*)
      (t
       (!number (cos n))))))

(defun -math.cosh (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (case n
      (:nan
       *number-nan*)
      ((0 :-0)
       (!number 1))
      ((:infinity :-infinity)
       *number-infinity*)
      (t
       (!number (cosh n))))))

(defun -math.exp (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (case n
      (:nan
       *number-nan*)
      ((0 :-0)
       (!number 1))
      (:infinity
       *number-infinity*)
      (:-infinity
       *number-0*)
      (t
       (!number (exp n))))))

(defun -math.expm1 (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (case n
      (:nan
       *number-nan*)
      (0
       *number-0*)
      (:-0
       *number--0*)
      (:infinity
       *number-infinity*)
      (:-infinity
       (!number -1))
      (t
       (!number (- (exp x) 1))))))

(defun -math.floor (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (case n
      (:nan
       *number-nan*)
      (0
       *number-0*)
      (:-0
       *number--0*)
      (:infinity
       *number-infinity*)
      (:-infinity
       *number--infinity*)
      (t
       (!number (floor n))))))

(defun -math.fround (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (case n
      (:nan
       *number-nan*)
      ((0 :-0 :infinity :-infinity)
       x)
      (t
       (!number (fround n))))))

;;; In order to implement the no argument clause in standard,
;;; we use optional arguments to pretend to be a length 0 function
;;; with generated length 2.
(defun -math.hypot (&optional value1 value2 &rest values)
  (declare (type (or null number-type) value1 value2)
	   (type list values)))
  (let* ((val1 (if value1 (slot-value value1 '-number-data) 0))
	 (val2 (if value2 (slot-value value2 '-number-data) 0))
	 (valuel
	  (loop for elem in values collect (slot-value elem '-number-data)))
	 (value-list (progn (push val1 valuel) (push val2 valuel)))
	 (total 0)))
    (dolist (a value-list)
      (if (or (eql a :infinity) (eql a :-infinity))
	  (return-from %hypot *number-infinity*)
	  (if (eql a :nan)
	      (return-from %hypot *number-nan*)
	      (if (or (eql a :-0) (zerop a))
		  (identity total)
		  (incf total (* a a)))))
      (!number (sqrt total)))

(defun -math.imul (x y)
  (declare (type number-type x y))
  (let* ((a (slot-value (-to-uint32 x) '-number-data))
	 (b (slot-value (-to-uint32 y) '-number-data))
	 (product (mod (* a b) (expt 2 32))))
    (if (>= product (expt 2 31))
	(!number (- product (expt 2 32)))
	(!number product))))

(defun -math.log (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (cond ((or (eql n :nan) (< n 0) (eql n :-infinity))
	   *number-nan*)
	  ((or (zerop n) (eql n :-0))
	   *number--infinity*)
	  ((= n 1)
	   *number-0*)
	  ((eql n :infinity)
	   *number-infinity*)
	  (t
	   (!number (log n))))))

(defun -math.log1p (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (cond ((or (eql n :nan) (< n -1) (eql n :-infinity))
	   *number-nan*)
	  ((= n -1)
	   *number--infinity*)
	  ((zerop n)
	   *number-0*)
	  ((eql n :-0)
	   *number--0*)
	  ((eql n :infinity)
	   *number-infinity)
	  (t
	   (!number (log (1+ x)))))))

(defun -math.log10 (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (cond ((or (eql n :nan) (eql n :-infinity) (< n 0))
	   *number-nan*)
	  ((or (zerop n) (eql n :-0))
	   *number--infinity*)
	  ((= n 1)
	   *number-0*)
	  ((eql n :infinity)
	   *number-infinity*)
	  (t
	   (!number (log x 10))))))

(defun -math.log2 (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (cond ((or (eql n :nan) (eql n :-infinity) (< n 0))
	   *number-nan*)
	  ((or (zerop n) (eql n :-0))
	   *number--infinity*)
	  ((= n 1)
	   *number-0*)
	  ((eql n :infinity)
	   *number-infinity*)
	  (t
	   (!number (log x 2))))))

(defun -math.max (&optional value1 value2 &rest values)
  (declare (type (or null number-type) value1 value2)
	   (type list values))
  (let ((val1 (if value1 (slot-value value1 '-number-data) :-infinity))
	(val2 (if value2 (slot-value value2 '-number-data) :-infinity))
	(valuel
	 (loop for elem in values collect (slot-value elem '-number-data)))
	(value-list (progn (push val1 valuel) (pusl val2 valuel)))
	(maximum :-infinity))
    (dolist (a value-list)
      (case a
	(:nan
	 (return-from %max *number-nan*))
	(:-infinity
	 nil)
	(:infinity
	 (return-from %max *number-infinity*))
	(:-0
	 (when (or (< maximum 0) (eql maximum :-infinity))
	   (setf maximum :-0)))
	(t
	 (if (eql maximum :-infinity)
	     (setf maximum a)
	     (if (eql maximum :-0)
		 (when (>= a 0) (setf maximum a))
		 (setf maximum (max a maximum)))))))
    (!number maximum)))

(defun -math.min (&optional value1 value2 &rest values)
  (declare (type (or null number-type) value1 value2)
	   (type list values))
  (let ((val1 (if value1 (slot-value value1 '-number-data) :infinity))
	(val2 (if value2 (slot-value value2 '-number-data) :infinity))
	(valuel
	 (loop for elem in values collect (slot-value elem '-number-data)))
	(value-list (progn (push val1 valuel) (push val2 valuel)))
	(minimum :infinity))
    (dolist (a value-list)
      (case a
	(:nan
	 (return-from %min *number-nan*))
	(:infinity
	 nil)
	(:-infinity
	 (return-from %min *number--infinity*))
	(:-0
	 (when (or (>= minimum 0) (eql minimum :infinity))
	   (setf minimum :-0)))
	(t
	 (if (eql minimum :infinity)
	     (setf minimum a)
	     (if (eql minimum :-0)
		 (when (< a 0) (setf minimum a))
		 (setf minimum (min a minimum)))))))
    (!number minimum)))

(defun -math.pow (x y)
  (declare (type number-type x y))
  (let ((m (slot-value x '-number-data))
	(n (slot-value y '-number-data)))
    (cond ((eql n :nan)
	   *number-nan*)
	  ((or (zerop n) (eql n :-0))
	   (!number 1))
	  ;; We fall through, so N won't be 0.
	  ((eql m :nan)
	   *number-nan*)
	  ((and (> (abs m) 1) (eql n :infinity))
	   *number-infinity*)
	  ((and (> (abs m) 1) (eql n :-infinity))
	   *number-0*)
	  ((and (= (abs m) 1) (or (eql n :infinity) (eql n :-infinity)))
	   *number-nan*)
	  ((and (< (abs m) 1) (eql n :infinity))
	   *number-0*)
	  ((and (< (abs m) 1) (eql n :-infinity))
	   *number-infinity*)
	  ((and (eql m :infinity) (> n 0))
	   *number-infinity*)
	  ((and (eql m :infinity) (< n 0))
	   *number-infinity*)
	  ((and (eql m :-infinity) (> n 0) (oddp n))
	   *number--infinity*)
	  ((and (eql m :-infinity) (> n 0) (evenp n))
	   *number-infinity*)
	  ((and (eql m :-infinity) (< n 0) (oddp n))
	   *number--0*)
	  ((and (eql m :-infinity) (< n 0) (evenp n))
	   *number-0*)
	  ;; We are facing +/-0 problem, now ignore it.
	  ((and (zerop m) (> n 0))
	   *number-0*)
	  ((and (zerop m) (< n 0))
	   *number-infinity*)
	  ((and (eql m :-0) (> n 0) (oddp n))
	   *number--0*)
	  ((and (eql m :-0) (> n 0) (evenp n))
	   *number-0*)
	  ((and (eql m :-0) (< n 0) (oddp n))
	   *number--infinity*)
	  ((and (eql m :-0) (< n 0) (evenp n))
	   *number-infinity*)
	  ((and (< m 0)
		(eql :false (slot-value (-is-integer y) '-boolean-data)))
	   *number-nan*)
	  (t
	   (!number (expt m n))))))

(defun -math.random ()
  (!number (coerce (/ (random most-positive-fixnum) most-positive-fixnum)
		   'double-float)))

(defun -math.round (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (cond ((eql n :nan)
	   *number-nan*)
	  ((zerop n)
	   *number-0*)
	  ((eql n :-0)
	   *number--0*)
	  ((eql n :infinity)
	   *number-infinity*)
	  ((eql n :-infinity)
	   *number--infinity*)
	  ((and (< n 0) (>= n -0.5))
	   *number--0*)
	  (t
	   (!number (round n))))))

(defun -math.sign (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (cond ((eql n :nan)
	   *number-nan*)
	  ((zerop n)
	   *number-0*)
	  ((eql n :-0)
	   *number--0*)
	  ((or (< n 0) (eql n :-infinity))
	   (!number -1))
	  ((or (> n 0) (eql n :infinity))
	   (!number 1)))))

(defun -math.sin (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (case n
      ((:nan :infinity :-infinity)
       *number-nan*)
      (0
       *number-0*)
      (:-0
       *number--0*)
      (t
       (!number (sin n))))))

(defun -math.sinh (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (case n
      (:nan
       *number-nan*)
      (0
       *number-0*)
      (:-0
       *number--0*)
      (:infinity
       *number-infinity*)
      (:-infinity
       *number--infinity*)
      (t
       (!number (sinh n))))))

(defun -math.sqrt (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (cond ((or (eql n :nan) (< n 0) (eql n :-infinity))
	   *number-nan*)
	  ((zerop n)
	   *number-0*)
	  ((eql n :-0)
	   *number--0*)
	  ((eql n :infinity)
	   *number-infinity*)
	  (t
	   (!number (sqrt n))))))

(defun -math.tan (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (case n
      ((:nan :infinity :-infinity)
       *number-nan*)
      (0
       *number-0*)
      (:-0
       *number-0*)
      (t
       (!number (tan n))))))

(defun -math.tanh (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (case n
      (:nan
       *number-nan*)
      (0
       *number-0*)
      (:-0
       *number--0*)
      (:infinity
       (!number 1))
      (:-infinity
       (!number -1))
      (t
       (!number (tanh n))))))

(defun -math.trunc (x)
  (declare (type number-type x))
  (let ((n (slot-value x '-number-data)))
    (cond ((eql n :nan)
	   *number-nan*)
	  ((zerop n)
	   *number-0*)
	  ((eql n :-0)
	   *number--0*)
	  ((eql n :infinity)
	   *number-infinity*)
	  ((eql n :-infinity)
	   *number--infinity*)
	  (t	  
	   (!number (truncate n))))))
