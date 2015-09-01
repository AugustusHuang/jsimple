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

;;;; Number related routines and constants, since number is native object,
;;;; it seems no need to implement number as a class.
(in-package :jsimple-builtin)

;;; In SBCL 32-bit version, it's OK.
(defconstant +number-max-safe-integer+ (- (expt 2 53) 1))
(defconstant +number-max-value+ most-positive-double-float)
(defconstant +number-min-safe-integer+ (- (- (expt 2 53) 1)))
(defconstant +number-min-value+ least-positive-double-float)
;;; XXX
(defconstant +number-epsilon+ (* 2 double-float-epsilon))

(defconstant +math-e+ (exp 1.0d0))
(defconstant +math-ln10+ (log 10.0d0))
(defconstant +math-ln2+ (log 2.0d0))
(defconstant +math-log10e+ (log +math-e+ 10))
(defconstant +math-log2e+ (log +math-e+ 2))
(defconstant +math-pi+ pi)
(defconstant +math-sqrt-1/2 (sqrt (/ 1.0d0 2.0d0)))
(defconstant +math-sqrt-2 (sqrt 2.0d0))

(deftype js-number-raw ()
  `(and double-float (member :nan :infinity :-infinity)))

(defclass js-number (js-object)
  ((constructor :reader constructor :type function
		:initarg :constructor :initform #'js-number-constructor)
   (data :accessor data :type js-number-raw
	 :initarg :data :initform 0.0d0))
  (:documentation "Builtin Number prototype."))

(defmethod js-intern-data ((this js-number))
  (slot-value this 'data))

(defun js-number-constructor (value)
  (typecase value
    (js-undefined
     (make-instance 'js-number :data :nan))
    (js-null
     (make-instance 'js-number :data 0.0d0))
    (js-boolean
     (make-instance 'js-number :data (if (js-boolean-data value)
					 1.0d0
					 0.0d0)))
    (js-number
     (make-instance 'js-number :data (js-intern-data value)))
    (js-string
     (make-instance 'js-number :data (js-string-to-number
				      (js-intern-data value))))
    (js-symbol
     (error 'js-type-error))
    (js-object
     ())))

