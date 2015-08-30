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

(in-package :jsimple-builtin)

;;; Array related builtin functions.
(deftype array-index-numeric () `(integer 0 ,(- (expt 2 32) 2)))

;;; Numeric array metaclass, all typed array will be a subclass of it.
;;; Ideas from CLEM package by Cyrus Harmon.
(defclass numeric-array-class (standard-class)
  ((element-type :initarg :element-type)
   (min-val :initarg :min-val)
   (max-val :initarg :max-val)
   (accumulator-type :initarg :accumulator-type)))

(let ((gac (find-class 'numeric-array-class)))
  (defun numeric-array-class-p (class)
    (subtypep (class-of class) 'gac)))

(defun fill-numeric-array-class-slots-from-ancestors (class &rest all-keys)
  (mapcar #'(lambda (x)
              (let ((name (slot-definition-name x))
                    (initargs (slot-definition-initargs x)))
                (unless (getf (car all-keys) (car initargs))
                  (fill-slot-from-ancestor name class))))
          (numeric-array-class-slots class)))

(defun numeric-array-class-precedence-list (class)
  (remove-if-not #'(lambda (x) (numeric-array-class-p x))
		 (class-precedence-list class)))

(defun numeric-array-class-slots (class)
  (let ((slots) (slot-names))
    (mapcar #'(lambda (x)
                (mapcar #'(lambda (y)
                            (unless (member (slot-definition-name y)
                                            slot-names)
                              (push y slots)
                              (push (slot-definition-name y)
                                    slot-names)))
                        (class-direct-slots (class-of x))))
            (numeric-array-class-precedence-list class))
    slots))

(defgeneric element-type (class)
  (:documentation "Numeric array class element type accessor."))

(defmethod element-type ((class numeric-array-class))
  (car (slot-value class 'element-type)))

(defgeneric accumulator-type (class)
  (:documentation "Numeric array class function type accessor."))
(defmethod accumulator-type ((class numeric-array-class))
  (car (slot-value class 'accumulator-type)))

(defgeneric min-val (class)
  (:documentation "General array class minimal value accessor."))
(defmethod min-val ((class numeric-array-class))
  (car (slot-value class 'min-val)))

(defgeneric max-val (class)
  (:documentation "General array class maximal value accessor."))
(defmethod maxval ((class numeric-array-class))
  (car (slot-value class 'maxval)))

(defmethod validate-superclass ((c1 numeric-array-class) (c2 standard-class))
  t)

(defmethod validate-superclass ((c1 standard-class) (c2 numeric-array-class))
  t)

(defun add-root-class (root-class direct-superclasses)
  (if (member root-class direct-superclasses)
      direct-superclasses
      (insert-before root-class
		     (car (class-direct-superclasses root-class))
		     direct-superclasses)))

;;; FIXME: TYPED-MIXIN is useless here... So what to do?
(defmethod initialize-instance :around ((class numeric-array-class)
					&rest all-keys
					&key direct-superclasses
					  &allow-other-keys)
  (let ((root-class (find-class 'typed-mixin))
	(mc (find-class 'numeric-array-class)))
    (if (and root-class (not (equal class root-class)))
	(if (member-if #'(lambda (super)
			   (eq (class-of super) mc)) direct-superclasses)
	    (call-next-method)
            (apply #'call-next-method class
                   :direct-superclasses
                   (add-root-class root-class direct-superclasses)
                   (remove-keyword-arg all-keys :direct-superclasses)))
	(call-next-method)))
  (finalize-inheritance class)
  (fill-numeric-array-class-slots-from-ancestors class all-keys)
  class)

(defmethod reinitialize-instance :around ((class numeric-array-class)
					  &rest all-keys
					  &key direct-superclasses
					    &allow-other-keys)
  (let ((root-class (find-class 'typed-mixin))
	(mc (find-class 'numeric-array-class)))
    (if (and root-class (not (equal class root-class)))
	(if (member-if #'(lambda (super)
			   (eq (class-of super) mc)) direct-superclasses)
	    (call-next-method)
	    (apply #'call-next-method class
		   :direct-superclasses
		   (add-root-class root-class direct-superclasses)
		   (remove-keyword-arg all-keys :direct-superclasses)))
	(call-next-method)))
  (finalize-inheritance class)
  (fill-numeric-array-class-slots-from-ancestors class all-keys)
  class)

;;; And there should be typed array definitions and methods.
;;; General js-general-array, will be the superclass of all numeric arrays.
(defclass js-general-array ()
  ((array :accessor array)
   (length :initarg :length :initform 1 :type array-index-numeric)
   (initial-element :accessor initial-element
		    :initarg :initial-element :initform 0d0)
   ;; Enable those two by default?
   (adjustable :accessor adjustable :initarg :adjustable :initform nil)
   (resizeable :accessor resizable :initform nil))
  (:metaclass numeric-array-class)
  (:element-type double-float)
  (:min-val nil)
  (:max-val nil))

(defmacro def-array-class (type direct-superclasses &key 
						      (element-type)
						      (accumulator-type)
						      (initial-element)
						      min-val max-val)
  (unless direct-superclasses (setf direct-superclasses '(js-general-array)))
  `(progn
     (defclass ,type ,direct-superclasses
       ((initial-element :accessor initial-element
                         :initarg :initial-element :initform ,initial-element))
       (:metaclass numeric-array-class)
       ,@(when element-type
	       `((:element-type ,(delistify element-type))))
       ,@(when accumulator-type
	       `((:accumulator-type ,(delistify accumulator-type))))
       ,@(when min-val
	       `((:min-val ,(if (symbolp min-val)
				(symbol-value min-val) min-val))))
       ,@(when max-val
	       `((:max-val ,(if (symbolp max-val)
				(symbol-value max-val) max-val)))))))

(def-array-class js-array ()
  :element-type t
  :accumulator-type t)

(def-array-class js-number-array (js-array)
  :element-type number
  :accumulator-type number)

(def-array-class js-real-array (js-number-array)
  :element-type real
  :accumulator-type real)

(def-array-class js-float-array (js-real-array)
  :element-type float
  :accumulator-type float)

(def-array-class js-integer-array (js-real-array)
  :element-type integer
  :accumulator-type integer)

(def-array-class js-ubyte-array (js-integer-array)
  :element-type (unsigned-byte *)
  :accumulator-type (unsigned-byte *))

(def-array-class js-uint8-array (js-ubyte-array)
  :element-type (unsigned-byte 8)
  :accumulator-type (unsigned-byte 32)
  :minval 0
  :maxval #.(- (expt 2 8) 1))

(def-array-class js-uint16-array (js-ubyte-array)
  :element-type (unsigned-byte 16)
  :accumulator-type (unsigned-byte 32)
  :minval 0
  :maxval #.(- (expt 2 16) 1))

(def-array-class js-uint32-array (js-ubyte-array)
  :element-type (unsigned-byte 32)
  :accumulator-type (unsigned-byte 32)
  :minval 0
  :maxval #.(- (expt 2 32) 1))

(def-array-class js-int8-array (js-integer-array)
  :element-type (signed-byte 8)
  :accumulator-type (signed-byte 32)
  :minval #.(- (expt 2 7))
  :maxval #.(- (expt 2 7) 1))

(def-array-class js-int16-array (js-integer-array)
  :element-type (signed-byte 16)
  :accumulator-type (signed-byte 32)
  :minval #.(- (expt 2 15))
  :maxval #.(- (expt 2 15) 1))

(def-array-class js-int32-array (js-integer-array)
  :element-type (signed-byte 32)
  :accumulator-type (signed-byte 32)
  :minval #.(- (expt 2 31))
  :maxval #.(- (expt 2 31) 1))

(def-array-class js-float32-array (js-float-array)
  :element-type single-float
  :accumulator-type single-float
  :initial-element 0f0
  :minval most-negative-single-float
  :maxval most-positive-single-float)

(def-array-class js-float64-array (js-float-array)
  :element-type double-float
  :accumulator-type double-float
  :initial-element 0d0
  :minval most-negative-double-float
  :maxval most-positive-double-float)

;;; All internal ECMA utility functions on arrays.
;;; FIXME: VALUES can't be passed this way.
(defun js-array-build (&rest values)
  (make-instance 'js-array :array values))

(defun js-int8-array-build (&rest values)
  (make-instance 'js-int8-array values))

(defun js-int16-array-build (&rest values)
  (make-instance 'js-int16-array values))

(defun js-int32-array-build (&rest values)
  (make-instance 'js-int32-array values))

(defun js-float32-array-build (&rest values)
  (make-instance 'js-float32-array values))

(defun js-float64-array-build (&rest values)
  (make-instance 'js-float64-array values))

(defun js-uint8-array-build (&rest values)
  (make-instance 'js-uint8-array values))

(defun js-uint8-clamped-array-build (&rest values)
  (make-instance 'js-uint8-clamped-array values))

(defun js-uint16-array-build (&rest values)
  (make-instance 'js-uint16-array values))

(defun js-uint32-array-build (&rest values)
  (make-instance 'js-uint32-array values))
