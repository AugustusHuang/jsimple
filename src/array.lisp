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

;;; General array metaclass, all typed array will be a subclass of it.
(defclass general-array-class (standard-class)
  ((element-type :initarg :element-type)
   (min-val :initarg :min-val)
   (max-val :initarg :max-val)
   (accumulator-type :initarg :accumulator-type)))

(let ((gac (find-class 'general-array-class)))
  (defun general-array-class-p (class)
    (subtypep (class-of class) 'gac)))

(defun fill-general-array-class-slots-from-ancestors (class &rest all-keys)
  (mapcar #'(lambda (x)
              (let ((name (slot-definition-name x))
                    (initargs (slot-definition-initargs x)))
                (unless (getf (car all-keys) (car initargs))
                  (fill-slot-from-ancestor name class))))
          (general-array-class-slots class)))

(defun general-array-class-precedence-list (class)
  (remove-if-not #'(lambda (x) (general-array-class-p x))
		 (class-precedence-list class)))

(defun standard-matrix-class-slots (class)
  (let ((slots) (slot-names))
    (mapcar #'(lambda (x)
                (mapcar #'(lambda (y)
                            (unless (member (slot-definition-name y)
                                            slot-names)
                              (push y slots)
                              (push (slot-definition-name y)
                                    slot-names)))
                        (class-direct-slots (class-of x))))
            (general-array-class-precedence-list class))
    slots))

(defgeneric element-type (class)
  (:documentation "General array class element type accessor."))

(defmethod element-type ((class standard-matrix-class))
  (car (slot-value class 'element-type)))

(defgeneric accumulator-type (class)
  (:documentation "General array class function type accessor."))

(defmethod accumulator-type ((class standard-matrix-class))
  (car (slot-value class 'accumulator-type)))

(defgeneric min-val (class)
  (:documentation "General array class minimal value accessor."))
(defmethod min-val ((class standard-matrix-class))
  (car (slot-value class 'min-val)))

(defgeneric max-val (class)
  (:documentation "General array class maximal value accessor."))
(defmethod maxval ((class standard-matrix-class))
  (car (slot-value class 'maxval)))

(defmethod validate-superclass ((c1 general-array-class) (c2 standard-class))
  t)

(defmethod validate-superclass ((c1 standard-class) (c2 general-array-class))
  t)

(defun add-root-class (root-class direct-superclasses)
  (if (member root-class direct-superclasses)
      direct-superclasses
      (insert-before root-class
		     (car (class-direct-superclasses root-class))
		     direct-superclasses)))

;;; FIXME: TYPED-MIXIN is useless here... So what to do?
(defmethod initialize-instance :around ((class general-array-class)
					&rest all-keys
					&key direct-superclasses
					  &allow-other-keys)
  (let ((root-class (find-class 'typed-mixin))
	(mc (find-class 'standard-matrix-class)))
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
  (fill-standard-matrix-class-slots-from-ancestors class all-keys)
  class)

(defmethod reinitialize-instance :around ((class standard-matrix-class)
					  &rest all-keys
					  &key direct-superclasses
					    &allow-other-keys)
  (let ((root-class (find-class 'typed-mixin))
	(mc (find-class 'standard-matrix-class)))
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
  (fill-standard-matrix-class-slots-from-ancestors class all-keys)
  class)

;;; And there should be typed array definitions and methods.
