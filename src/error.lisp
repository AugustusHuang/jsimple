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

;;;; All lesp interpreter error handling function.
;;;; NOTE: Some of them have been moved to related file in order to prevent
;;;; appearence of compile-warning. Make it silent!
(in-package :lesp-error)

(define-condition general-error ()
  ()
  (:documentation "General lesp error, will be the superclass of all."))

(define-condition ir-error (general-error)
  ((tree-pos :initarg :tree-pos :reader :ir-error-tree-pos))
  (:documentation "Jsimple IR error."))

(defmethod print-object ((err ir-error) stream)
  (call-next-method)
  (format stream "ir error in js tree ~A."
	  (ir-error-tree-pos err)))

(defun ir-error (control &rest args)
  (error 'ir-error :format-control control :format-arguments args))

;;; Since this is a really simple interpreter, don't expect too much optimize,
;;; and almost all error will be reported as runtime error since it may not
;;; be modified.
(define-condition runtime-error (general-error)
  ()
  (:documentation "Jsimple runtime error."))

(defmethod print-object ((err runtime-error) stream)
  (call-next-method)
  (format stream "runtime error."))

(defun runtime-error (control &rest args)
  (error 'runtime-error :format-control control :format-arguments args))

;;; ARITHMETIC-ERROR is a dirty name...
(define-condition jarithmetic-error (runtime-error)
  ((operator :initarg :operator :reader jarithmetic-error-operator)
   (operands :initarg :operands :reader jarithmetic-error-operands))
  (:documentation "Arithmetic error, occurs when trying to do silly arithmetic or invalid operands."))

(defmethod print-object ((err jarithmetic-error) stream)
  (call-next-method)
  (format stream "arithmetic error operator ~A operands ~A."
	  (jarithmetic-error-operator err)
	  (jarithmetic-error-operands err)))

(defun jarithmetic-error (control &rest args)
  (error 'jarithmetic-error :format-control control :format-arguments args))

(define-condition divide-by-0-error (runtime-error)
  ()
  (:documentation "Division by 0 error, occurs when trying to divide a number by 0."))

(defmethod print-object ((err divide-by-0-error) stream)
  (call-next-method)
  (format stream "divide by 0 error."))

(defun divide-by-0-error (control &rest args)
  (error 'divide-by-0-error :format-control control :format-arguments args))

(define-condition invalid-index-error (runtime-error)
  ((name :initarg :name :reader invalid-index-error-name)
   (index :initarg :index :reader invalid-index-error-index)
   (type :initarg :type :reader invalid-index-error-type))
  (:documentation "Invalid index of an object, occurs when user wants to access a non-existing slot of an array or string."))

(defmethod print-object ((err invalid-index-error) stream)
  (call-next-method)
  (format stream "invalid index error object ~S of type ~A index ~A."
	  (invalid-index-error-name err)
	  (invalid-index-error-type err)
	  (invalid-index-error-index err)))

(defun invalid-index-error (control &rest args)
  (error 'invalid-index-error :format-control control :format-arguments args))

(define-condition invalid-property-error (runtime-error)
  ((name :initarg :name :reader invalid-property-error-name)
   (property :initarg :property :reader invalid-property-error-property))
  (:documentation "Invalid property of an object, occurs when user wants to access a non-existing property of an object."))

(defmethod print-object ((err invalid-property-error) stream)
  (call-next-method)
  (format stream "invalid property error object ~S property ~A."
	  (invalid-property-error-name err)
	  (invalid-property-error-property err)))

(defun invalid-property-error (control &rest args)
  (error 'invalid-property-error :format-control control :format-arguments args))

(define-condition invalid-function-error (runtime-error)
  ((name :initarg :name :reader invalid-function-error-name)
   (funame :initarg :funame :reader invalid-function-error-funame)
   (type :initarg :type :reader invalid-function-error-type))
  (:documentation "Invalid function of an object, occurs when user wants to call a non-existing or out of scope function of an object."))

(defmethod print-object ((err invalid-function-error) stream)
  (call-next-method)
  (format stream "invalid function error object ~S of type ~A function ~S."
	  (invalid-function-error-name err)
	  (invalid-function-error-type err)
	  (invalid-function-error-funame err)))

(defun invalid-function-error (control &rest args)
  (error 'invalid-function-error :format-control control :format-arguments args))

(define-condition invalid-object-error (runtime-error)
  ((name :initarg :name :reader invalid-object-error-name))
  (:documentation "Invalid object, occurs when user wants to access or refer a non-existing or out of scope object."))

(defmethod print-object ((err invalid-object-error) stream)
  (call-next-method)
  (format stream "invalid object error object ~S not defined."
	  (invalid-object-error-name err)))

(defun invalid-object-error (control &rest args)
  (error 'invalid-object-error :format-control control :format-arguments args))

;;; Though we can represent this number as NaN, signal it to the user.
(define-condition number-too-large-error (runtime-error)
  ((number :initarg :number :reader number-too-large-error-number))
  (:documentation "Too large number, occurs when user wants to use a number larger than maximum."))

(defmethod print-object ((err number-too-large-error) stream)
  (call-next-method)
  (format stream "number too large error ~D."
	  (number-too-large-error-number err)))

(defun number-too-large-error (control &rest args)
  (error 'number-too-large-error :format-control control :format-arguments args))

