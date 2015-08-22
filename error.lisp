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

;;;; All jsimple interpreter error handling function.

(in-package :jsimple-error)

(define-condition general-error ()
  ()
  (:documentation "General jsimple error, will be the superclass of all."))

(define-condition lexer-error (general-error)
  ((char :initarg :char :reader lexer-error-char)
   (line :initarg :line :reader lexer-error-line))
  (:report (lambda (condition stream)
	     (format stream "Lexer error: ~S at line ~D."
		     (lexer-error-char condition)
		     (lexer-error-line condition))))
  (:documentation "Jsimple lexer error."))

(define-condition parser-error (general-error)
  (())
  (:documentation "Jsimple parser error."))

;;; Since this is a really simple interpreter, don't expect too much optimize,
;;; and almost all error will be reported as runtime error since it may not
;;; be modified.
(define-condition runtime-error (general-error)
  ()
  (:documentation "Jsimple runtime error."))

(define-condition arithmetic-error (runtime-error)
  ((operator :initarg :operator :reader arithmetic-error-operator)
   (operands :initarg :operands :reader arithmetic-error-operands))
  (:report (lambda (condition stream)
	     (format stream "Arithmetic error: operator ~A, operands ~A."
		     (arithmetic-error-operator condition)
		     (arithmetic-error-operands condition))))
  (:documentation "Arithmetic error, occurs when trying to do silly arithmetic or invalid operands."))

(define-condition divide-by-0-error (runtime-error)
  ()
  (:documentation "Division by 0 error, occurs when trying to divide a number by 0."))

(define-condition invalid-index (runtime-error)
  ((name :initarg :name :reader invalid-index-name)
   (index :initarg :index :reader invalid-index-index)
   (type :initarg :type :reader invalid-index-type))
  (:report (lambda (condition stream)
	     ;; Since index maybe a non-number...
	     (format stream "Invalid index error: object ~S of type ~A, index ~A."
		     (invalid-index-name condition)
		     (invalid-index-type condition)
		     (invalid-index-index condition))))
  (:documentation "Invalid index of an object, occurs when user wants to access a non-existing slot of an array or string."))

(define-condition invalid-property (runtime-error)
  ((name :initarg :name :reader invalid-name-name)
   (property :initarg :property :reader invalid-name-property))
  (:report (lambda (condition stream)
	     (format stream "Invalid property error: object ~S, property ~A."
		     (invalid-name-name condition)
		     (invalid-name-property condition))))
  (:documentation "Invalid property of an object, occurs when user wants to access a non-existing property of an object."))

(define-condition invalid-function (runtime-error)
  ((name :initarg :name :reader invalid-function-name)
   (funame :initarg :funame :reader invalid-function-funame)
   (type :initarg :type :reader invalid-function-type))
  (:report (lambda (condition stream)
	     (format stream "Invalid function error: object ~S of type ~A, function ~S."
		     (invalid-function-name condition)
		     (invalid-function-type condition)
		     (invalid-function-funame condition))))
  (:documentation "Invalid function of an object, occurs when user wants to call a non-existing or out of scope function of an object."))

(define-condition invalid-object (runtime-error)
  ((name :initarg :name :reader invalid-object-name))
  (:report (lambda (condition stream)
	     (format stream "Invalid object error: object ~S not defined."
		     (invalid-object-name condition))))
  (:documentation "Invalid object, occurs when user wants to access or refer a non-existing or out of scope object."))

;;; Though we can represent this number as NaN, signal it to the user.
(define-condition number-too-large (runtime-error)
  ((number :initarg :number :reader number-too-large-number))
  (:report (lambda (condition name)
	     (format stream "Number too larget: ~D."
		     (number-too-large-number condition))))
  (:documentation "Too large number, occurs when user wants to use a number larger than maximum."))

