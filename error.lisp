(in-package :jsimple-error)

(define-condition general-error ()
  ()
  (:documentation "General jsimple error, will be the superclass of all."))

(define-condition lexer-error (general-error)
  ((char :initarg :char :reader lexer-error-char)
   (position :initarg :position :reader lexer-error-position))
  (:report (lambda (condition stream)
	     (format stream "Lexer error: ~S at position ~D."
		     (lexer-error-char condition)
		     (lexer-error-position condition))))
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
		     (invalid-name-name condition)
		     (invalid-name-type condition)
		     (invalid-name-funame condition))))
  (:documentation "Invalid function of an object, occurs when user wants to call a non-existing function of an object."))

