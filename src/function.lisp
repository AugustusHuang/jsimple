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

(in-package :lesp-builtin)

;;; Since naming style is quite different in Lisp and ES,
;;; functions like Another and another will be the same...
;;; But they are different, if use thisIsAFunctionStyle, it will
;;; be parsed into this-is-a-function-style in Lisp, and if use ThisType,
;;; it will be parsed into !this-type, and then the problem will be trivial...
;;; See builtin-util.lisp

(defclass -function-proto (builtin-function)
  ((proto :initform (find-class '-object-proto))
   (-prototype :initform (find-class '-function-proto))
   (-extensible :initform :true)
   ;; Internal slots of function objects:
   ;; The lexical environment that the function was closed over. Used as the
   ;; outer environment when evaluating the code of the function.
   (-environment :initarg :-environment)
   ;; The root parse node of the source text that defines the function's
   ;; formal parameter list.
   (-formal-parameters :initarg :-formal-parameters)
   ;; Either "normal" "classConstructor" or "generator"
   (-function-kind :type string-raw :initarg :-function-kind)
   ;; The root parse node of the source text that defines the function's body.
   (-script-code :initarg :-script-code)
   ;; Either "base" or "derived".
   (-constructor-kind :type string-raw :initarg :-constructor-kind)
   ;; The code realm in which the function was created and which provides any
   ;; intrinsic objects that are accessed when evaluating the function.
   (-realm :initarg :realm)
   ;; Defineds how 'this' reference are interpreted within the formal
   ;; parameters and code body of the function. :LEXICAL means 'this' refers
   ;; to the 'this' value of a lexically enclosing function. :STRICT means
   ;; the 'this' value is used exactly as provided by an invocation of the
   ;; function. :GLOBAL means a 'this' value of :UNDEFINED is interpreted as a
   ;; reference to the global object.
   (-this-mode :type (member :lexical :strict :global) :initarg :-this-mode)
   ;; :TRUE if this is a strict mode function, :FALSE if this is not.
   (-strict :type boolean-raw :initarg :-strict)
   ;; If the function uses 'super' this is the object whose [[GetPrototypeOf]]
   ;; provides the object where 'super' property lookups begin.
   (-home-object :type symbol-raw :initarg :-home-object)
   (-call :type (or object-raw undefined-raw nil) :initarg :-call)
   (-construct :type (or object-raw undefined-raw nil) :initarg :-construct)
   (length :initform (make-property :value 0
				    :configurable :true))
   (name :initform (make-property :value ""
				  :configurable :true))
   (constructor :initform -function :allocation :class)
   (prototype :initform nil)
   (has-own-property :type property :allocation :class
		     :initform (make-property :value '!has-own-property))
   (is-prototype-of :type property :allocation :class
		    :initform (make-property :value '!is-prototype-of))
   (property-is-enumerable :type property :allocation :class
			   :initform
			   (make-property :value '!property-is-enumerable))
   (to-locale-string :type property :allocation :class
		     :initform (make-property :value '!to-locale-string))
   (to-string :type property :allocation :class
	      :initform (make-property :value '!to-string))
   (value-of :type property :allocation :class
	     :initform (make-property :value '!value-of))
   (apply :type property :allocation :class
	  :initform (make-property :value !apply))
   (bind :type property :allocation :class
	 :initform (make-property :value !bind))
   (call :type property :allocation :class
	 :initform (make-property :value !call)))
  (:metaclass funcallable-standard-class)
  (:documentation "Function prototype, provides inherited properties."))

(defmethod -get-prototype-of ((this -function-proto))
  )

(defmethod -set-prototype-of ((this -function-proto) proto)
  )

(defmethod -is-extensible ((this -function-proto))
  )

(defmethod -prevent-extensions ((this -function-proto))
  )

(defmethod -get-own-property ((this -function-proto) key)
  )

(defmethod -has-property ((this -function-proto) key)
  )

(defmethod -get ((this -function-proto) key receiver)
  )

(defmethod -set ((this -function-proto) key value receiver)
  )

(defmethod -delete ((this -function-proto) key)
  )

(defmethod -define-own-property ((this -function-proto) key descriptor)
  )

(defmethod -enumerate ((this -function-proto))
  )

(defmethod -own-property-keys ((this -function-proto))
  )

;;; Function prototype property methods. We have to reimplement the methods
;;; 'inherited' from object prototype, since they are of different metaclass
;;; indeed.
(defmethod %apply ((this -function-proto) this-arg args)
  )

(defmethod %bind ((this -function-proto) this-arg &rest args)
  )

(defmethod %call ((this -function-proto) this-arg &rest args)
  )

(defmethod %has-own-property ((this -object-proto) value)
  (let ((p (-to-property-key value))
	(o (-to-object this)))
    (if (eql (-get-own-property o p) :undefined)
	*boolean-false*
	*boolean-true*)))

(defmethod %is-prototype-of ((this -object-proto) value)
  (if (not (eql (-type this) 'object-type))
      *boolean-false*
      (loop for value = (-get-prototype-of value)
	 if (eql value :null)
	 do (return-from %is-prototype-of *boolean-false*)
	 ;; XXX: What does the standard mean? The instance itself as a
	 ;; prototype or the class object? -- Augustus, 18 Sep 2015.
	 if (eql value (class-of this))
	 do (return-from %is-prototype-of *boolean-true*))))

(defmethod %property-is-enumerable ((this -object-proto) value)
  (let* ((p (-to-property-key value))
	 (o (-to-object this))
	 (desc (-get-own-property o p)))
    (if (eql desc :undefined)
	*boolean-false*
	(if (eql (property-enumerable desc) :false)
	    *boolean-false*
	    *boolean-true*))))

(defmethod %to-locale-string ((this -object-proto))
  (%to-string this))

(defmethod %to-string ((this -function-proto) &optional radix)
  (declare (ignore radix))
  )

(defmethod %value-of ((this -function-proto))
  this)

;;; This is a global symbol, name it special.
(defmethod -symbol.has-instance ((this -function-proto) value)
  )
