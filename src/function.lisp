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

(defclass -function-prototype (-object-prototype)
  ((-prototype :initform '-object-prototype)
   ;; Extensible is the same.
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
   (constructor :initform (make-property :value '-function) :allocation :class)
   (length :type (or property -null) :accessor length :allocation :class
	   :initarg :length :initform (make-property :value 0
						     :configurable :true))
   (name :type (or property -null) :accessor name :allocation :class
	 :initarg :name :initform (make-property :value ""
						 :configurable :true))
   ;; FunctionPrototype doesn't have a prototype property.
   (properties
    :initform
    (append (fetch-properties (find-class '-object-prototype))
	    '((apply . (make-property :value 'apply))
	      (bind . (make-property :value 'bind))
	      (call . (make-property :value 'call))
	      ;; 'name' property of this function is
	      ;; "[Symbol.hasInstance]".
	      (has-instance . (make-property :value 'has-instance))))
    :allocation :class))
  (:documentation "Function prototype, provides inherited properties."))

(defclass -function (-function-prototype)
  ((-prototype :initform '-function-prototype)
   ;; Extensible is the same.
   (length :allocation :class :initform (make-property :value 1
						       :configurable :true))
   (prototype :type (or property -null) :accessor prototype
	      :initarg :prototype :initform (make-property :value '-function-prototype)
	      :allocation :class)
   (properties
    :initform (fetch-properties (find-class '-function-prototype))
    :allocation :class))
  (:documentation "Function constructor, used with new operator."))

;;; Helper functions.
(defmethod fetch-own-properties ((this -function-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-own-properties ((this -function))
  (properties (make-instance (class-name this))))

(defmethod -get-prototype-of ((this -function-prototype))
  )

(defmethod -set-prototype-of ((this -function-prototype) proto)
  )

(defmethod -is-extensible ((this -function-prototype))
  )

(defmethod -prevent-extensions ((this -function-prototype))
  )

(defmethod -get-own-property ((this -function-prototype) key)
  )

(defmethod -has-property ((this -function-prototype) key)
  )

(defmethod -get ((this -function-prototype) key receiver)
  )

(defmethod -set ((this -function-prototype) key value receiver)
  )

(defmethod -delete ((this -function-prototype) key)
  )

(defmethod -define-own-property ((this -function-prototype) key descriptor)
  )

(defmethod -enumerate ((this -function-prototype))
  )

(defmethod -own-property-keys ((this -function-prototype))
  )

;;; Function prototype property methods.
(defmethod apply ((this -function-prototype) this-arg args)
  )

(defmethod bind ((this -function-prototype) this-arg &rest args)
  )

(defmethod call ((this -function-prototype) this-arg &rest args)
  )

(defmethod to-string ((this -function-prototype) &optional radix)
  (declare (ignore radix))
  )

(defmethod has-instance ((this -function-prototype) value)
  )
