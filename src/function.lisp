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

;;; Every function has its own symbol, once created, store it into a global
;;; hashtable in order to prevent duplicates. (Since naming style is quite
;;; different in Lisp and ES, functions like Another and another will be the
;;; same... But they are different, if use thisIsAFunctionStyle, it will
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
   (-function-kind :initarg :-function-kind)
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
   (-this-mode :type (or :lexical :strict :global) :initarg :-this-mode)
   ;; :TRUE if this is a strict mode function, :FALSE if this is not.
   (-strict :type boolean-raw :initarg :-strict)
   ;; If the function uses 'super' this is the object whose [[GetPrototypeOf]]
   ;; provides the object where 'super' property lookups begin.
   (-home-object :type symbol :initarg :-home-object)
   (constructor :initform (make-property :value '-function))
   (length :type (or property -null) :accessor length
	   :initarg :length :initform (make-property :value 0))
   (name :type (or property -null) :accessor name
	 :initarg :name :initform (make-property :value ""))
   (own-properties
    :initform '((apply . (make-property :value 'apply))
		(bind . (make-property :value 'bind))
		(call . (make-property :value 'call))
		;; 'name' property of this function is
		;; "[Symbol.hasInstance]".
		(has-instance . (make-property :value 'has-instance))))
   (inherit-properties
    :initform (fetch-own-properties (find-class '-object-prototype))))
  (:documentation "Function prototype, provides inherited properties."))

(defclass -function ()
  ((-prototype :type (or symbol -null)
	       :initarg :-prototype :initform '-function-prototype)
   (-extensible :type (or boolean-raw -undefined)
		:initarg :-extensible :initform :true)
   (length :type (or property -null) :accessor length
	   :initarg :length :initform (make-property :value 1
						     :configurable :true))
   (prototype :type (or property -null) :accessor prototype
	      :initarg :prototype :initform (make-property :value '-function-prototype))
   ())
  (:documentation "Function constructor, used with new operator."))

;;; Helper functions.
(defmethod fetch-own-properties ((this -function-prototype))
  (own-properties (make-instance (class-name this))))

(defmethod fetch-own-properties ((this -function))
  (own-properties (make-instance (class-name this))))

(defmethod fetch-inherit-properties ((this -function-prototype))
  (inherit-properties (make-instance (class-name this))))

(defmethod fetch-inherit-properties ((this -function))
  (inherit-properties (make-instance (class-name this))))

