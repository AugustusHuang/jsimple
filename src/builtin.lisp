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

;;;; Directly translate builtin methods into Common Lisp code,
;;;; since these functions will be functions belong to prototypes,
;;;; they are typed functions, so they will be efficient and don't need
;;;; further optimization.
(in-package :lesp-builtin)

;;;; -UNDEFINED, -NULL are trivial, use the keywords.
(defun undefined-p (value)
  (eql value :undefined))
(deftype undefined-raw ()
  `(satisfies undefined-p))

(defun null-p (value)
  (eql value :null))
(deftype null-raw ()
  `(satisfies null-p))

(deftype boolean-raw ()
  `(member :true :false))
(deftype number-raw ()
  `(or double-float integer (member :nan :infinity :-infinity)))
(deftype string-raw ()
  'string)
(deftype symbol-raw ()
  'symbol)
(deftype function-raw ()
  '-function-proto)
(deftype object-raw ()
  `(or -object-proto -function-proto))

(deftype +js-value-types+ ()
  `(or undefined-raw null-raw boolean-raw number-raw symbol-raw string-raw
       object-raw))

(deftype +js-primitive-value-types+ ()
  `(or undefined-raw null-raw boolean-raw number-raw symbol-raw string-raw))

;;; Integer index is a canonical numeric string value in [0, 2^53-1],
;;; array index is an integer index in [0, 2^32-1>. -- ECMA V6.
(deftype integer-index-numeric () `(integer 0 ,(- (expt 2 53) 1)))

;;; Interface of type.
(deftype undefined-type () 'undefined-raw)
(deftype null-type () 'null-raw)
(deftype boolean-type () '-boolean-proto)
(deftype number-type () '-number-proto)
(deftype string-type () '-string-proto)
(deftype symbol-type () '-symbol-proto)
(deftype function-type () '-function-proto)
(deftype object-type () `(and (or -object-proto -function-proto)
			      (not -boolean-proto)
			      (not -number-proto)
			      (not -string-proto)
			      (not -symbol-proto)))

(deftype +js-types+ ()
  `(or undefined-type null-type boolean-type number-type string-type
       symbol-type object-type))

;;; In an object, property list is an associative list with CARs keys (symbols
;;; or strings) and CDRs property structures.
(defstruct property
  ;; The value retrieved by a get access of the property.
  (value :undefined :type (or +js-value-types+ function-raw null))
  ;; If not :UNDEFINED must be a function object, in this implementation,
  ;; function is an object, but has parallel status, so there's no way to
  ;; be both function and object. The function's [[Call]] method is called
  ;; with an empty argument list to retrieve the property value each time
  ;; a get access of the property is performed.
  (get nil :type (or undefined-raw function-raw null))
  ;; If not :UNDEFINED must be a function object, the function's [[Call]]
  ;; method is called with an argument list containing assigned value,
  ;; assigns the property with this argument.
  (set nil :type (or undefined-raw function-raw null))
  ;; If :FALSE, attempts to change [[Value]] attribute using [[Set]] failes.
  (writable :false :type (or boolean-raw null))
  ;; If :TRUE, the property will be enumerable by a for-in.
  (enumerable :false :type boolean-raw)
  ;; If :FALSE, attempts to delete the property, change the property to be
  ;; an accessor property, or change its attributes will fail.
  (configurable :false :type boolean-raw))

;;; In all classes, NIL means this slot is not present in this instance,
;;; it is present as a slot only because of it's possible to have this
;;; slot non-NIL. :UNDEFINED means this slot is present but not initialized,
;;; and others mean their own corresponding meanings.
;;; NOTE: Here we change the status, PROTO means the 'real' proto, -PROTOTYPE
;;; means the instance proto, and PROTOTYPE will only appear in functions.
;;; e.g. 123, as a number, is an instance of class NUMBER-PROTO, which has
;;; superclass OBJECT-PROTO, so its PROTO slot will be OBJECT-PROTO, its
;;; -PROTOTYPE slot will be NUMBER-PROTO, and it won't have a PROTOTYPE slot.
(defclass proto ()
  ((proto :type (or object-raw null-raw null) :initarg :proto :initform nil
	  :allocation :class)
   (-prototype :type (or object-raw null) :initarg :-prototype :initform nil)
   (-extensible :type (or boolean-raw undefined-raw null) :initarg :-extensible
		:initform nil)
   (-primitive-value :type (or +js-primitive-value-types+ null)
		     :initarg :-primitive-value :initform nil)
   (constructor :type (or object-raw null-raw null) :initarg :constructor
		:allocation :class :accessor constructor :initform nil)
   (properties :type list :initarg :properties :accessor properties
	       :initform nil))
  (:documentation "General prototype class, used as a helper basis class,
it is implementation specific."))

(defclass builtin-function ()
  ((proto :type (or object-raw null) :initarg :proto :initform nil
	  :allocation :class)
   (-prototype :type (or object-raw null) :initarg :-prototype
	       :initform nil)
   (-extensible :type (or boolean-raw undefined-raw null) :initarg :-extensible
		:initform nil)
   ;; LENGTH and NAME are present no matter in which function, make them
   ;; outstanding. Only globally inherited properties will be present.
   (length :type (or property null) :initarg :length :accessor length
	   :initform nil)
   (name :type (or property null) :initarg :name :accessor name
	 :initform nil)
   (constructor :type (or property null) :initarg :constructor
		:accessor constructor :allocation :class :initform nil)
   ;; Only ObjectPrototype has a :NULL prototype.
   (prototype :type (or property null) :initarg :prototype :accessor prototype
	      :initform nil)
   (properties :type list :initarg :properties
	       :accessor properties :initform nil))
  (:metaclass funcallable-standard-class)
  (:documentation "Builtin function prototype class, used as a helper
funcallable class, it is implementation specific."))

;;; All the funcallable class should have at least one instance,
;;; all funcallable instances acts like a wrapper, which contains properties
;;; information.
(defmethod initialize-instance :after ((func -function-proto) &key)
  (with-slots (name) func
    (let ((fun (symbol-function
		;; NAME is ThisStyle, cast to THIS-STYLE and find the function.
		(string-upcase (camel-to-hyphen (property-value name))))))
      (if (eql (type-of (symbol-function name)) 'function)
	  ;; The type of the function is a general function.
	  (let ((arg-list (sb-introspect:function-lambda-list fun)))
	    (set-funcallable-instance-function
	     func
	     (eval `(function (lambda ,arg-list
		      (,name ,(remove-& arg-list)))))))
	  ;; The type of the function is standard generic function.
	  (let ((arg-list (generic-function-lambda-list fun)))
	    (set-funcallable-instance-function
	     func
	     (eval `(function (lambda ,arg-list
		      (,name ,(remove-& arg-list)))))))))))

;;; THIS is an instance of internal type.
(defun find-property (this key)
  (let ((slots (class-slots (class-of this))))
    (typecase key
      (symbol
       ;; Firstly try to find as a outer inherited property.
       (loop for i in slots
	  if (eql key (slot-definition-name i))
	  do (return-from find-property (cons key (slot-value this key))))
       ;; Not found, try to find as a inner property.
       (let ((assoc-list (slot-value this 'properties)))
	 (if (assoc key assoc-list)
	     (assoc key assoc-list)
	     :undefined)))
      (string
       ;; They are fixed, simply case KEY to upper-case and compare.
       (loop for i in slots
	  if (string= (symbol-name (slot-definition-name i))
		      (string-upcase key))
	  do (return-from find-property
	       (cons key (slot-value this (slot-definition-name i)))))
       ;; Not found, then find the exact string property name.
       (let ((assoc-list (slot-value this 'properties)))
	 (if (assoc key assoc-list)
	     (assoc key assoc-list)
	     :undefined))))))

;;; Remove property function.
(defun remove-property (this key)
  (let ((assoc-list (slot-value this 'properties)))
    ;; We can't remove outer properties, only do it in inner ones.
    (rplacd (assoc key assoc-list) nil)))

;;; NOTE: Do we need this?
(defun clean-up-properties (this)
  (let ((assoc-list (slot-value this 'properties)))
    (loop for i in assoc-list
       if (null (cdr i))
       do (setf assoc-list (remove i assoc-list)))))

(defun -type (arg)
  (typecase arg
    (undefined-type
     'undefined-type)
    (null-type
     'null-type)
    (boolean-type
     'boolean-type)
    (number-type
     'number-type)
    (string-type
     'string-type)
    (symbol-type
     'symbol-type)
    (function-type
     'function-type)
    ;; Function is an object, but it is more exact.
    (object-type
     'object-type)
    (t
     (error "Invalid argument"))))

(defun -to-primitive (arg &key (hint 'default))
  (assert (eql (-type arg) 'object-type)
	  (arg hint)
	  "Type of ARG is not object.")
  (let ((h hint)
	(result nil))
    (when (and (not (eql h 'string))
	       (not (eql h 'number)))
      (error "HINT must be STRING, NUMBER or DEFAULT."))
    (case h
      ('string
       (setf result (!to-string arg)))
      ((number default)
       (setf result (!value-of arg))))
    (if (not (eql (-type arg) 'object-type))
	result
	(error ""))))

(defun -is-array (arg)
  (when (not (eql (-type arg) 'object-type))
    (return-from -is-array *boolean-false*))
  ;; Do it in lisp level.
  (cond ((typep arg '-array-proto)
	 *boolean-true*)
	((typep arg '-proxy-proto)
	 (if (eql :null (slot-value arg '-proxy-handler))
	     (error "")
	     (-is-array (slot-value arg '-proxy-target))))
	(t
	 *boolean-false*)))

(defun -is-callable (arg)
  (when (not (eql (-type arg) 'object-type))
    (return-from -is-callable *boolean-false*))
  (if (null (slot-value arg '-call))
      *boolean-false*
      *boolean-true*))

(defun -is-constructor (arg)
  (when (not (eql (-type arg) 'object-type))
    (return-from -is-constructor *boolean-false*))
  (if (null (slot-value arg '-construct))
      *boolean-false*
      *boolean-true*))

(defun -is-integer (arg)
  (when (not (eql (type-of arg) '-number-proto))
    (-boolean :false))
  (let ((data (slot-value arg '-number-data)))
    (if (or (eql :nan data)
	    (eql :infinity data)
	    (eql :-infinity data))
	(-boolean :false)
	(when (/= (floor (abs data)) (abs data))
	  (-boolean :false)))
    (-boolean :true)))

(defun -is-property-key (arg)
  (if (or (eql (-type arg) 'string-type)
	  (eql (-type arg) 'symbol-type))
      *boolean-true*
      *boolean-false*))

(defun -is-reg-exp (arg)
  )

;;; Internal methods will have name camel-to-hyphen ed. Constructor instances
;;; will have name camel-to-hyphen ed too, but since built-in function should
;;; have a instance of -FUNCTION-PROTO without using the duplicated symbol,
;;; add ! as a prefix.
(setf !.eval
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value ".eval")
		     :length (make-property :value 1))
      !.is-finite
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value ".isFinite")
		     :length (make-property :value 1))
      !.is-nan
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value ".isNaN")
		     :length (make-property :value 1))
      !.parse-float
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value ".parseFloat")
		     :length (make-property :value 1))
      !.parse-int
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value ".parseInt")
		     :length (make-property :value 2))
      !.decode-uri
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value ".decodeURI")
		     :length (make-property :value 1))
      !.decode-uri-component
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value ".decodeURIComponent")
		     :length (make-property :value 1))
      !.encode-uri
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value ".encodeURI")
		     :length (make-property :value 1))
      !.encode-uri-component
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value ".encodeURIComponent")
		     :length (make-property :value 1)))

(setf !object.assign
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Object.assign")
		     :length (make-property :value 2))
      !object.create
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Object.create")
		     :length (make-property :value 2))
      !object.define-properties
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Object.defineProperties")
		     :length (make-property :value 2))
      !object.define-property
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Object.defineProperty")
		     :length (make-property :value 3))
      !object.freeze
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Object.freeze")
		     :length (make-property :value 1))
      !object.get-own-property-descriptor
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Object.getOwnPropertyDescriptor")
		     :length (make-property :value 2))
      !object.get-own-property-names
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Object.getOwnPropertyNames")
		     :length (make-property :value 1))
      !object.get-own-property-symbols
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Object.getOwnPropertySymbols")
		     :length (make-property :value 1))
      !object.get-prototype-of
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Object.getPrototypeOf")
		     :length (make-property :value 1))
      !object.is
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Object.is")
		     :length (make-property :value 2))
      !object.is-extensible
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Object.isExtensible")
		     :length (make-property :value 1))
      !object.is-frozen
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Object.isFrozen")
		     :length (make-property :value 1))
      !object.is-sealed
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Object.isSealed")
		     :length (make-property :value 1))
      !object.keys
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Object.keys")
		     :length (make-property :value 1))
      !object.prevent-extensions
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Object.preventExtensions")
		     :length (make-property :value 1))
      !object.seal
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Object.seal")
		     :length (make-property :value 1))
      !object.set-prototype-of
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Object.setPrototypeOf")
		     :length (make-property :value 2))
      !symbol.for
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Symbol.for")
		     :length (make-property :value 1))
      !symbol.keyFor
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Symbol.keyFor")
		     :length (make-property :value 1))
      !number.is-finite
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Number.isFinite")
		     :length (make-property :value 1))
      !number.is-integer
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Number.isInteger")
		     :length (make-property :value 1))
      !number.is-nan
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Number.isNaN")
		     :length (make-property :value 1))
      !number.is-safe-integer
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Number.isSafeInteger")
		     :length (make-property :value 1))
      !number.parse-float
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Number.parseFloat")
		     :length (make-property :value 1))
      !number.parse-int
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Number.parseInt")
		     :length (make-property :value 2))
      !math.abs
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.abs")
		     :length (make-property :value 1))
      !math.acos
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.acos")
		     :length (make-property :value 1))
      !math.acosh
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.acosh")
		     :length (make-property :value 1))
      !math.asin
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.asin")
		     :length (make-property :value 1))
      !math.asinh
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.asinh")
		     :length (make-property :value 1))
      !math.atan
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.atan")
		     :length (make-property :value 1))
      !math.atanh
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.atanh")
		     :length (make-property :value 1))
      !math.atan2
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.atan2")
		     :length (make-property :value 2))
      !math.cbrt
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.cbrt")
		     :length (make-property :value 1))
      !math.ceil
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.ceil")
		     :length (make-property :value 1))
      !math.clz32
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.clz32")
		     :length (make-property :value 1))
      !math.cos
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.cos")
		     :length (make-property :value 1))
      !math.cosh
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.cosh")
		     :length (make-property :value 1))
      !math.exp
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.exp")
		     :length (make-property :value 1))
      !math.expm1
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.expm1")
		     :length (make-property :value 1))
      !math.floor
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.floor")
		     :length (make-property :value 1))
      !math.fround
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.fround")
		     :length (make-property :value 1))
      !math.hypot
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.hypot")
		     :length (make-property :value 2))
      !math.imul
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.imul")
		     :length (make-property :value 2))
      !math.log
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.log")
		     :length (make-property :value 1))
      !math.log1p
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.log1p")
		     :length (make-property :value 1))
      !math.log10
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.log10")
		     :length (make-property :value 1))
      !math.log2
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.log2")
		     :length (make-property :value 1))
      !math.max
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.max")
		     :length (make-property :value 2))
      !math.min
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.min")
		     :length (make-property :value 2))
      !math.pow
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.pow")
		     :length (make-property :value 2))
      !math.random
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.random")
		     :length (make-property :value 0))
      !math.round
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.round")
		     :length (make-property :value 1))
      !math.sign
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.sign")
		     :length (make-property :value 1))
      !math.sin
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.sin")
		     :length (make-property :value 1))
      !math.sinh
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.sinh")
		     :length (make-property :value 1))
      !math.sqrt
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.sqrt")
		     :length (make-property :value 1))
      !math.tan
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.tan")
		     :length (make-property :value 1))
      !math.tanh
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.tanh")
		     :length (make-property :value 1))
      !math.trunc
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Math.trunc")
		     :length (make-property :value 1))
      !date.now
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Date.now")
		     :length (make-property :value 0))
      !date.parse
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Date.parse")
		     :length (make-property :value 1))
      !date.utc
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Date.UTC")
		     :length (make-property :value 7))
      !string.from-char-code
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "String.fromCharCode")
		     :length (make-property :value 1))
      !string.from-code-point
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "String.fromCodePoint")
		     :length (make-property :value 1))
      !string.raw
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "String.raw")
		     :length (make-property :value 1))
      !array.from
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Array.from")
		     :length (make-property :value 1))
      !array.is-array
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Array.isArray")
		     :length (make-property :value 1))
      !array.of
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Array.of")
		     :length (make-property :value 0))
      !int8-array.from
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Int8Array.from")
		     :length (make-property :value 1))
      !uint8-array.from
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Uint8Array.from")
		     :length (make-property :value 1))
      !uint8-clamped-array.from
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Uint8ClampedArray.from")
		     :length (make-property :value 1))
      !int16-array.from
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Int16Array.from")
		     :length (make-property :value 1))
      !uint16-array.from
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Uint16Array.from")
		     :length (make-property :value 1))
      !int32-array.from
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Int32Array.from")
		     :length (make-property :value 1))
      !uint32-array.from
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Uint32Array.from")
		     :length (make-property :value 1))
      !float32-array.from
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Float32Array.from")
		     :length (make-property :value 1))
      !float64-array.from
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Float64Array.from")
		     :length (make-property :value 1))
      !int8-array.of
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Int8Array.of")
		     :length (make-property :value 0))
      !uint8-array.of
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Uint8Array.of")
		     :length (make-property :value 0))
      !uint8-clamped-array.of
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Uint8ClampedArray.of")
		     :length (make-property :value 0))
      !int16-array.of
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Int16Array.of")
		     :length (make-property :value 0))
      !uint16-array.of
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Uint16Array.of")
		     :length (make-property :value 0))
      !int32-array.of
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Int32Array.of")
		     :length (make-property :value 0))
      !uint32-array.of
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Uint32Array.of")
		     :length (make-property :value 0))
      !float32-array.of
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Float32Array.of")
		     :length (make-property :value 0))
      !float64-array.of
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Float64Array.of")
		     :length (make-property :value 0))
      !array-buffer.is-view
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "ArrayBuffer.isView")
		     :length (make-property :value 1))
      ;; LENGTH tested from V8.
      !json.parse
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "JSON.parse")
		     :length (make-property :value 2))
      !json.stringify
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "JSON.stringify")
		     :length (make-property :value 3))
      !promise.all
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Promise.all")
		     :length (make-property :value 1))
      !promise.race
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Promise.race")
		     :length (make-property :value 1))
      !promise.reject
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Promise.reject")
		     :length (make-property :value 1))
      !promise.resolve
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Promise.resolve")
		     :length (make-property :value 1))
      ;; I don't know what's the sense of Reflect class...
      !proxy.revocable
      (make-instance '-function-proto
		     :-prototype nil
		     :-extensible nil
		     :name (make-property :value "Proxy.revocable")
		     :length (make-property :value 2)))

(setf !object
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Object")
		     :length (make-property :value 1 :configurable :true)
		     :prototype (make-property :value (find-class '-object-proto))
		     :properties
		     '((assign . (make-property :value !assign))
		       (create . (make-property :value !create))
		       (define-properties . (make-property :value !define-properties))
		       (define-property . (make-property :value !define-property))
		       (freeze . (make-property :value !freeze))
		       (get-own-property-descriptor . (make-property :value !get-own-property-descriptor))
		       (get-own-property-names . (make-property :value !get-own-property-names))
		       (get-own-property-symbols . (make-property :value !get-own-property-symbols))
		       (get-prototype-of . (make-property :value !get-prototype-of))
		       (is . (make-property :value !is))
		       (is-extensible . (make-property :value !is-extensible))
		       (is-frozen . (make-property :value !is-frozen))
		       (is-sealed . (make-property :value !is-sealed))
		       (keys . (make-property :value !keys))
		       (prevent-extensions . (make-property :value !prevent-extensions))
		       (seal . (make-property :value !seal))
		       (set-prototype-of . (make-property :value !set-prototype-of))))
      !function
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Function")
		     :length (make-property :value 1 :configurable :true)
		     :prototype (make-property :value (find-class '-function-proto)))
      !boolean
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Boolean")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-boolean-proto)))
      !number
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Number")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class 'number-proto)))
      !string
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "String")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class 'string-proto)))
      !symbol
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Symbol")
		     :length (make-property :value 0)
		     :prototype (make-property :value (find-class '-symbol-proto)))
      !error
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Error")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-error-proto)))
      !eval-error
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "EvalError")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-eval-error-proto)))
      !range-error
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "RangeError")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-eval-error-proto)))
      !reference-error
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "ReferenceError")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-reference-error-proto)))
      !syntax-error
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "SyntaxError")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-syntax-error-proto)))
      !type-error
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "TypeError")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-type-error-proto)))
      !uri-error
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "URIError")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-uri-error-proto)))
      !array
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Array")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-array-proto)))
      !float32-array
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Float32Array")
		     :length (make-property :value 3)
		     :prototype (make-property :value (find-class '-float32-array-proto)))
      !float64-array
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Float64Array")
		     :length (make-property :value 3)
		     :prototype (make-property :value (find-class '-float64-array-proto)))
      !int8-array
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Int8Array")
		     :length (make-property :value 3)
		     :prototype (make-property :value (find-class '-int8-array-proto)))
      !int16-array
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Int16Array")
		     :length (make-property :value 3)
		     :prototype (make-property :value (find-class '-int16-array-proto)))
      !int32-array
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Int32Array")
		     :length (make-property :value 3)
		     :prototype (make-property :value (find-class '-int32-array-proto)))
      !uint8-array
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Uint8Array")
		     :length (make-property :value 3)
		     :prototype (make-property :value (find-class '-uint8-array-proto)))
      !uint8-clamped-array
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Uint8ClampedArray")
		     :length (make-property :value 3)
		     :prototype (make-property :value (find-class '-uint8-clamped-array-proto)))
      !uint16-array
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Uint16Array")
		     :length (make-property :value 3)
		     :prototype (make-property :value (find-class '-uint16-array-proto)))
      !uint32-array
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Uint32Array")
		     :length (make-property :value 3)
		     :prototype (make-property :value (find-class '-uint32-array-proto)))
      !date-0
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Date-0")
		     :length (make-property :value 7)
		     :prototype (make-property :value (find-class '-date-proto)))
      !date-1
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Date-1")
		     :length (make-property :value 7)
		     :prototype (make-property :value (find-class '-date-proto)))
      !reg-exp
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "RegExp")
		     :length (make-property :value 2)
		     :prototype (make-property :value (find-class '-reg-exp-proto)))
      !map
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Map")
		     :length (make-property :value 0)
		     :prototype (make-property :value (find-class '-map-proto)))
      !set
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Set")
		     :length (make-property :value 0)
		     :prototype (make-property :value (find-class '-set-proto)))
      !weak-map
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "WeakMap")
		     :length (make-property :value 0)
		     :prototype (make-property :value (find-class '-weak-map-proto)))
      !weak-set
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "WeakSet")
		     :length (make-property :value 0)
		     :prototype (make-property :value (find-class '-weak-set-proto)))
      !array-buffer
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "ArrayBuffer")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-array-buffer-proto)))
      !data-view
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "DataView")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-data-view-proto)))
      !promise
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Promise")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-promise-proto))))

(declaim (inline %eval))
(defun %eval (x)
  )

;;; NOTE: It seems that the isFinite and isNaN function in Math class
;;; will not coerce its parameter to number, so they may output different
;;; answer, we have to handle this problem. -- Augustus, 16 Sep 2015.
(defun %is-finite (number)
  (let ((data (slot-value (to-number number) 'number-data)))
    (case data
      ((:nan :infinity :-infinity)
       (!boolean :false))
      (t
       (!boolean :true)))))

(defun %is-nan (number)
  (let ((data (slot-value (to-number number) 'number-data)))
    (case data
      (:nan
       (!boolean :true))
      (t
       (!boolean :false)))))

;;; Helper function to parse a general number...
;;; PARSE-INT is internal...
(defun %parse-float (string)
  ;; Firstly check the first two chars, if they match 0x/0X, 0o/0O, 0b/0B,
  ;; use corresponding radix PARSE-INTEGER. Or handle decimal values.
  (let ((str (slot-value (-to-string string) 'string-data)))
    (declare (type string str))
    (when (> (length str) 2)
      (let ((first-char (char str 0))
	    (second-char (char str 1)))
	(when (char= first-char #\0)
	  (case second-char
	    ((#\x #\X)
	     (return-from parse-number
	       (!number (parse-integer str :start 2 :radix 16))))
	    ((#\o #\O)
	     (return-from parse-number
	       (!number (parse-integer str :start 2 :radix 8))))
	    ((#\b #\B)
	     (return-from parse-number
	       (!number (parse-integer str :start 2 :radix 2))))))))
    ;; Now we must be parsing a decimal, or NaN.
    (let ((sign 1)
	  (integer-part 0)
	  (decimal-part 0.0d0)
	  (saw-integer-digits nil)
	  (saw-decimal-digits nil)
	  (saw-decimal-point nil)
	  (exponent #\E)
	  (exponent-sign 1)
	  (exponent-value 0.0d0)
	  (position 0))
      ;; Underlying methods are taken from Mezzano's reader.
      (declare (type integer integer-part)
	       (type double-float decimal-part exponent-value))
      (flet ((peek ()
	       (when (< position (length str))
		 (char str position)))
	     (consume ()
	       (prog1 (char str position)
		 (incf position))))
	;; Check for a leading sign.
	(case (peek)
	  (#\- (consume)
	       (setf sign -1))
	  (#\+ (consume)))
	;; Remaining string must not be empty.
	(when (null (peek))
	  (return-from parse-number (!number :nan)))
	;; Parse the integer portion.
	(loop
	   (let ((weight (position (peek) +decimal-digits+)))
	     (when (not weight) (return))
	     (consume)
	     (setf saw-integer-digits t)
	     (setf integer-part (+ (* integer-part 10) weight))))
	;; Parse the decimal portion.
	(when (char= #\. (peek))
	  (setf saw-decimal-point t)
	  (consume)
	  ;; If there was an integer part, then the next character
	  ;; must be either a decimal-digit or an exponent marker.
	  ;; If there was no integer part, it must be a decimal-digit.
	  (when (and (not (or (not saw-integer-digits)
			      (find (peek) +exponent-indicator+)))
		     (not (find (peek) +decimal-digits+)))
	    (return-from parse-number (-number :nan)))
	  ;; Accumulate decimal digits.
	  (let ((first-decimal position))
	    (loop
	       (when (not (find (peek) +decimal-digits+))
		 (return))
	       (setf saw-decimal-digits t)
	       (consume))
	    ;; Now works backwards and build the decimal part.
	    (dotimes (i (- position first-decimal))
	      (incf decimal-part (digit-char-p (char str (- position i 1))))
	      (setf decimal-part (/ decimal-part 10.0d0)))))
	;; And look for an exponent.
	(when (find (peek) +exponent-indicator+)
	  (setf exponent (consume))
	  (case (peek)
	    (#\- (consume)
		 (setf exponent-sign -1))
	    (#\+ (consume)))
	  ;; Must be at least one digit in the exponent
	  ;; and one digit in the integer part
	  (when (or (not (find (peek) +decimal-digits+))
		    (not saw-integer-digits))
	    (return-from parse-number (!number :nan)))
	  ;; Read exponent part.
	  (loop (when (not (find (peek) +decimal-digits+))
		  (return))
	     (setf exponent-value (+ (* exponent-value 10.0d0)
				     (digit-char-p (consume))))))
	;; Must be at the end.
	(when (peek)
	  (return-from parse-number (-number :nan)))
	(!number
	 (* sign
	    (+ integer-part decimal-part)
	    (expt 10.0d0 (* exponent-sign exponent-value))))))))

(defun %parse-int (string &optional radix)
  (when (and (or (= radix 0) (= radix 16) (not radix))
	     (char= (char string 0) #\0)
	     (char= (char string 1) #\x))
    (!number (parse-integer (slot-value string '-string-data) :radix 16
			    :junk-allowed t)))
  (!number (parse-integer (slot-value string '-string-data) :radix radix
			  :junk-allowed t)))

(defun %decode-uri (encoded)
  )

(defun %decode-uri-component (encoded-component)
  )

(defun %encode-uri (uri)
  )

(defun %encode-uri-component (component)
  )

