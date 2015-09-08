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
(deftype object-raw ()
  `(or standard-class -function-proto))

(deftype +js-value-types+ ()
  `(or undefined-raw null-raw boolean-raw number-raw symbol-raw string-raw
       object-raw))

(deftype +js-primitive-value-types+ ()
  `(or undefined-raw null-raw boolean-raw number-raw symbol-raw string-raw))

;;; Integer index is a canonical numeric string value in [0, 2^53-1],
;;; array index is an integer index in [0, 2^32-1>. -- ECMA V6.
(deftype integer-index-numeric () `(integer 0 ,(- (expt 2 53) 1)))

;;; In an object, property list is an associative list with CARs keys (symbols
;;; or strings) and CDRs property structures.
(defstruct property
  ;; The value retrieved by a get access of the property.
  (value :undefined :type +js-value-types+)
  ;; If not :UNDEFINED must be a function object, in this implementation,
  ;; function is an object, but has parallel status, so there's no way to
  ;; be both function and object. The function's [[Call]] method is called
  ;; with an empty argument list to retrieve the property value each time
  ;; a get access of the property is performed.
  (get :undefined :type (or undefined-raw -function-proto))
  ;; If not :UNDEFINED must be a function object, the function's [[Call]]
  ;; method is called with an argument list containing assigned value,
  ;; assigns the property with this argument.
  (set :undefined :type (or undefined-raw -function-proto))
  ;; If :FALSE, attempts to change [[Value]] attribute using [[Set]] failes.
  (writable :false :type boolean-raw)
  ;; If :TRUE, the property will be enumerable by a for-in.
  (enumerable :false :type boolean-raw)
  ;; If :FALSE, attempts to delete the property, change the property to be
  ;; an accessor property, or change its attributes will fail.
  (configurable :false :type boolean-raw))

(defclass proto ()
  ((-prototype :type (or object-raw null-raw) :initarg :-prototype
	       :initform :null)
   (-extensible :type (or boolean-raw undefined-raw) :initarg :-extensible
		:initform :undefined)
   (constructor :type (or object-raw null-raw) :initarg :constructor
		:allocation :class :accessor properties :initform :null)
   (properties :type list :initarg :properties :accessor properties
	       :initform nil))
  (:documentation "General prototype class, used as a helper basis class,
it is implementation specific."))

(defclass builtin-function ()
  ((-prototype :type (or object-raw null-raw) :initarg :-prototype
	       :initform :null)
   (-extensible :type (or boolean-raw undefined-raw) :initarg :-extensible
		:initform :undefined)
   ;; LENGTH and NAME are present no matter in which function, make them
   ;; outstanding. Only CONSTRUCTOR, LENGTH, NAME can be out.
   (length :type (or property null-raw) :initarg :length :accessor length
	   :initform (make-property :value 0))
   (name :type (or property null-raw) :initarg :name :accessor name
	 :initform (make-property :value ""))
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

(defun to-primitive (arg &key (hint 'default))
  )

;;; Internal methods will have name camel-to-hyphen ed. Constructor instances
;;; will have name camel-to-hyphen ed too, but since built-in function should
;;; have a instance of -FUNCTION-PROTO without using the duplicated symbol,
;;; add ! as a prefix.
(setf !eval
      (make-instance '-function-proto
		     :name (make-property :value "eval")
		     :length (make-property :value 1))
      !is-finite
      (make-instance '-function-proto
		     :name (make-property :value "isFinite")
		     :length (make-property :value 1))
      !is-nan
      (make-instance '-function-proto
		     :name (make-property :value "isNaN")
		     :length (make-property :value 1))
      !parse-float
      (make-instance '-function-proto
		     :name (make-property :value "parseFloat")
		     :length (make-property :value 1))
      !parse-int
      (make-instance '-function-proto
		     :name (make-property :value "parseInt")
		     :length (make-property :value 2))
      !decode-uri
      (make-instance '-function-proto
		     :name (make-property :value "decodeURI")
		     :length (make-property :value 1))
      !decode-uri-component
      (make-instance '-function-proto
		     :name (make-property :value "decodeURIComponent")
		     :length (make-property :value 1))
      !encode-uri
      (make-instance '-function-proto
		     :name (make-property :value "encodeURI")
		     :length (make-property :value 1))
      !encode-uri-component
      (make-instance '-function-proto
		     :name (make-property :value "encodeURIComponent")
		     :length (make-property :value 1)))

(setf !assign
      (make-instance '-function-proto
		     :name (make-property :value "assign")
		     :length (make-property :value 2))
      !create
      (make-instance '-function-proto
		     :name (make-property :value "create")
		     :length (make-property :value 2))
      !define-properties
      (make-instance '-function-proto
		     :name (make-property :value "defineProperties")
		     :length (make-property :value 2))
      !define-property
      (make-instance '-function-proto
		     :name (make-property :value "defineProperty")
		     :length (make-property :value 3))
      !freeze
      (make-instance '-function-proto
		     :name (make-property :value "freeze")
		     :length (make-property :value 1))
      !get-own-property-descriptor
      (make-instance '-function-proto
		     :name (make-property :value "getOwnPropertyDescriptor")
		     :length (make-property :value 2))
      !get-own-property-names
      (make-instance '-function-proto
		     :name (make-property :value "getOwnPropertyNames")
		     :length (make-property :value 1))
      !get-own-property-symbols
      (make-instance '-function-proto
		     :name (make-property :value "getOwnPropertySymbols")
		     :length (make-property :value 1))
      !get-prototype-of
      (make-instance '-function-proto
		     :name (make-property :value "getPrototypeOf")
		     :length (make-property :value 1))
      !is
      (make-instance '-function-proto
		     :name (make-property :value "is")
		     :length (make-property :value 2))
      !is-extensible
      (make-instance '-function-proto
		     :name (make-property :value "isExtensible")
		     :length (make-property :value 1))
      !is-frozen
      (make-instance '-function-proto
		     :name (make-property :value "isFrozen")
		     :length (make-property :value 1))
      !is-sealed
      (make-instance '-function-proto
		     :name (make-property :value "isSealed")
		     :length (make-property :value 1))
      !keys
      (make-instance '-function-proto
		     :name (make-property :value "keys")
		     :length (make-property :value 1))
      !prevent-extensions
      (make-instance '-function-proto
		     :name (make-property :value "preventExtensions")
		     :length (make-property :value 1))
      !seal
      (make-instance '-function-proto
		     :name (make-property :value "seal")
		     :length (make-property :value 1))
      !set-prototype-of
      (make-instance '-function-proto
		     :name (make-property :value "setPrototypeOf")
		     :length (make-property :value 2))
      )

(setf -object
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Object")
		     :length (make-property :value 1 :configurable :true)
		     :properties
		     '((prototype . (make-property :value (find-class '-object-proto')))
		       (assign . (make-property :value !assign))
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
      -function
      (make-instance '-function-proto
		     :-prototype '-function-proto
		     :name (make-property :value "Function")
		     :length (make-property :value 1 :configurable :true)
		     :properties
		     '((prototype . (make-property :value (find-class '-function-proto)))))
      -boolean
      (make-instance '-function-proto
		     :-prototype '-function-proto
		     :name (make-property :value "Boolean")
		     :length (make-property :value 1)
		     :properties
		     '((prototype . (make-property :value (find-class '-boolean-proto)))))
      -symbol
      (make-instance '-function-proto
		     :-prototype '-function-proto
		     :name (make-property :value "Symbol")
		     :length (make-property :value 0)
		     :properties
		     '(()))
      -error
      (make-instance '-function-proto
		     :-prototype '-function-proto
		     :name (make-property :value "Error")
		     :length (make-property :value 1)
		     :properties
		     '(()))
      -eval-error
      (make-instance '-function-proto
		     :-prototype '-function-proto
		     :name (make-property :value "EvalError")
		     :length (make-property :value 1)
		     :properties
		     '(()))
      -range-error
      (make-instance '-function-proto
		     :-prototype '-function-proto
		     :name (make-property :value "RangeError")
		     :length (make-property :value 1)
		     :properties
		     '(()))
      -reference-error
      (make-instance '-function-proto
		     :-prototype '-function-proto
		     :name (make-property :value "ReferenceError")
		     :length (make-property :value 1)
		     :properties
		     '(()))

(declaim (inline !eval))
(defun !eval (x)
  )

(defun is-finite (number)
  (let ((num (to-number number)))
    (case num
      (((-number :nan) (-number :infinity) (-number :-infinity))
       :false)
      (t
       :true))))

(defun is-nan (number)
  (let ((num (to-number number)))
    (case num
      ((-number :nan)
       :true)
      (t
       :false))))

;;; Helper function to parse a general number...
;;; PARSE-INT is internal...
(defun parse-float (string)
  ;; Firstly check the first two chars, if they match 0x/0X, 0o/0O, 0b/0B,
  ;; use corresponding radix PARSE-INTEGER. Or handle decimal values.
  (declare (type string string))
  (let ((sign 1))
    (when (> (length string) 2)
      (let ((first-char (char string 0))
	    (second-char (char string 1)))
	(when (char= first-char #\0)
	  (case second-char
	    ((#\x #\X)
	     (return-from parse-number
	       (parse-integer string :start 2 :radix 16)))
	    ((#\o #\O)
	     (return-from parse-number
	       (parse-integer string :start 2 :radix 8)))
	    ((#\b #\B)
	     (return-from parse-number
	       (parse-integer string :start 2 :radix 2)))))))
    ;; Now we must be parsing a decimal, or NaN.
    (let ((integer-part 0)
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
	       (when (< position (length string))
		 (char string position)))
	     (consume ()
	       (prog1 (char string position)
		 (incf position))))
	;; Check for a leading sign.
	(case (peek)
	  (#\- (consume)
	       (setf sign -1))
	  (#\+ (consume)))
	;; Remaining string must not be empty.
	(when (null (peek))
	  (return-from parse-number (values :nan 0)))
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
	    (return-from parse-number (values :nan 0)))
	  ;; Accumulate decimal digits.
	  (let ((first-decimal position))
	    (loop
	       (when (not (find (peek) +decimal-digits+))
		 (return))
	       (setf saw-decimal-digits t)
	       (consume))
	    ;; Now works backwards and build the decimal part.
	    (dotimes (i (- position first-decimal))
	      (incf decimal-part (digit-char-p (char string (- position i 1))))
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
	    (return-from parse-number (values :nan 0)))
	  ;; Read exponent part.
	  (loop (when (not (find (peek) +decimal-digits+))
		  (return))
	     (setf exponent-value (+ (* exponent-value 10.0d0)
				     (digit-char-p (consume))))))
	;; Must be at the end.
	(when (peek)
	  (return-from parse-number (values :nan 0)))
	(* sign
	   (+ integer-part decimal-part)
	   (expt 10.0d0 (* exponent-sign exponent-value)))))))

(defun parse-int (string radix)
  (parse-integer string :radix radix))

(defun decode-uri (encoded)
  )

(defun decode-uri-component (encoded-component)
  )

(defun encode-uri (uri)
  )

(defun encode-uri-component (component)
  )

