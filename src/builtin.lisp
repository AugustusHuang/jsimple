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
(in-package :jsimple-builtin)

;;;; -UNDEFINED, -NULL are trivial, use the keywords.
(defun undefined-p (value)
  (eql value :undefined))
(deftype -undefined ()
  `(satisfies undefined-p))

(defun null-p (value)
  (eql value :null))
(deftype -null ()
  `(satisfies null-p))

(deftype +js-types+ ()
  `(or -undefined -null -boolean -number -symbol -string -object))

(deftype +js-primitive-value+ ()
  `(or -undefined -null -boolean -number -symbol -string))

;;; In an object, property list is an associative list with CARs keys (symbols
;;; or strings) and CDRs property structures.
(defstruct property
  ;; The value retrieved by a get access of the property.
  (value :undefined :type +js-type+)
  ;; If not :UNDEFINED must be a function object, in this implementation,
  ;; function is an object, but has parallel status, so there's no way to
  ;; be both function and object. The function's [[Call]] method is called
  ;; with an empty argument list to retrieve the property value each time
  ;; a get access of the property is performed.
  (get :undefined :type (or -undefined -function))
  ;; If not :UNDEFINED must be a function object, the function's [[Call]]
  ;; method is called with an argument list containing assigned value,
  ;; assigns the property with this argument.
  (set :undefined :type (or -undefined -function))
  ;; If :FALSE, attempts to change [[Value]] attribute using [[Set]] failes.
  (writable :false :type boolean-raw)
  ;; If :TRUE, the property will be enumerable by a for-in.
  (enumerable :false :type boolean-raw)
  ;; If :FALSE, attempts to delete the property, change the property to be
  ;; an accessor property, or change its attributes will fail.
  (configurable :false :type boolean-raw))

;;; Well known symbols are built-in symbol values are typically used
;;; as the keys of properties. -- ECMA V6.
;;; At very early stage, all WELL-KNOWN-SYMBOLS are undefined.
;;; These symbols will correspond to every object, and every object will
;;; has its specific typed symbol function of them.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *well-known-symbols*
    (let ((symbols (make-hash-table :test 'string=)))
      (dolist (name '("hasInstance" "isConcatSpreadable" "iterator" "match"
		      "replace" "search" "species" "split" "toPrimitive"
		      "toStringTag" "unscopables"))
	(setf (gethash name symbols) :js-undefined))
      symbols))

  ;; WELL-KNOWN-OBJECTS are well-known intrinsic objects can be refered..
  ;; When a symbol is met, corresponding function will be translated into
  ;; Lisp land directly.
  (defvar *well-known-objects*
    (let ((objs (make-hash-table :test 'string=)))
      (dolist (name '("Array" "ArrayBuffer" "ArrayBuffer.prototype"
		      "Array.prototype" "Array.prototype.values" "Boolean"
		      "Boolean.prototype" "DataView" "DataView.prototype"
		      "Date" "Date.prototype" "decodeURI" "decodeURIComponent"
		      "encodeURI" "encodeURIComponent" "Error"
		      "Error.prototype" "eval" "EvalError"
		      "EvalError.prototype" "Float32Array"
		      "Float32Array.prototype" "Float64Array"
		      "Float64Array.prototype" "Function" "Function.prototype"
		      "Int8Array" "Int8Array.prototype" "Int16Array"
		      "Int16Array.prototype" "Int32Array"
		      "Int32Array.prototype" "isFinite" "isNaN" "JSON" "Map"
		      "Map.prototype" "Math" "Number" "Number.prototype"
		      "Object" "Object.prototype"
		      "parseFloat" "parseInt" "Promise" "Promise.prototype"
		      "Proxy" "RangeError" "RangeError.prototype"
		      "ReferenceError" "ReferenceError.prototype" "Reflect"
		      "RegExp" "RegExp.prototype" "Set.prototype" "String"
		      "String.prototype" "Symbol" "Symbol.prototype"
		      "SyntaxError" "SyntaxError.prototype" "TypeError"
		      "TypeError.prototype" "Uint8Array" "Uint8Array.prototype"
		      "Uint8ClampedArray" "Uint8ClampedArray.prototype"
		      "Uint16Array" "Uint16Array.prototype" "Uint32Array"
		      "Uint32Array.prototype" "URIError" "URIError.prototype"
		      "WeakMap" "WeakMap.prototype" "WeakSet"
		      "WeakSet.prototype"))
	(setf (gethash name objs) nil))
      objs))
  
  ;; Each symbol value immutably holds an associated value called
  ;; description that is either :js-undefined or a string value. -- ECMA V6.
  (setf (gethash "hasInstance" *well-known-symbols*) "Symbol.hasInstance"
	(gethash "isConcatSpreadable" *well-known-symbols*)
	"Symbol.isConcatSpreadable"
	(gethash "iterator" *well-known-symbols*) "Symbol.iterator"
	(gethash "match" *well-known-symbols*) "Symbol.match"
	(gethash "replace" *well-known-symbols*) "Symbol.replace"
	(gethash "search" *well-known-symbols*) "Symbol.search"
	(gethash "species" *well-known-symbols*) "Symbol.species"
	(gethash "split" *well-known-symbols*) "Symbol.split"
	(gethash "toPrimitive" *well-known-symbols*) "Symbol.toPrimitive"
	(gethash "toStringTag" *well-known-symbols*) "Symbol.toStringTag"
	(gethash "unscopables" *well-known-symbols*) "Symbol.unscopables")

  ;; All well known objects has related functions.
  (setf
   ;; All builtin constructors and object functions. The first element
   ;; is the default constructor.
   ;; TODO: Add object functions...
   (gethash "Array" *well-known-objects*)
   '(js-array-build)
   (gethash "ArrayBuffer" *well-known-objects*)
   '(js-array-buffer-build)
   (gethash "Boolean" *well-known-objects*)
   '(js-boolean-build)
   (gethash "DataView" *well-known-objects*)
   '(js-data-view-build)
   (gethash "Date" *well-known-objects*)
   '(js-date-build)
   (gethash "Error" *well-known-objects*)
   '(js-error-build)
   (gethash "EvalError" *well-known-objects*)
   '(js-eval-error-build)
   (gethash "Float32Array" *well-known-objects*)
   '(js-float32-array-build)
   (gethash "Float64Array" *well-known-objects*)
   '(js-float64-array-build)
   ;; This is the function constructor, not the function declaration.
   (gethash "Function" *well-known-objects*)
   '(js-function-build)
   (gethash "Int8Array" *well-known-objects*)
   '(js-int8-array-build)
   (gethash "Int16Array" *well-known-objects*)
   '(js-int16-array-build)
   (gethash "Int32Array" *well-known-objects*)
   '(js-int32-array-build)
   (gethash "Map" *well-known-objects*)
   '(js-map-build)
   (gethash "Math" *well-known-objects*)
   '(js-math-build)
   (gethash "Number" *well-known-objects*)
   '(js-number-build js-is-integer js-is-safe-integer)
   (gethash "Object" *well-known-objects*)
   '(js-object-build
     js-assign js-create js-define-properties js-define-property js-freeze
     js-get-own-property-descriptor js-get-own-property-names
     js-get-own-property-symbols js-get-property-of js-is
     js--is-extensible js-is-frozen js-is-sealed js-keys
     js-prevent-extensions js-seal js-set-prototype-of)
   (gethash "Proxy" *well-known-objects*)
   '(js-proxy-build)
   (gethash "Promise" *well-known-objects*)
   '(js-promise-build)
   (gethash "RangeError" *well-known-objects*)
   '(js-range-error-build)
   (gethash "ReferenceError" *well-known-objects*)
   '(js-reference-error-build)
   (gethash "RegExp" *well-known-objects*)
   '(js-reg-exp-build)
   (gethash "Set" *well-known-objects*)
   '(js-set-build)
   (gethash "String" *well-known-objects*)
   '(js-string-build)
   (gethash "Symbol" *well-known-objects*)
   '(js-symbol-build)
   (gethash "SyntaxError" *well-known-objects*)
   '(js-syntax-error-build)
   (gethash "TypeError" *well-known-objects*)
   '(js-type-error-build)
   (gethash "Uint8Array" *well-known-objects*)
   '(js-uint8-array-build)
   (gethash "Uint8ClampedArray" *well-known-objects*)
   '(js-uint8-clamped-array-build)
   (gethash "Uint16Array" *well-known-objects*)
   '(js-uint16-array-build)
   (gethash "Uint32Array" *well-known-objects*)
   '(js-uint32-array-build)
   (gethash "URIError" *well-known-objects*)
   '(js-uri-error-build)
   (gethash "WeakMap" *well-known-objects*)
   '(js-weak-map-build)
   (gethash "WeakSet" *well-known-objects*)
   '(js-weak-set-build)
   ;; And builtin functions.
   (gethash "isFinite" *well-known-objects*)
   '(js-is-finite)
   (gethash "isNaN" *well-known-objects*)
   '(js-is-nan)
   (gethash "parseFloat" *well-known-objects*)
   '(js-parse-float)
   (gethash "parseInt" *well-known-objects*)
   '(js-parse-int)
   (gethash "decodeURI" *well-known-objects*)
   '(js-decode-uri)
   (gethash "encodeURI" *well-known-objects*)
   '(js-encode-uri)
   (gethash "decodeURIComponent" *well-known-objects*)
   '(js-decode-uri-component)
   (gethash "encodeURIComponent" *well-known-objects*)
   '(js-encode-uri-component)
   ;; And builtin prototypes. Value will be a list of functions.
   ;; The length property is implied.
   ;; XXX: Use CLOS or not? If use CLOS, simplify the function names...
   (gethash "Array.prototype" *well-known-objects*)
   '(js-concat js-copy-within js-entries js-every js-fill js-filter js-find
     js-find-index js-for-each js-index-of js-join js-keys js-last-index-of
     js-map js-pop js-push js-reduce js-reduce-right js-reverse js-shift
     js-slice js-some js-sort js-splice js-to-locale-string js-to-string
     js-unshift js-values)
   (gethash "ArrayBuffer.prototype" *well-known-objects*)
   '(js-byte-length js-slice)
   (gethash "Boolean.prototype" *well-known-objects*)
   '(js-to-string)
   (gethash "DataView.prototype" *well-known-objects*)
   '(js-buffer js-byte-length js-byte-offset js-get-float32 js-get-float64
     js-get-int8 js-get-int16 js-get-int32 js-get-uint8 js-get-uint16
     js-get-uint32 js-set-float32 js-set-float64 js-set-int8 js-set-int16
     js-set-int32 js-set-uint8 js-set-uint16 js-set-uint32)
   (gethash "Date.prototype" *well-known-objects*)
   '(js-get-date js-get-day js-get-hours js-get-milliseconds js-get-minutes
     js-get-month js-get-seconds js-get-time js-get-timezone-offset
     js-get-utc-date js-get-utc-day js-get-utc-full-year js-get-utc-hours
     js-get-utc-milliseconds js-get-utc-minutes js-get-utc-month
     js-get-utc-seconds js-set-date js-set-full-year js-set-hours
     js-set-milliseconds js-set-minutes js-set-month js-set-seconds
     js-set-time js-set-utc-date js-set-utc-full-year js-set-utc-hours
     js-set-utc-milliseconds js-set-utc-minutes js-set-utc-month
     js-set-utc-seconds js-to-date-string js-to-iso-string js-to-json
     js-to-locale-date-string js-to-locale-string js-to-locale-time-string
     js-to-string js-to-time-string js-to-utc-string js-value-of)
   (gethash "EvalError.prototype" *well-known-objects*)
   '(js-message js-name js-to-string)
   (gethash "Error.prototype" *well-known-objects*)
   '(js-message js-name js-to-string)
   (gethash "Float32Array.prototype" *well-known-objects*)
   '(js-buffer js-byte-length js-byte-offset js-copy-within js-entries
     js-every js-fill js-filter js-find js-find-index js-for-each js-index-of
     js-join js-keys js-last-index-of js-length js-map js-reduce
     js-reduce-right js-reverse js-set js-slice js-some js-sort js-subarray
     js-to-locale-string js-to-string js-values)
   (gethash "Float64Array.prototype" *well-known-objects*)
   '(js-buffer js-byte-length js-byte-offset js-copy-within js-entries
     js-every js-fill js-filter js-find js-find-index js-for-each js-index-of
     js-join js-keys js-last-index-of js-length js-map js-reduce
     js-reduce-right js-reverse js-set js-slice js-some js-sort js-subarray
     js-to-locale-string js-to-string js-values)
   (gethash "Function.prototype" *well-known-objects*)
   '(js-apply js-bind js-call js-to-string)
   (gethash "Int8Array.prototype" *well-known-objects*)
   '(js-buffer js-byte-length js-byte-offset js-copy-within js-entries
     js-every js-fill js-filter js-find js-find-index js-for-each js-index-of
     js-join js-keys js-last-index-of js-length js-map js-reduce
     js-reduce-right js-reverse js-set js-slice js-some js-sort js-subarray
     js-to-locale-string js-to-string js-values)
   (gethash "Int16Array.prototype" *well-known-objects*)
   '(js-buffer js-byte-length js-byte-offset js-copy-within js-entries
     js-every js-fill js-filter js-find js-find-index js-for-each js-index-of
     js-join js-keys js-last-index-of js-length js-map js-reduce
     js-reduce-right js-reverse js-set js-slice js-some js-sort js-subarray
     js-to-locale-string js-to-string js-values)
   (gethash "Int32Array.prototype" *well-known-objects*)
   '(js-buffer js-byte-length js-byte-offset js-copy-within js-entries
     js-every js-fill js-filter js-find js-find-index js-for-each js-index-of
     js-join js-keys js-last-index-of js-length js-map js-reduce
     js-reduce-right js-reverse js-set js-slice js-some js-sort js-subarray
     js-to-locale-string js-to-string js-values)
   (gethash "Map.prototype" *well-known-objects*)
   '(js-clear js-delete js-entries js-for-each js-get js-has js-keys js-set
     js-size js-values)
   (gethash "Math.prototype" *well-known-objects*)
   '(js-abs js-acos js-acosh js-asin js-asinh js-atan js-atanh js-atan2
     js-cbrt js-ceil js-clz32 js-cos js-cosh js-exp js-expm1 js-floor
     js-fround js-hypot js-imul js-log js-log1p js-log10 js-log2 js-max js-min
     js-pow js-random js-round js-sign js-sin js-sinh js-sqrt js-tan js-tanh
     js-trunc)
   (gethash "Number.prototype" *well-known-objects*)
   '(js-to-exponential js-to-fixed js-to-locale-string js-to-precision
     js-to-string js-value-of)
   (gethash "Object.prototype" *well-known-objects*)
   '(js-has-own-property js-is-prototype-of js-property-is-enumerable
     js-to-locale-string js-to-string js-value-of)
   (gethash "Promise.prototype" *well-known-objects*)
   '(js-catch js-then)
   (gethash "RangeError.prototype" *well-known-objects*)
   '(js-message js-name js-to-string)
   (gethash "ReferenceError.prototype" *well-known-objects*)
   '(js-message js-name js-to-string)
   (gethash "RegExp.prototype" *well-known-objects*)
   '(js-exec js-flags js-global js-ignore-case js-multiline js-source
     js-sticky js-test js-to-string js-unicode)
   (gethash "Set.prototype" *well-known-objects*)
   '(js-add js-clear js-delete js-entries js-for-each js-has js-keys js-size
     js-values)
   (gethash "String.prototype" *well-known-objects*)
   '(js-char-at js-char-code-at js-code-point-at js-concat js-ends-with
     js-includes js-index-of js-last-index-of js-local-compare js-match
     js-normalize js-repeat js-replace js-search js-slice js-split
     js-starts-with js-substring js-to-locale-lower-case
     js-to-locale-upper-case js-to-lower-case js-to-string js-to-upper-case
     js-trim js-value-of)
   (gethash "Symbol.prototype" *well-known-objects*)
   '(js-to-string js-value-of)
   (gethash "SyntaxError.prototype" *well-known-objects*)
   '(js-message js-name js-to-string)
   (gethash "TypeError.prototype" *well-known-objects*)
   '(js-message js-name js-to-string)
   (gethash "Uint8Array.prototype" *well-known-objects*)
   '(js-buffer js-byte-length js-byte-offset js-copy-within js-entries
     js-every js-fill js-filter js-find js-find-index js-for-each js-index-of
     js-join js-keys js-last-index-of js-length js-map js-reduce
     js-reduce-right js-reverse js-set js-slice js-some js-sort js-subarray
     js-to-locale-string js-to-string js-values)
   (gethash "Uint8ClampedArray.prototype" *well-known-objects*)
   '(js-buffer js-byte-length js-byte-offset js-copy-within js-entries
     js-every js-fill js-filter js-find js-find-index js-for-each js-index-of
     js-join js-keys js-last-index-of js-length js-map js-reduce
     js-reduce-right js-reverse js-set js-slice js-some js-sort js-subarray
     js-to-locale-string js-to-string js-values)
   (gethash "Uint16Array.prototype" *well-known-objects*)
   '(js-buffer js-byte-length js-byte-offset js-copy-within js-entries
     js-every js-fill js-filter js-find js-find-index js-for-each js-index-of
     js-join js-keys js-last-index-of js-length js-map js-reduce
     js-reduce-right js-reverse js-set js-slice js-some js-sort js-subarray
     js-to-locale-string js-to-string js-values)
   (gethash "Uint32Array.prototype" *well-known-objects*)
   '(js-buffer js-byte-length js-byte-offset js-copy-within js-entries
     js-every js-fill js-filter js-find js-find-index js-for-each js-index-of
     js-join js-keys js-last-index-of js-length js-map js-reduce
     js-reduce-right js-reverse js-set js-slice js-some js-sort js-subarray
     js-to-locale-string js-to-string js-values)
   (gethash "URIError.prototype" *well-known-objects*)
   '(js-message js-name js-to-string)
   (gethash "WeakMap.prototype" *well-known-objects*)
   '(js-delete js-get js-has js-set)
   (gethash "WeakSet.prototype" *well-known-objects*)
   '(js-add js-delete js-has)
   ))

;;; Integer index is a canonical numeric string value in [0, 2^53-1],
;;; array index is an integer index in [0, 2^32-1>. -- ECMA V6.
(deftype integer-index-numeric () `(integer 0 ,(- (expt 2 53) 1)))

;;; In order to avoid re-definition of global functions and variables,
;;; make a hashtable to store those already created, and check every time
;;; when we have to define a global one instead of a scoped one.
;;; Since ES/JS know upper and lowercase, use STRING= instead of EQL or EQUAL.
(defvar *global-names*
  (make-hash-table :test 'string=))

;;; Essential internal methods, generic or some?
(defun js-get-prototype-of (obj)
  )

(defun js-set-prototype-of (obj proto)
  )

(defun js-is-extensible (obj)
  )

(defun js-prevent-extensions (obj key)
  )

(defun js-get-own-property (obj key)
  )

(defun js-has-property (obj)
  )

(defun js-get (obj key receiver)
  )

(defun js-set (obj key value receiver)
  )

(defun js-delete (obj key)
  )

(defun js-define-own-property (obj key descriptor)
  )

(defun js-enumerate (obj)
  )

(defun js-own-property-keys (obj)
  )

(defun js-call (obj args)
  )

(defun js-construct (obj args)
  )
