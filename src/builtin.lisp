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
(in-package :jsimple-ir)

(defparameter +js-type+
  '(member :js-int8 :js-uint8 :js-int16 :js-uint16 :js-int32 :js-uint32
    :js-float :js-double :js-null :js-boolean :js-int8-array :js-uint8-array
    :js-int16-array :js-uint16-array :js-int32-array :js-uint32-array
    :js-float-array :js-double-array :js-string :js-symbol
    :js-object :js-function :js-undefined))

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
   '(js-array)
   (gethash "ArrayBuffer" *well-known-objects*)
   '(js-arraybuffer)
   (gethash "Boolean" *well-known-objects*)
   '(js-boolean)
   (gethash "DataView" *well-known-objects*)
   '(js-data-view)
   (gethash "Date" *well-known-objects*)
   '(js-date)
   (gethash "Error" *well-known-objects*)
   '(js-error)
   (gethash "EvalError" *well-known-objects*)
   '(js-eval-error)
   (gethash "Float32Array" *well-known-objects*)
   '(js-float32-array)
   (gethash "Float64Array" *well-known-objects*)
   '(js-float64-array)
   ;; This is the function constructor, not the function declaration.
   (gethash "Function" *well-known-objects*)
   '(js-function)
   (gethash "Int8Array" *well-known-objects*)
   '(js-int8-array)
   (gethash "Int16Array" *well-known-objects*)
   '(js-int16-array)
   (gethash "Int32Array" *well-known-objects*)
   '(js-int32-array)
   (gethash "Map" *well-known-objects*)
   '(js-map)
   (gethash "Math" *well-known-objects*)
   '(js-math)
   (gethash "Number" *well-known-objects*)
   '(js-number)
   (gethash "Object" *well-known-objects*)
   '(js-object)
   (gethash "Proxy" *well-known-objects*)
   '(js-proxy)
   (gethash "Promise" *well-known-objects*)
   '(js-promise)
   (gethash "RangeError" *well-known-objects*)
   '(js-range-error)
   (gethash "ReferenceError" *well-known-objects*)
   '(js-reference-error)
   (gethash "RegExp" *well-known-objects*)
   '(js-reg-exp)
   (gethash "Set" *well-known-objects*)
   '(js-set)
   (gethash "String" *well-known-objects*)
   '(js-string)
   (gethash "Symbol" *well-known-objects*)
   '(js-symbol)
   (gethash "SyntaxError" *well-known-objects*)
   '(js-syntax-error)
   (gethash "TypeError" *well-known-objects*)
   '(js-type-error)
   (gethash "Uint8Array" *well-known-objects*)
   '(js-uint8-array)
   (gethash "Uint16Array" *well-known-objects*)
   '(js-uint16-array)
   (gethash "Uint32Array" *well-known-objects*)
   '(js-uint32-array)
   (gethash "URIError" *well-known-objects*)
   '(js-uri-error)
   (gethash "WeakMap" *well-known-objects*)
   '(js-weak-map)
   (gethash "WeakSet" *well-known-objects*)
   '(js-weak-set)
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
   '(js-array-concat js-array-copy-within js-array-entries
     js-array-every js-array-fill js-array-filter js-array-find
     js-array-find-index js-array-for-each js-array-index-of
     js-array-join js-array-keys js-array-last-index-of
     js-array-map js-array-pop js-array-push js-array-reduce
     js-array-reduce-right js-array-reverse js-array-shift
     js-array-slice js-array-some js-array-sort js-array-splice
     js-array-to-locale-string js-array-to-string js-array-unshift
     js-array-values)
   (gethash "ArrayBuffer.prototype" *well-known-objects*)
   '(js-array-buffer-byte-length js-array-buffer-slice)
   (gethash "Boolean.prototype" *well-known-objects*)
   '(js-boolean-to-string)
   (gethash "DataView.prototype" *well-known-objects*)
   '(js-data-view-buffer js-data-view-byte-length js-data-view-byte-offset
     js-data-view-get-float32 js-data-view-get-float64 js-data-view-get-int8
     js-data-view-get-int16 js-data-view-get-int32 js-data-view-get-uint8
     js-data-view-get-uint16 js-data-view-get-uint32 js-data-view-set-float32
     js-data-view-set-float64 js-data-view-set-int8 js-data-view-set-int16
     js-data-view-set-int32 js-data-view-set-uint8 js-data-view-set-uint16
     js-data-view-set-uint32)
   (gethash "Date.prototype" *well-known-objects*)
   '(js-date-get-date js-date-get-day js-date-get-hours
     js-date-get-milliseconds js-date-get-minutes js-date-get-month
     js-date-get-seconds js-date-get-time js-date-get-timezone-offset
     js-date-get-utc-date js-date-get-utc-day js-date-get-utc-full-year
     js-date-get-utc-hours js-date-get-utc-milliseconds
     js-date-get-utc-minutes js-date-get-utc-month
     js-date-get-utc-seconds js-date-set-date
     js-date-set-full-year js-date-set-hours js-date-set-milliseconds
     js-date-set-minutes js-date-set-month js-date-set-seconds
     js-date-set-time js-date-set-utc-date js-date-set-utc-full-year
     js-date-set-utc-hours js-date-set-utc-milliseconds
     js-date-set-utc-minutes js-date-set-utc-month
     js-date-set-utc-seconds js-date-to-date-string
     js-date-to-iso-string js-date-to-json js-date-to-locale-date-string
     js-date-to-locale-string js-date-to-locale-time-string
     js-date-to-string js-date-to-time-string js-date-to-utc-string
     js-date-value-of)
   (gethash "EvalError.prototype" *well-known-objects*)
   '(js-eval-error-message js-eval-error-name js-eval-error-to-string)
   (gethash "Error.prototype" *well-known-objects*)
   '(js-error-message js-error-name js-error-to-string)
   (gethash "Float32Array.prototype" *well-known-objects*)
   (gethash "Float64Array.prototype" *well-known-objects*)
   (gethash "Function.prototype" *well-known-objects*)
   '(js-function-apply js-function-bind js-function-call js-function-to-string)
   (gethash "Int8Array.prototype" *well-known-objects*)
   '(js-int8-array-buffer js-int8-array-byte-length
     js-int8-array-byte-offset js-int8-array-copy-within
     js-int8-array-entries js-int8-array-every js-int8-array-fill
     js-int8-array-filter js-int8-array-find js-int8-array-find-index
     js-int8-array-for-each js-int8-array-index-of js-int8-array-join
     js-int8-array-keys js-int8-array-last-index-of js-int8-array-length
     js-int8-array-map js-int8-array-reduce js-int8-array-reduce-right
     js-int8-array-reverse js-int8-array-set js-int8-array-slice
     js-int8-array-some js-int8-array-sort js-int8-array-subarray
     js-int8-array-to-locale-string js-int8-array-to-string
     js-int8-array-values)
   (gethash "Int16Array.prototype" *well-known-objects*)
   (gethash "Int32Array.prototype" *well-known-objects*)
   (gethash "Map.prototype" *well-known-objects*)
   '(js-map-clear js-map-delete js-map-entries js-map-for-each
     js-map-get js-map-has js-map-keys js-map-set js-map-size js-map-values)
   (gethash "Math.prototype" *well-known-objects*)
   '(js-math-abs js-math-acos js-math-acosh js-math-asin js-math-asinh
     js-math-atan js-math-atanh js-math-atan2 js-math-cbrt js-math-ceil
     js-math-clz32 js-math-cos js-math-cosh js-math-exp js-math-expm1
     js-math-floor js-math-fround js-math-hypot js-math-imul js-math-log
     js-math-log1p js-math-log10 js-math-log2 js-math-max js-math-min
     js-math-pow js-math-random js-math-round js-math-sign js-math-sin
     js-math-sinh js-math-sqrt js-math-tan js-math-tanh js-math-trunc)
   (gethash "Number.prototype" *well-known-objects*)
   '(js-number-to-exponential js-number-to-fixed
     js-number-to-locale-string js-number-to-precision
     js-number-to-string js-number-value-of)
   (gethash "Object.prototype" *well-known-objects*)
   '(js-object-assign js-object-create js-object-define-properties
     js-object-define-property js-object-freeze
     js-object-get-own-property-descriptor js-object-get-own-property-names
     js-object-get-own-property-symbols js-object-get-property-of
     js-object-is js-object-is-extensible js-object-is-frozen
     js-object-is-sealed js-object-keys js-object-prevent-extensions
     js-object-seal js-object-set-prototype-of
     js-object-has-own-property js-object-is-prototype-of
     js-object-property-is-enumerable js-object-to-locale-string
     js-object-to-string js-object-value-of)
   (gethash "Promise.prototype" *well-known-objects*)
   '(js-promise-catch js-promise-then)
   (gethash "RangeError.prototype" *well-known-objects*)
   '(js-range-error-message js-range-error-name js-range-error-to-string)
   (gethash "ReferenceError.prototype" *well-known-objects*)
   '(js-reference-error-message js-reference-error-name
     js-reference-error-to-string)
   (gethash "RegExp.prototype" *well-known-objects*)
   '(js-regexp-exec js-regexp-flags js-regexp-global
     js-regexp-ignore-case js-regexp-multiline js-regexp-source
     js-regexp-sticky js-regexp-test js-regexp-to-string
     js-regexp-unicode)
   (gethash "Set.prototype" *well-known-objects*)
   '(js-set-add js-set-clear js-set-delete js-set-entries js-set-for-each
     js-set-has js-set-keys js-set-size js-set-values)
   (gethash "String.prototype" *well-known-objects*)
   '(js-string-char-at js-string-char-code-at js-string-code-point-at
     js-string-concat js-string-ends-with js-string-includes
     js-string-index-of js-string-last-index-of js-string-local-compare
     js-string-match js-string-normalize js-string-repeat
     js-string-replace js-string-search js-string-slice js-string-split
     js-string-starts-with js-string-substring
     js-string-to-locale-lower-case js-string-to-locale-upper-case
     js-string-to-lower-case js-string-to-string js-string-to-upper-case
     js-string-trim js-string-value-of)
   (gethash "Symbol.prototype" *well-known-objects*)
   '(js-symbol-to-string js-symbol-value-of)
   (gethash "SyntaxError.prototype" *well-known-objects*)
   '(js-syntax-error-message js-syntax-error-name js-syntax-error-to-string)
   (gethash "TypeError.prototype" *well-known-objects*)
   '(js-type-error-message js-type-error-name js-type-error-to-string)
   (gethash "Uint8Array.prototype" *well-known-objects*)
   (gethash "Uint8ClampedArray.prototype" *well-known-objects*)
   (gethash "Uint16Array.prototype" *well-known-objects*)
   (gethash "Uint32Array.prototype" *well-known-objects*)
   (gethash "URIError.prototype" *well-known-objects*)
   '(js-uri-error-message js-uri-error-name js-uri-error-to-string)
   (gethash "WeakMap.prototype" *well-known-objects*)
   '(js-weak-map-delete js-weak-map-get js-weak-map-has js-weak-map-set)
   (gethash "WeakSet.prototype" *well-known-objects*)
   '(js-weak-set-add js-weak-set-delete js-weak-set-has)
   ))

;;; Integer index is a canonical numeric string value in [0, 2^53-1],
;;; array index is an integer index in [0, 2^32-1>. -- ECMA V6.
(deftype integer-index-numeric () `(integer 0 ,(- (expt 2 53) 1)))

;;; In SBCL 32-bit version, it's OK.
(defconstant +number-max-safe-integer+ (- (expt 2 53) 1))
(defconstant +number-max-value+ most-positive-double-float)
(defconstant +number-min-safe-integer+ (- (- (expt 2 53) 1)))
(defconstant +number-min-value+ least-positive-double-float)

(defconstant +math-e+ (exp 1.0d0))
(defconstant +math-ln10+ (log 10.0d0))
(defconstant +math-ln2+ (log 2.0d0))
(defconstant +math-log10e+ (log +math-e+ 10))
(defconstant +math-log2e+ (log +math-e+ 2))
(defconstant +math-pi+ pi)
(defconstant +math-sqrt-1/2 (sqrt (/ 1.0d0 2.0d0)))
(defconstant +math-sqrt-2 (sqrt 2.0d0))

;;; In order to avoid re-definition of global functions and variables,
;;; make a hashtable to store those already created, and check every time
;;; when we have to define a global one instead of a scoped one.
;;; Since ES/JS know upper and lowercase, use STRING= instead of EQL or EQUAL.
(defvar *global-names*
  (make-hash-table :test 'string=))

