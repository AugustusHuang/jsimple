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

;;;; Generic methods definitions, since some of them will be used by different
;;;; types, move it into an one and only place.

;;; Here when we are facing error, we mean all kinds of errors,
;;; so EvalError will have the same generic functions set as Error.
;;; EvalError, RangeError, ReferenceError, SyntaxError, TypeError, URIError.
(defgeneric js-apply (func arg-array)
  (:documentation "XXX.prototype.apply function. Candidate: Function"))

(defgeneric js-assign (obj &rest sources)
  (:documentation "XXX.assign function. Candidate: Object"))

(defgeneric js-bind (func &rest args)
  (:documentation "XXX.prototype.bind function. Candidate: Function"))

(defgeneric js-call (func &rest args)
  (:documentation "XXX.prototype.call function. Candidate: Function"))

(defgeneric js-create (target proto)
  (:documentation "XXX.create function. Candidate: Object"))

(defgeneric js-define-properties (obj properties)
  (:documentation "XXX.defineProperties function. Candidate: Object"))

(defgeneric js-define-property (obj property attributes)
  (:documentation "XXX.defineProperty function. Candidate: Object"))

(defgeneric js-for (key)
  (:documentation "XXX.for function. Candidate: Symbol"))

(defgeneric js-freeze (obj)
  (:documentation "XXX.freeze function. Candidate: Object"))

(defgeneric js-from-char-code (string &rest code-units)
  (:documentation "XXX.fromCharCode function. Candidate: String"))

(defgeneric js-from-code-point (string &rest code-points)
  (:documentation "XXX.fromCodePoint function. Candidate: String"))

(defgeneric js-get-own-property-descriptor (obj property)
  (:documentation "XXX.getOwnPropertyDescriptor function. Candidate: Object"))

(defgeneric js-get-own-property-names (obj)
  (:documentation "XXX.getOwnPropertyNames function. Candidate: Object"))

(defgeneric js-get-own-property-symbols (obj)
  (:documentation "XXX.getOwnPropertySymbols function. Candidate: Object"))

(defgeneric js-get-prototype-of (obj)
  (:documentation "XXX.getPrototypeOf function. Candidate: Object"))

(defgeneric js-has-own-property (obj value)
  (:documentation "XXX.prototype.hasOwnProperty function. Candidate: Object"))

(defgeneric js-is (value1 value2)
  (:documentation "XXX.is function. Candidate: Object"))

(defgeneric js-is-extensible (obj)
  (:documentation "XXX.isExtensible function. Candidate: Object"))

(defgeneric js-is-frozen (obj)
  (:documentation "XXX.isFrozen function. Candidate: Object"))

(defgeneric js-is-prototype-of (obj value)
  (:documentation "XXX.prototype.isPrototypeOf function. Candidate: Object"))

(defgeneric js-is-sealed (obj)
  (:documentation "XXX.isSealed function. Candidate: Object"))

(defgeneric js-key-for (symbol)
  (:documentation "XXX.keyFor function. Candidate: Symbol"))

(defgeneric js-keys (obj)
  (:documentation "XXX.keys function. Candidate: Object"))

(defgeneric js-message (error-name)
  (:documentation "XXX.prototype.message function. Candidate: Error"))

(defgeneric js-name (error-name)
  (:documentation "XXX.prototype.name function. Candidate: Error"))

(defgeneric js-prevent-extensions (obj)
  (:documentation "XXX.preventExtensions. Candidate: Object"))

(defgeneric js-property-is-enumerable (obj value)
  (:documentation "XXX.prototype.propertyIsEnumerable function. Candidate: Object"))

(defgeneric js-length (func)
  (:documentation "XXX.length function. Candidate: Function"))

(defgeneric js-now (date)
  (:documentation "XXX.now function. Candidate: Date"))

(defgeneric js-parse (date string)
  (:documentation "XXX.parse function. Candidate: Date"))

(defgeneric js-raw (string template &rest substitutions)
  (:documentation "XXX.raw function. Candidate: String"))

(defgeneric js-seal (obj)
  (:documentation "XXX.seal function. Candidate: Object"))

(defgeneric js-set-prototype-of (obj proto)
  (:documentation "XXX.setPrototypeOf function. Candidate: Object"))

(defgeneric js-to-exponential (num fraction-digits)
  (:documentation "XXX.prototype.toExponential function. Candidate: Number"))

(defgeneric js-to-fixed (num fraction-digits)
  (:documentation "XXX.prototype.toFixed function. Candidate: Number"))

(defgeneric js-to-json (date key)
  (:documentation "XXX.prototype.toJSON function. Candidate: Date"))

(defgeneric js-to-locale-string (target)
  (:documentation "XXX.prototype.toLocaleString function. Candidate: Object,
Number, Date"))

(defgeneric js-to-precision (num precision)
  (:documentation "XXX.prototype.toPrecision function. Candidate: Number"))

(defgeneric js-to-string (target &optional radix)
  (:documentation "XXX.prototype.toString function. Candidate: Object,
Function, Boolean, Symbol, Error, Number, Date"))

(defgeneric js-utc (date year month &rest args)
  (:documentation "XXX.UTC function. Candidate: Date"))

(defgeneric js-value-of (target)
  (:documentation "XXX.prototype.valueOf function. Candidate: Object, Boolean,
Symbol, Number, Date"))

