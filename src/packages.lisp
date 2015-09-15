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

(in-package :cl-user)

(defpackage :lesp-error
  (:use :cl)
  (:documentation
   "Error module of lesp.")
  ;; Those error functions should be wrapped by ES errors.
  (:export :general-error
	   :runtime-error
	   :jarithmetic-error
	   :divide-by-0-error
	   :invalid-index-error
	   :invalid-property-error
	   :invalid-function-error
	   :invalid-object-error
	   :invalid-class-error
	   :number-too-large-error))

(defpackage :lesp-parser
  (:use :cl
	:lesp-error)
  (:documentation
   "Parser of lesp.")
  (:export :*char*
	   :*line*
	   :*position*
	   :lexer-error
	   :parser-error
	   :parse-js
	   :parse-js-string))

(defpackage :lesp-builtin
  (:use :cl
	;; How about other implementations?
	:sb-pcl
	;; Use LOCAL-TIME pack to handle time and timezones.
	:local-time
	:lesp-error)
  (:documentation
   "Builtin feature package of Ecma-script 6.")
  ;; Will be a long list...
  (:export
   ;; Firstly utilities.
   :camel-to-hyphen
   :hyphen-to-camel
   ;; Then global objects and functions. If this function is a wrapped
   ;; funcallable instance, export their instances instead of raw functions.
   :!eval
   :!is-finite
   :!is-nan
   :!parse-float
   :!parse-int
   :!decode-uri
   :!decode-uri-component
   :!encode-uri
   :!encode-uri-component
   :!-array
   :!-array-buffer
   :!-boolean
   :!-data-view
   :!-date
   :!-error
   :!-eval-error
   :!-float32-array
   :!-float64-array
   :!-function
   :!-int8-array
   :!-int16-array
   :!-int32-array
   :!-map
   :!-number
   :!-object
   :!-proxy
   :!-promise
   :!-range-error
   :!-reference-error
   :!-reg-exp
   :!-set
   :!-string
   :!-symbol
   :!-syntax-error
   :!-type-error
   :!-uint8-array
   :!-uint8-clamped-array
   :!-uint16-array
   :!-uint32-array
   :!-uri-error
   :!-weak-map
   :!-weak-set
   ;; And then the internal methods.
   :-get-prototype-of
   :-set-prototype-of
   :-is-extensible
   :-prevent-extensions
   :-get-own-property
   :-has-property
   :-get
   :-set
   :-delete
   :-define-own-property
   :-enumerate
   :-own-property-keys
   :-call
   :-construct
   ;; Then the constructors' functions.
   :!assign
   :!create
   :!define-properties
   :!define-property
   :!freeze
   :!get-own-property-descriptor
   :!get-own-property-names
   :!get-own-property-symbols
   :!get-prototype-of
   :!is
   :!is-extensible
   :!is-frozen
   :!is-sealed
   :!keys
   :!prevent-extensions
   :!seal
   :!set-prototype-of
   :!for
   :!key-for
   :!is-integer
   :!is-safe-integer
   :!abs
   :!acos
   :!acosh
   :!asin
   :!asinh
   :!atan
   :!atanh
   :!atan2
   :!cbrt
   :!ceil
   :!clz32
   :!cos
   :!cosh
   :!exp
   :!expm1
   :!floor
   :!fround
   :!hypot
   :!imul
   :!log
   :!log1p
   :!log10
   :!log2
   :!max
   :!min
   :!pow
   :!random
   :!round
   :!sign
   :!sin
   :!sinh
   :!sqrt
   :!tan
   :!tanh
   :!trunc
   :!now
   :!parse
   :!utc
   :!from-char-code
   :!from-code-point
   :!raw
   :!from
   :!is-array
   :!of
   :!is-view
   :!stringify
   :!all
   :!race
   :!reject
   :!resolve
   :!revocable
   ;; And methods of the prototypes.
   :!has-own-property
   :!is-prototype-of
   :!property-is-enumerable
   :!to-locale-string
   :!to-string
   :!value-of
   :!apply
   :!bind
   :!call
   :!has-instance
   :!to-primitive
   :!to-exponential
   :!to-fixed
   :!to-precision
   :!get-date
   :!get-day
   :!get-full-year
   :!get-hours
   :!get-milliseconds
   :!get-minutes
   :!get-month
   :!get-seconds
   :!get-time
   :!get-timezone-offset
   :!get-utc-date
   :!get-utc-day
   :!get-utc-full-year
   :!get-utc-hours
   :!get-utc-milliseconds
   :!get-utc-minutes
   :!get-utc-month
   :!get-utc-seconds
   :!set-date
   :!set-full-year
   :!set-hours
   :!set-milliseconds
   :!set-minutes
   :!set-month
   :!set-seconds
   :!set-time
   :!set-utc-date
   :!set-utc-full-year
   :!set-utc-hours
   :!set-utc-milliseconds
   :!set-utc-minutes
   :!set-utc-month
   :!set-utc-seconds
   :!to-date-string
   :!to-isos-string
   :!to-json
   :!to-locale-date-string
   :!to-locale-time-string
   :!to-time-string
   :!to-utc-string
   :!char-at
   :!char-code-at
   :!code-point-at
   :!concat
   :!ends-with
   :!includes
   :!index-of
   :!last-index-of
   :!locale-compare
   :!match
   :!normalize
   :!repeat
   :!replace
   :!search
   :!slice
   :!split
   :!starts-with
   :!substring
   :!to-locale-lower-case
   :!to-locale-upper-case
   :!to-lower-case
   :!to-upper-case
   :!trim
   :!iterator
   :!next
   :!exec
   :!test
   :!copy-within
   :!entries
   :!every
   :!fill
   :!filter
   :!find
   :!find-index
   :!for-each
   :!join
   :!map
   :!pop
   :!push
   :!reduce
   :!reduce-right
   :!reverse
   :!shift
   :!some
   :!sort
   :!unshift
   :!values
   :!set
   :!subarray
   :!clear
   :!delete
   :!get
   :!has
   :!add
   :!get-float32
   :!get-float64
   :!get-int8
   :!get-int16
   :!get-int32
   :!get-uint8
   :!get-uint16
   :!get-uint32
   :!set-float32
   :!set-float64
   :!set-int8
   :!set-int16
   :!set-int32
   :!set-uint8
   :!set-uint16
   :!set-uint32
   :!return
   :!throw
   :!catch
   :!then))

(defpackage :lesp-ir
  (:use :cl
	:lesp-error
	:lesp-parser
	:lesp-builtin)
  (:documentation
   "Intermediate representation optimizer module of lesp.")
  (:export))

(defpackage :lesp-runtime
  (:use :cl
	:lesp-error
	:lesp-parser
	:lesp-builtin
	:lesp-ir)
  (:documentation
   "Runtime environment of lesp.")
  (:export :top-level))

(defpackage :lesp-test
  (:use :cl
	:lesp-error
	:lesp-runtime)
  (:documentation
   "Testsuite of lesp.")
  (:export))
