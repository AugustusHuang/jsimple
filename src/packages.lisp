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
	:lesp-error)
  (:documentation
   "Builtin feature package of Ecma-script 6.")
  ;; Will be a long list...
  (:export :camel-to-hyphen
	   :hyphen-to-camel
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
	   :has-own-property
	   :is-prototype-of
	   :property-is-enumerable
	   :to-locale-string
	   :to-string
	   :value-of
	   :!apply ;; APPLY is a dirty name.
	   :bind
	   :call
	   :has-instance
	   :to-exponential
	   :to-fixed
	   :to-precision
	   ))

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
