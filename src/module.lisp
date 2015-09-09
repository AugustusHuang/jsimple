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

(defclass module-namespace ()
  ((-module :initarg :-module :type module-record)
   (-exports :initarg :-exports :type (list string))
   (to-string-tag :type property :initarg :to-string-tag
		  :initform (make-property :value "Module"
					   :configurable :true))
   (iterator :type property :initarg :iterator
	     :initform (make-property :value !iterator)))
  (:documentation "Module namespace object."))

(defstruct module-record
  (-realm :undefined :type (or -undefined realm-record))
  (-environment :undefined :type (or -undefined lexical-environment))
  (-namespace :undefined :type (or module-namespace -undefined))
  (-evaluated :false :type boolean-raw))

(defmethod -get-prototype-of ((this module-namespace))
  :null)

(defmethod -set-prototype-of ((this module-namespace) proto)
  (assert (or (type-of proto -object-prototype)
	      (type-of proto -null))
	  (this proto)
	  "PROTO is not of type Object or Null.")
  (-boolean :false))

(defmethod -is-extensible ((this module-namespace))
  (-boolean :false))

(defmethod -prevent-extensions ((this module-namespace))
  (-boolean :true))

(defmethod -get-own-property ((this module-namespace) key)
  )

(defmethod -define-own-property ((this module-namespace) key descriptor)
  (-boolean :false))

(defmethod -has-property ((this module-namespace) key)
  )

(defmethod -get ((this module-namespace) key receiver)
  )

(defmethod -set ((this module-namespace) key value receiver)
  (-boolean :false))

(defmethod -delete ((this module-namespace) key)
  )

(defmethod -enumerate ((this module-namespace))
  )

(defmethod -own-property-keys ((this module-namespace))
  )

(defun create-module-namespace (module exports)
  )
