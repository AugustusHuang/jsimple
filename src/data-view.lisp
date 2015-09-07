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

(defclass -data-view-prototype (-object-prototype)
  ((-prototype :initform '-object-prototype)
   ;; This internal slot is not used now.
   (-data-view :type null :initform nil)
   ;; TODO: Decide their types!
   (-viewed-array-buffer)
   (-byte-length)
   (-byte-offset)
   (constructor :initform '-data-view :allocation :class)
   (properties
    :initform
    (append '((buffer . (make-property :get 'buffer))
	      (byte-length . (make-property :get 'byte-length))
	      (byte-offset . (make-property :get 'byte-offset))
	      (get-float32 . (make-property :get 'get-float32))
	      (get-float64 . (make-property :get 'get-float64))
	      (get-int8 . (make-property :value 'get-int8))
	      (get-int16 . (make-property :value 'get-int16))
	      (get-int32 . (make-property :value 'get-int32))
	      (get-uint8 . (make-property :value 'get-uint8))
	      (get-uint16 . (make-property :value 'get-uint16))
	      (get-uint32 . (make-property :value 'get-uint32))
	      (set-float32 . (make-property :value 'set-float32))
	      (set-float64 . (make-property :value 'set-float64))
	      (set-int8 . (make-property :value 'set-int8))
	      (set-int16 . (make-property :value 'set-int16))
	      (set-int32 . (make-property :value 'set-int32))
	      (set-uint8 . (make-property :value 'set-uint8))
	      (set-uint16 . (make-property :value 'set-uint16))
	      (set-uint32 . (make-property :value 'set-uint32))
	      (to-string-tag . (make-property :value "DataView"
				:configurable :true)))
	    (fetch-properties (find-class '-function-prototype)))
    :allocation :class))
  (:documentation ""))

(defclass -data-view (-function-prototype)
  ((-prototype :initform '-function-prototype)
   (length :initform (make-property :value 3) :allocation :class)
   (prototype :type (or property -null) :allocation :class :initarg :prototype
	      :initform (make-property :value '-data-view-prototype))
   (properties :initform (fetch-properties (find-class '-function-prototype))
	       :allocation :class))
  (:documentation "Data view constructor, used only with new operator."))

