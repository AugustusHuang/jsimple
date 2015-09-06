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

;;;; Builtin array definitions.
(defclass -array-prototype (-object-prototype)
  ((-prototype :initform '-object-prototype :allocation :class)
   (length :type integer :initarg :length
	   :initform (make-property :value 0 :writable :true))
   (constructor :initform '-array :allocation :class)
   (properties
    :initform
    (append (fetch-properties (find-class '-object-prototype))
	    '((concat . (make-property :value 'concat))
	      (copy-within . (make-property :value 'copy-within))
	      (entries . (make-property :value 'entries))
	      (every . (make-property :value 'every))
	      (fill . (make-property :value 'fill))
	      (filter . (make-property :value 'filter))
	      (find . (make-property :value '!find))
	      (find-index . (make-property :value 'find-index))
	      (for-each . (make-property :value 'for-each))
	      (index-of . (make-property :value 'index-of))
	      (join . (make-property :value 'join))
	      (keys . (make-property :value 'keys))
	      (last-index-of . (make-property :value 'last-index-of))
	      (map . (make-property :value '!map))
	      (pop . (make-property :value '!pop))
	      (push . (make-property :value '!push))
	      (reduce . (make-property :value '!reduce))
	      (reduce-right . (make-property :value 'reduce-right))
	      (reverse . (make-property :value '!reverse))
	      (shift . (make-property :value 'shift))
	      (slice . (make-property :value 'slice))
	      (some . (make-property :value '!some))
	      (sort . (make-property :value '!sort))
	      (splice . (make-property :value 'splice))
	      (unshift . (make-property :value 'unshift))
	      (values . (make-property :value '!values))
	      (iterator . (make-property :value 'iterator))
	      ;; Refer to ECMA-262 version 6 p.424.
	      (unscopables . (make-property :value (create)
			      :configurable :true))))
    :allocation :class)
  (:documentation "Array prototype, provides inherited properties."))

(defclass -array (-function-prototype)
  ((-prototype :initform '-function-prototype :allocation :class)
   (length :initform (make-property :value 1) :allocation :class)
   (prototype :type (or property -null) :allocation :class :initarg :prototype
	      :initform '-array-prototype)
   (properties
    :initform
    (append (fetch-properties (find-class '-function-prototype))
	    '((from . (make-property :value 'from))
	      (is-array . (make-property :value 'is-array))
	      (of . (make-property :value 'of))
	      (species . (make-property :get 'species))))
    :allocation :class))
  (:documentation "Builtin array prototype."))

(defclass -array-iterator-prototype (-iterator-prototype)
  ((-prototype :initform '-iterator-prototype :allocation :class)
   (-iterated-object :type -object-prototype :initarg :-iterated-object)
   (-array-iterator-next-index :type integer
			       :initarg :-array-iterator-next-index)
   ;; Possible values are "key" "value" and "key+value".
   (-array-iteration-kind :type string
			  :initarg :-array-iteration-kind)
   (properties
    :initform
    (append (fetch-properties (find-class '-iterator-prototype))
	    '((next . (make-property :value 'next))
	      (to-string-tag . (make-property :value "Array Iterator"
				:configurable :true))))
    :allocation :class))
  (:documentation "Array iterator prototype, provides inherited array iterator
properties"))

;;; Typed arrays.
(defclass -int8-array-prototype ()
  ()
  (:documentation ""))

(defclass -int8-array ()
  ()
  (:documentation ""))

(defclass -int16-array-prototype ()
  ()
  (:documentation ""))

(defclass -int16-array ()
  ()
  (:documentation ""))

(defclass -int32-array-prototype ()
  ()
  (:documentation ""))

(defclass -int32-array ()
  ()
  (:documentation ""))

(defclass -float32-array-prototype ()
  ()
  (:documentation ""))

(defclass -float32-array ()
  ()
  (:documentation ""))

(defclass -float64-array-prototype ()
  ()
  (:documentation ""))

(defclass -float64-array ()
  ()
  (:documentation ""))

(defclass -uint8-array-prototype ()
  ()
  (:documentation ""))

(defclass -uint8-array ()
  ()
  (:documentation ""))

(defclass -uint8-clamped-array-prototype ()
  ()
  (:documentation ""))

(defclass -uint8-clamped-array ()
  ()
  (:documentation ""))

(defclass -uint16-array-prototype ()
  ()
  (:documentation ""))

(defclass -uint16-array ()
  ()
  (:documentation ""))

(defclass -uint32-array-prototype ()
  ()
  (:documentation ""))

(defclass -uint32-array ()
  ()
  (:documentation ""))

