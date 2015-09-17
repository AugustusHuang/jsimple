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
(defclass -array-proto (-object-proto)
  ((-prototype :initform (find-class '-object-proto))
   (-array-data :type array :initarg :-array-data)
   (length :type property :initarg :length
	   :initform (make-property :value 0 :writable :true))
   (constructor :initform (make-property :value -array) :allocation :class)
   (concat :type property :allocation :class
	   :initform (make-property :value !concat))
   (copy-within :type property :allocation :class
		:initform (make-property :value !copy-within))
   (entries :type property :allocation :class
	    :initform (make-property :value !entries))
   (every :type property :allocation :class
	  :initform (make-property :value !every))
   (fill :type property :allocation :class
	 :initform (make-property :value !fill))
   (filter :type property :allocation :class
	   :initform :filter (make-property :value !filter))
   (find :type property :allocation :class
	 :initform (make-property :value !find))
   (find-index :type property :allocation :class
	       :initform (make-property :value !find-index))
   (for-each :type property :allocation :class
	     :initform (make-property :value !for-each))
   (index-of :type property :allocation :class
	     :initform (make-property :value !index-of))
   (join :type property :allocation :class
	 :initform (make-property :value !join))
   (keys :type property :allocation :class
	 :initform (make-property :value !keys))
   (last-index-of :type property :allocation :class
		  :initform (make-property :value !last-index-of))
   (map :type property :allocation :class
	:initform (make-property :value !map))
   (pop :type property :allocation :class
	:initform (make-property :value !pop))
   (push :type property :allocation :class
	 :initform (make-property :value !push))
   (reduce :type property :allocation :class
	   :initform (make-property :value !reduce))
   (reduce-right :type property :allocation :class
		 :initform (make-property :value !reduce-right))
   (reverse :type property :allocation :class
	    :initform (make-property :value !reverse))
   (shift :type property :allocation :class
	  :initform (make-property :value !shift))
   (slice :type property :allocation :class
	  :initform (make-property :value !slice))
   (some :type property :allocation :class
	 :initform (make-property :value !some))
   (sort :type property :allocation :class
	 :initform (make-property :value !sort))
   (splice :type property :allocation :class
	   :initform (make-property :value !splice))
   (unshift :type property :allocation :class
	    :initform (make-property :value !unshift))
   (values :type property :allocation :class
	   :initform (make-property :value !values))
   (iterator :type property :allocation :class
	     :initform (make-property :value !iterator))
   ;; Refer to ECMA-262 version 6 p.424.
   (unscopables :type property :allocation :class
		:initform (make-property :value !unscopables
					 :configurable :true)))
  (:documentation "Array prototype, provides inherited properties."))

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

(defun -array-0 (&optional len)
  )

(defun -array-1 (item1 item2 &rest items)
  )

;;; Typed arrays.
(defclass -int8-array-proto (-array-proto)
  ()
  (:documentation "Int8 array prototype."))

(defclass -int16-array-proto (-array-proto)
  ()
  (:documentation "Int16 array prototype."))

(defclass -int32-array-proto (-array-proto)
  ()
  (:documentation "Int32 array prototype."))

(defclass -float32-array-proto (-array-proto)
  ()
  (:documentation "Float32 array prototype."))

(defclass -float64-array-proto (-array-proto)
  ()
  (:documentation "Float64 array prototype."))

(defclass -uint8-array-proto (-array-proto)
  ()
  (:documentation "Unsigned int8 array prototype."))

(defclass -uint8-clamped-array-proto (-array-proto)
  ()
  (:documentation "Unsigned clamped int8 array prototype."))

(defclass -uint16-array-proto (-array-proto)
  ()
  (:documentation "Unsigned int16 array prototype."))

(defclass -uint32-array-proto (-array-proto)
  ()
  (:documentation "Unsigned int32 array prototype."))

(defun -int8-array ()
  )

(defun -int16-array ()
  )

(defun -int32-array ()
  )

(defun -float32-array ()
  )

(defun -float64-array ()
  )

(defun -uint8-array ()
  )

(defun -uint8-clamped-array ()
  )

(defun -uint16-array ()
  )

(defun -uint32-array ()
  )

(defmethod print-object ((this -array-proto) stream)
  )

(defmethod -define-own-property ((this -array-proto) key descriptor)
  )

(defmethod -get-own-property ((this -array-proto) key)
  )

(defmethod -has-property ((this -array-proto) key)
  )

(defmethod -get ((this -array-proto) key receiver)
  )

(defmethod -set ((this -array-proto) key value receiver)
  )

(defmethod -own-property-keys ((this -array-proto))
  )

(defun %from (items &optional mapfn arg)
  )

(defun %is-array (arg)
  )

(defun %of (&rest items)
  )

(defun concat ((this -array-proto) &rest args)
  )

(defun copy-within ((this -array-proto) target start &optional end)
  )

(defun entries ((this -array-proto))
  )

(defun every ((this -array-proto) callback &optional arg)
  )

(defun fill ((this -array-proto) value &optional start end)
  )

(defun filter ((this -array-proto) callback &optional arg)
  )

(defun find ((this -array-proto) predicate &optional arg)
  )

(defun find-index ((this -array-proto) predicate &optional arg)
  )

(defun for-each ((this -array-proto) callback &optional arg)
  )

(defun index-of ((this -array-proto) search &optional from)
  )

(defun join ((this -array-proto) separator)
  )

(defun keys ((this -array-proto))
  )

(defun map ((this -array-proto) callback &optional arg)
  )

(defun pop ((this -array-proto))
  )

(defun push ((this -array-proto) &rest items)
  )

(defun reduce ((this -array-proto) callback &optional initial)
  )

(defun reduce-right ((this -array-proto) callback &optional initial)
  )

(defun reverse ((this -array-proto))
  )

(defun shift ((this -array-proto))
  )

(defun slice ((this -array-proto) start end)
  )

(defun some ((this -array-proto) callback &optional arg)
  )

(defun sort ((this -array-proto) compare)
  )

(defun splice ((this -array-proto) start delete &rest items)
  )

(defun to-locale-string ((this -array-proto))
  )

(defun to-string ((this -array-proto))
  )

(defun unshift ((this -array-proto) &rest items)
  )

(defun values ((this -array-proto))
  )

(defun iterator ((this -array-proto))
  )
