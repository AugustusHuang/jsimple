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

;;;; Date object.
(in-package :jsimple-builtin)

(defclass -date-prototype (-object-prototype)
  ((-prototype :initform '-object-prototype :allocation :class)
   (-date-value :type number-raw :initarg :-date-value)
   (constructor :initform (make-property :value '-date) :allocation :class)
   (properties
    :initform
    (append (fetch-properties (find-class '-object-prototype))
	    '(()))))
  (:documentation "Date prototype, provides inherited properties."))

(defclass -date (-function-prototype)
  ((-prototype :initform '-function-prototype :allocation :class)
   (length :initform (make-property :value 7) :allocation :class)
   (prototype :type (or property -null) :allocation :class :initarg :prototype
	      :initform (make-property :value '-date-prototype))
   (properties
    :initform
    (append (fetch-properties (find-class '-function-prototype))
	    '((now . (make-property :value 'now))
	      (parse . (make-property :value 'parse))
	      (utc . (make-property :value 'utc))))
    :allocation :class))
  (:documentation "Date constructor, used with new operator."))

(defmethod fetch-properties ((this -date-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -date))
  (properties (make-instance (class-name this))))

(defmethod get-date ((this -date-prototype))
  )

(defmethod get-day ((this -date-prototype))
  )

(defmethod get-full-year ((this -date-prototype))
  )

(defmethod get-hours ((this -date-prototype))
  )

(defmethod get-milliseconds ((this -date-prototype))
  )

(defmethod get-minutes ((this -date-prototype))
  )

(defmethod get-month ((this -date-prototype))
  )

(defmethod get-seconds ((this -date-prototype))
  )

(defmethod get-time ((this -date-prototype))
  )

(defmethod get-timezone-offset ((this -date-prototype))
  )

(defmethod get-utc-date ((this -date-prototype))
  )

(defmethod get-utc-day ((this -date-prototype))
  )

(defmethod get-utc-full-year ((this -date-prototype))
  )

(defmethod get-utc-hours ((this -date-prototype))
  )

(defmethod get-utc-milliseconds ((this -date-prototype))
  )

(defmethod get-utc-minutes ((this -date-prototype))
  )

(defmethod get-utc-month ((this -date-prototype))
  )

(defmethod get-utc-seconds ((this -date-prototype))
  )

(defmethod set-date ((this -date-prototype) date)
  )

(defmethod set-full-year ((this -date-prototype) year &optional month date)
  )

(defmethod set-hours ((this -date-prototype) hour &optional min sec ms)
  )

(defmethod set-milliseconds ((this -date-prototype) ms)
  )

(defmethod set-minutes ((this -date-prototype) min sec ms)
  )

(defmethod set-month ((this -date-prototype) month &optional date)
  )

(defmethod set-seconds ((this -date-prototype) sec &optional ms)
  )

(defmethod set-time ((this -date-prototype) time)
  )

(defmethod set-utc-date ((this -date-prototype) date)
  )

(defmethod set-utc-full-year ((this -date-prototype) year &optional month date)
  )

(defmethod set-utc-hours ((this -date-prototype) hour &optional min sec ms)
  )

(defmethod set-utc-milliseconds ((this -date-prototype) ms)
  )

(defmethod set-utc-minutes ((this -date-prototype) min &optional sec ms)
  )

(defmethod set-utc-month ((this -date-prototype) month &optional date)
  )

(defmethod set-utc-seconds ((this -date-prototype) sec &optional ms)
  )

(defmethod to-date-string ((this -date-prototype))
  )

(defmethod to-iso-string ((this -date-prototype))
  )

(defmethod to-json ((this -date-prototype) key)
  )

(defmethod to-locale-date-string ((this -date-prototype))
  )

(defmethod to-locale-string ((this -date-prototype))
  )

(defmethod to-locale-time-string ((this -date-prototype))
  )

(defmethod to-string ((this -date-prototype))
  )

(defmethod to-time-string ((this -date-prototype))
  )

(defmethod to-utc-string ((this -date-prototype))
  )

(defmethod value-of ((this -date-prototype))
  )

(defmethod to-primitive ((this -date-prototype) hint)
  )
