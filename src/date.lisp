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
(in-package :lesp-builtin)

(defclass -date-prototype (-object-prototype)
  ((-prototype :initform '-object-prototype)
   (-date-value :type date :initarg :-date-value)
   (constructor :initform (make-property :value '-date) :allocation :class)
   (properties
    :initform
    (append (fetch-properties (find-class '-object-prototype))
	    '(()))))
  (:documentation "Date prototype, provides inherited properties."))

;;; Internal DATE structure, uses ideas borrowed from LOCAL-TIME package by
;;; Daniel Lowe and Attila Lendvai. Here use Millisecond instead of nano one.

(defstruct date
  ((day 0 :type integer)
   (sec 0 :type integer)
   (msec 0 :type integer)))

(defmethod fetch-properties ((this -date-prototype))
  (properties (make-instance (class-name this))))

(defun count-day (year month date)
  )

(defun count-sec (hour minute second)
  )

;;; In this constructor function we don't assume UTC time, so we need to
;;; consider the timezone information.
(defun -date
    (&optional value year month &optional date hours minutes seconds ms)
  (if (null value)
      (if (null year)
	  (make-instance '-date-proto
			 :-date-value (make-date :day (get-current-day)
						 :sec (get-current-sec)
						 :msec (get-current-msec)))
	  (let ((y (-to-number year))
		(yr 0)
		(m (-to-number month))
		(dt (if date (-to-number date) 1))
		(h (if hours (-to-number hours) 0))
		(min (if minutes (-to-number minutes) 0))
		(s (if seconds (-to-number seconds) 0))
		(milli (if ms (-to-number ms) 0)))
	    (when (not (eql y :nan))
	      (let ((int-y (to-integer y)))
		(if (<= 0 int-y 99)
		    (setf yr (+ 1900 int-y))
		    (setf yr y))))
	    (make-instance '-date-proto
			   :-date-value (make-date :day (count-day yr m dt)
						   :sec (count-sec h min s)
						   :msec milli))))
      (let ((tv nil))
	(if (and (eql (-type value 'object-type))
		 (slot-exists-p value '-date-value))
	    (setf tv (slot-value value '-date-value))
	    (let ((v (to-primitive value)))
	      (if (eql (-type v 'string-type))
		  (setf tv (!parse v))
		  (setf tv (-to-number v)))))
	(make-instance '-date-proto
		       :-date-value tv))))

(defun !now ()
  (multiple-value-bind (sec usec) (sb-ext:get-time-of-day)
    (+ (* 1000 sec) (/ usec 1000))))

(defun !parse ()
  )

(defun !utc (year month &optional date hours minutes seconds ms)
  (let ((y (-to-number year))
	(yr 0)
	(m (-to-number month))
	(dt (if date (-to-number date) 1))
	(h (if hours (-to-number hours) 0))
	(min (if minutes (-to-number minutes) 0))
	(s (if seconds (-to-number seconds) 0))
	(milli (if ms (-to-number ms) 0)))
    (when (not (eql y :nan))
      (let ((int-y (to-integer y)))
	(if (<= 0 int-y 99)
	    (setf yr (+ 1900 int-y))
	    (setf yr y))))
    ;; Here the time is UTC, so do nothing on top of it.
    (make-date :day (count-day yr m dt)
	       :sec (count-sec h min s)
	       :msec milli)))

;;; Here in es land, date means the date of the month, and day means the day
;;; of the week, don't be confused.
(defmethod get-date ((this -date-prototype))
  (let ((tv (slot-value this '-date-value)))
    (DateFromTime(LocalTime(t)))))

(defmethod get-day ((this -date-prototype))
  (let ((tv (slot-value this '-date-value)))
    (WeekDay(LocalTime(t)))))

(defmethod get-full-year ((this -date-prototype))
  (let ((tv (slot-value this '-date-value)))
    (YearFromTime(LocalTime(t)))))

(defmethod get-hours ((this -date-prototype))
  (let ((tv (slot-value this '-date-value)))
    (HourFromTime(LocalTime(t)))))

(defmethod get-milliseconds ((this -date-prototype))
  (let ((tv (slot-value this '-date-value)))
    (msFromTime(LocalTime(t)))))

(defmethod get-minutes ((this -date-prototype))
  (let ((tv (slot-value this '-date-value)))
    ))

(defmethod get-month ((this -date-prototype))
  (let ((tv (slot-value this '-date-value)))
    ))

(defmethod get-seconds ((this -date-prototype))
  (let ((tv (slot-value this '-date-value)))
    ))

;;; XXX: Return the raw local time value, not the structure.
(defmethod get-time ((this -date-prototype))
  (slot-value this '-date-value))

(defmethod get-timezone-offset ((this -date-prototype))
  (let ((tv (slot-value this '-date-value)))
    ))

(defmethod get-utc-date ((this -date-prototype))
  (let ((tv (slot-value this '-date-value)))
    ))

(defmethod get-utc-day ((this -date-prototype))
  (let ((tv (slot-value this '-date-value)))
    ))

(defmethod get-utc-full-year ((this -date-prototype))
  (let ((tv (slot-value this '-date-value)))
    ))

(defmethod get-utc-hours ((this -date-prototype))
  (let ((tv (slot-value this '-date-value)))
    ))

(defmethod get-utc-milliseconds ((this -date-prototype))
  (let ((tv (slot-value this '-date-value)))
    ))

(defmethod get-utc-minutes ((this -date-prototype))
  (let ((tv (slot-value this '-date-value)))
    ))

(defmethod get-utc-month ((this -date-prototype))
  (let ((tv (slot-value this '-date-value)))
    ))

(defmethod get-utc-seconds ((this -date-prototype))
  (let ((tv (slot-value this '-date-value)))
    ))

(defmethod set-date ((this -date-prototype) date)
  (let ((tv (slot-value this '-date-value))
	(dt (-to-number date)))
    ))

(defmethod set-full-year ((this -date-prototype) year &optional month date)
  (let* ((tv (slot-value this '-date-value))
	 (y (-to-number year))
	 (m (if month (-to-number month) (count-month tv)))
	 (dt (if date (-to-number date) (count-date tv))))
    ))

(defmethod set-hours ((this -date-prototype) hour &optional min sec ms)
  (let* ((tm (local-time (slot-value this '-date-value)))
	 (h (-to-number hour))
	 (m (if min (-to-number min) (count-minute tm)))
	 (s (if sec (-to-number sec) (count-second tm)))
	 (milli (if ms (-to-number ms) (count-ms tm))))
    ))

(defmethod set-milliseconds ((this -date-prototype) ms)
  (let ((tm (local-time (slot-value this '-date-value)))
	(milli (-to-number ms)))
    ))

(defmethod set-minutes ((this -date-prototype) min sec ms)
  (let ((tm (local-time (slot-value this '-date-value)))
	(m (-to-number min))
	(s (if sec (-to-number sec) (count-second tm)))
	(milli (if ms (-to-number ms) (count-ms tm))))
    ))

(defmethod set-month ((this -date-prototype) month &optional date)
  (let ((tm (local-time (slot-value this '-date-value)))
	(m (-to-number month))
	(dt (if date (-to-number date) (count-date tm))))
    ))

(defmethod set-seconds ((this -date-prototype) sec &optional ms)
  (let ((tm (local-time (slot-value this '-date-value)))
	(s (-to-number sec))
	(milli (if ms (-to-number ms) (count-ms tm))))
    ))

;;; The internal function is not currently definite.
(defmethod set-time ((this -date-prototype) time)
  (let ((tm (-to-number time)))
    (setf (slot-value this '-date-value) (make-date :day (count-day time)
						    :sec (count-sec time)
						    :ms (count-ms time)))))

(defmethod set-utc-date ((this -date-prototype) date)
  (let ((tm (slot-value this '-date-value))
	(dt (-to-number date)))
    ))

(defmethod set-utc-full-year ((this -date-prototype) year &optional month date)
  (let ((tm (slot-value this '-date-value))
	(y (-to-number year))
	(m (if month (-to-number month) (count-month tm)))
	(dt (if date (-to-number date) (count-date tm))))
    ))

(defmethod set-utc-hours ((this -date-prototype) hour &optional min sec ms)
  (let ((tm (slot-value this '-date-value))
	(h (-to-number hour))
	(m (if min (-to-number min) (count-minute tm)))
	(s (if sec (-to-number sec) (count-second tm)))
	(milli (if ms (-to-number ms) (count-ms tm))))
    ))

(defmethod set-utc-milliseconds ((this -date-prototype) ms)
  (let ((tm (slot-value this '-date-value))
	(milli (-to-number ms)))
    ))

(defmethod set-utc-minutes ((this -date-prototype) min &optional sec ms)
  (let ((tm (slot-value this '-date-value))
	(m (-to-number min))
	(s (if sec (-to-number sec) (count-second tm)))
	(milli (if ms (-to-number ms) (count-ms tm))))
    ))

(defmethod set-utc-month ((this -date-prototype) month &optional date)
  (let ((tm (slot-value this '-date-value))
	(m (-to-number month))
	(dt (if date (-to-number date) (count-date tm))))
    ))

(defmethod set-utc-seconds ((this -date-prototype) sec &optional ms)
  (let ((tm (slot-value this '-date-value))
	(s (-to-number sec))
	(milli (if ms (-to-number ms) (count-ms tm))))
    ))

;;; Implementation dependent. Use form in LOCAL-TIME pack.
(defmethod to-date-string ((this -date-prototype))
  )

(defmethod to-iso-string ((this -date-prototype))
  )

;;; How to implement this?
(defmethod to-json ((this -date-prototype) key)
  )

(defmethod to-locale-date-string ((this -date-prototype))
  )

(defmethod to-locale-string ((this -date-prototype))
  )

(defmethod to-locale-time-string ((this -date-prototype))
  )

;;; Also implementation dependent.
(defmethod to-string ((this -date-prototype))
  (-to-date-string (slot-value this '-date-value)))

(defmethod to-time-string ((this -date-prototype))
  )

(defmethod to-utc-string ((this -date-prototype))
  )

(defmethod value-of ((this -date-prototype))
  (slot-value this '-date-value))

(defmethod to-primitive ((this -date-prototype) hint)
  )
