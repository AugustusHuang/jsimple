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

;;;; Builtin string type definitions.
(in-package :lesp-builtin)

(defclass -string-proto (-object-proto)
  ((-prototype :initform (find-class '-object-prototype))
   (-string-data :type string-raw :initarg :-string-data)
   (length :type property :initarg :length :initform (make-property :value 0))
   (constructor :initform (make-property :value -string) :allocation :class)
   (char-at :type property :allocation :class
	    :initform (make-property :value !char-at))
   (char-code-at :type property :allocation :class
		 :initform (make-property :value !char-code-at))
   (code-point-at :type property :allocation :class
		  :initform (make-property :value !code-point-at))
   (concat :type property :allocation :class
	   :initform (make-property :value !concat))
   (ends-with :type property :allocation :class
	      :initform (make-property :value !ends-with))
   (includes :type property :allocation :class
	     :initform (make-property :value !includes))
   (index-of :type property :allocation :class
	     :initform (make-property :value !index-of))
   (last-index-of :type property :allocation :class
		  :initform (make-property :value !last-index-of))
   (locale-compare :type property :allocation :class
		   :initform (make-property :value !locale-compare))
   (match :type property :allocation :class
	  :initform (make-property :value !match))
   (normalize :type property :allocation :class
	      :initform (make-property :value !normalize))
   (repeat :type property :allocation :class
	   :initform (make-property :value !repeat))
   (replace :type property :allocation :class
	    :initform (make-property :value !replace))
   (search :type property :allocation :class
	   :initform (make-property :value !search))
   (slice :type property :allocation :class
	  :initform (make-property :value !slice))
   (split :type property :allocation :class
	  :initform (make-property :value !split))
   (starts-with :type property :allocation :class
		:initform (make-property :value !starts-with))
   (substring :type property :allocation :class
	      :initform (make-property :value !substring))
   (to-locale-lower-case :type property :allocation :class
			 :initform (make-property :value !to-locale-lower-case))
   (to-locale-upper-case :type property :allocation :class
			 :initform (make-property :value !to-locale-upper-case))
   (to-lower-case :type property :allocation :class
		  :initform (make-property :value !to-lower-case))
   (to-upper-case :type property :allocation :class
		  :initform (make-property :value !to-upper-case))
   (trim :type property :allocation :class
	 :initform (make-property :value !trim))
   (iterator :type property :allocation :class
	     :initform (make-property :value !iterator)))
  (:documentation "String prototype, provides inherited properties."))

(defclass -string-iterator-proto (-iterator-proto)
  ((-prototype :initform '-iterator-proto)
   (-iterated-string :type -string-proto :initarg :-iterated-string)
   (-string-iterator-next-index :type integer
				:initarg :-string-iterator-next-index)
   (properties
    :initform
    (append (fetch-properties (find-class '-iterator-prototype))
	    '((next . (make-property :value 'next))
	      (to-string-tag . (make-property :value "String Iterator"
				:configurable :true))))
    :allocation :class))
  (:documentation "String iterator prototype, provides inherited properties to
all string iterator objects."))

(defparameter *string-undefined* (-string "undefined"))
(defparameter *string-null* (-string "null"))
(defparameter *string-true* (-string "true"))
(defparameter *string-false* (-string "false"))
(defparameter *string-nan* (-string "NaN"))
(defparameter *string-0* (-string "0"))
(defparameter *string-infinity* (-string "Infinity"))
(defparameter *string--infinity* (-string "-Infinity"))

(defmethod fetch-properties ((this -string-proto))
  (properties (make-instance (class-name this))))

(defun -to-string (arg)
  (typecase arg
    (undefined-type
     *string-undefined*)
    (null-type
     *string-null*)
    (boolean-type
     (if (eq arg *boolean-true*)
	 *string-true*
	 *string-false*))
    (number-type
     (cond ((eq arg *number-nan*)
	    *string-nan*)
	   ;; We need 0.0d0 equal 0. This is a safe operation.
	   ((equalp arg *number-0*)
	    *string-0*)
	   ((eq arg *number-infinity*)
	    *string-infinity*)
	   ((eq arg *number--infinity*)
	    *string--infinity*)
	   ((< (slot-value arg '-number-data) 0)
	    (-string (concatenate 'string
				  "-"
				  (-to-string (-number (- (slot-value arg '-nubmer-data)))))))
	   (t
	    ;; FIXME: Complete this.
	    )))
    (string-type
     arg)
    (symbol-type
     (error "Invalid type conversion from symbol to string"))
    (object-type
     (-to-string (-to-primitive arg :hint 'string)))))

(defun -string (&optional value)
  (let ((s (if value
	       (if (eql (-type value 'symbol-type)
			(property-value (cdr (assoc 'to-string-tag (fetch-properties value))))
			(-to-string value)))
	       "")))
    (make-instance '-string-proto :-string-data s)))

;;; Internal methods.
(defmethod -get-prototype-of ((this -string-proto))
  )

(defmethod -set-prototype-of ((this -string-proto) proto)
  )

(defmethod -is-extensible ((this -string-proto))
  )

(defmethod -prevent-extensions ((this -string-proto))
  )

(defmethod -get-own-property ((this -string-proto) key)
  )

(defmethod -has-property ((this -string-proto) key)
  )

(defmethod -get ((this -string-proto) key receiver)
  )

(defmethod -set ((this -string-proto) key value receiver)
  )

(defmethod -delete ((this -string-proto) key)
  )

(defmethod -define-own-property ((this -string-proto) key descriptor)
  )

(defmethod -enumerate ((this -string-proto))
  )

(defmethod -own-property-keys ((this -string-proto))
  )

;;; How about using CHAR-CODE lib function?
(defun from-char-code (&rest code-units)
  (let* ((len (length code-units))
	 (out (make-string len)))
    (loop for next in code-units
       and i from 0 to (1- len)
       for next-cu = (to-uint16 next)
       do (setf (char out i) (code-char next-cu)))
    (!string out)))

(defun from-code-point (&rest code-points)
  (let* ((len (length code-points))
	 (out (make-array len :element-type 'integer)))
    (loop for next in code-points
       and i from 0 to (1- len)
       for next-cp = (-to-number next)
       if (or (not (= next-cp (-to-integer next-cp)))
	      ;; 10FFFF is 1114111
	      (<= 0 next-cp 1114111))
	 ;; Range error.
	 (error "Range error.")
       do (setf (aref out i) (UTF16Encoding next-cp)))
    (!string out))

(defun raw (template &rest substitutions)
  (let ((cooked (-to-object template)))
    ))

(defmethod char-at ((this -string-proto) pos)
  (let ((posi (-to-integer pos)))
    (if (or (< 0 posi) (>= posi (slot-value this 'length)))
	(!string "")
	(!string (string (char (slot-value this '-string-data) posi))))))

(defmethod char-code-at ((this -string-proto) pos)
  (let ((posi (-to-integer pos)))
    (if (or (< 0 posi) (>= posi (slot-value this 'length)))
	*number-nan*
	(!number (char-code (char (slot-value this '-string-data) posi))))))

(defmethod code-point-at ((this -string-proto) pos)
  (let ((posi (-to-integer pos))
	(size (slot-value this 'length))
	(str (slot-value this '-string-data)))
    (if (or (< 0 posi) (>= posi size))
	:undefined
	(let ((fst (char-code (char str posi)))
	  ;; D800 = 55296, DBFF = 56319.
	  (if (or (< fst 55296) (> fst 56319) (= (1+ posi) size))
	      fst
	      (let ((snd (char-code (char str (1+ posi)))))
		;; DC00 = 56320, DFFF = 57343.
		(if (or (< snd 56320) (> snd 57343))
		    fst
		    (UTF16Decode(fst,snd))))))))))

(defmethod concat ((this -string-proto) &rest args)
  (let ((out (slot-value this '-string-data)))
    (dolist (arg args)
      (setf out (concatenate 'string out arg)))
    (!string out)))

(defmethod ends-with ((this -string-proto) search &optional end)
  (when (eql (type-of search) '-reg-exp-proto)
    (error "Type error."))
  (let* ((search-str (slot-value (-to-string search) '-string-data))
	 (this-str (slot-value this '-string-data))
	 (len (length this-str))
	 (end-pos (if end (slot-value (-to-integer end) '-number-data) len))
	 (end-position (!min (!max end-pos 0) len))
	 (start (- end-position (length search-str))))
    (when (< start 0)
      (return-from ends-with *boolean-false*))
    (if (string= search-str (subseq this-str start))
	*boolean-true*
	*boolean-false*)))

(defmethod includes ((this -string-proto) search &optional pos)
  (when (eql (type-of search) '-reg-exp-proto)
    (error "Type error."))
  (let* ((search-str (slot-value search '-string-data))
	 (this-str (slot-value this '-string-data))
	 (position (if pos (slot-value (-to-integer pos) '-number-data) 0))
	 (len (length this-str))
	 (start (!min (!max position 0) len)))
    (if (search search-str this-str :start2 start)
	*boolean-true*
	*boolean-false*)))

(defmethod index-of ((this -string-proto) search &optional pos)
  (let* ((search-str (slot-value (-to-string search) '-string-data))
	 (this-str (slot-value this '-string-data))
	 (position (if pos (slot-value (-to-integer pos) '-number-data) 0))
	 (len (length this-str))
	 (start (!min (!max position 0) len)))
    (let ((s (search search-str this-str :start2 start)))
      (if s
	  (!number s)
	  (!number -1)))))

(defmethod last-index-of ((this -string-proto) search &optional pos)
  (let* ((search-str (slot-value (-to-string search) '-string-data))
	 (this-str (slot-value this '-string-data))
	 (position (if pos (slot-value (-to-integer pos) '-number-data) 0))
	 (len (length this-str))
	 (start (!min (!max position 0) len)))
    (let ((s (search search-str this-str :start2 start :from-end t)))
      (if s
	  (!number s)
	  (!number -1)))))

(defmethod locale-compare ((this -string-proto) that)
  (let ((this-str (slot-value this '-string-data))
	(that-str (slot-value that '-string-data)))
    ))

(defmethod match ((this -string-proto) regexp)
  )

(defmethod normalize ((this -string-proto) &optional form)
  (let ((this-str (slot-value this '-string-data))
	(form (if form (slot-value form '-string-data) "NFC")))
    (when (not (member form ("NFC" "NFD" "NFKC" "NFKD")))
      (error "Range error."))
    ))

(defmethod repeat ((this -string-proto) count)
  (let ((this-str (slot-value this '-string-data))
	(n (slot-value (-to-integer count) '-number-data)))
    (if (or (eql n :nan) (eql n :infinity) (eql n :-infinity) (< n 0))
	(error "Range error.")
	(if (zerop n)
	    (!string "")
	    (let ((str ""))
	      (!string (loop repeat n
			    do (setf str (concatenate 'string str this-str)))))))))

(defmethod replace ((this -string-proto) search replace)
  )

(defmethod search ((this -string-proto) regexp)
  (let ((this-str (slot-value this '-string-data)))
    ))

(defmethod slice ((this -string-proto) start &optional end)
  (let* ((this-str (slot-value this '-string-data))
	 (len (length this-str))
	 (int-start (slot-value (-to-integer start) '-number-data))
	 (int-end (if end (slot-value (-to-integer end) '-number-data) len))
	 (from (if (or (< int-start 0) (eql int-start :-infinity))
		   (!max 0 (+ len int-start))
		   (!min len int-start)))
	 (to (if (or (< int-end 0) (eql int-end :-infinity))
		 (!max 0 (+ len int-end))
		 (!min len int-end))))
    (!string (subseq this-str from to))))

(defmethod split ((this -string-proto) separator &optional limit)
  )

(defmethod starts-with ((this -string-proto) search &optional pos)
  (let* ((this-str (slot-value this '-string-data))
	 (search-str (slot-value (-to-string search) '-string-data))
	 (position (if pos (slot-value (-to-integer pos) '-number-data) 0))
	 (len (length this-str))
	 (slen (length search-str))
	 (start (!min (!max position 0) len)))
    (if (string= (subseq this-str start (+ start slen)) search-str)
	*boolean-true*
	*boolean-false*)))

(defmethod substring ((this -string-proto) start &optional end)
  (let* ((this-str (slot-value this '-string-data))
	 (len (length this-str))
	 (int-start (-to-integer start))
	 (int-end (if end (-to-integer end) len))
	 (final-start (!min (!max int-start 0) len))
	 (final-end (!min (!max int-end 0) len))
	 (from (!min final-start final-end))
	 (to (!max final-start final-end)))
    (!string (subseq this-str from to))))

(defmethod to-locale-lower-case ((this -string-proto))
  )

(defmethod to-locale-upper-case ((this -string-proto))
  )

(defmethod to-lower-case ((this -string-proto))
  (!string (string-downcase (slot-value this '-string-data))))

(defmethod to-string ((this -string-proto))
  this)

(defmethod to-upper-case ((this -string-proto))
  (!string (string-upcase (slot-value this '-string-data))))

(defmethod trim ((this -string-proto))
  (let ((this-str (slot-value this '-string-data)))
    (!string (string-trim '(#\Space #\Return #\Newline #\LINE_SEPARATOR
			    #\PARAGRAPH_SEPARATOR) this-str))))

(defmethod value-of ((this -string-proto))
  this)

(defmethod iterator ((this -string-proto))
  )

