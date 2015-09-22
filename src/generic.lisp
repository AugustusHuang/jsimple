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

;;;; Generic methods definitions, since some of them will be used by different
;;;; types, move it into an one and only place.
(defgeneric -get-prototype-of (this)
  (:documentation "Determine the object that provides inherited properties
for this object. A :NULL value indicates that there are not inherited
properties."))

(defgeneric -set-prototype-of (this object)
  (:documentation "Associate this object with another object that provides
inherited properties. Passing :NULL indicates that there are no inherited
properties. Returns :TRUE indicating that the operation was completed
successfully or :FALSE indicating that the operation was not successful."))

(defgeneric -is-extensible (this)
  (:documentation "Determine whether it is permitted to add additional
properties to this object."))

(defgeneric -prevent-extensions (this)
  (:documentation "Control whether new properties may be added to this object.
Returns :TRUE if the operation was successful of :FALSE if the operation was
unsuccessful."))

(defgeneric -get-own-property (this key)
  (:documentation "Return a property descriptor for the own property of this
object whose key is KEY, or :UNDEFINED if no such property exists."))

(defgeneric -has-property (this key)
  (:documentation "Return a Boolean value indicating whether this object
already has either an own or inherited property whose key is KEY."))

(defgeneric -get (this key receiver)
  (:documentation "Return the value of the property whose key is KEY from this
object. If any code must be executed to retrieve the property value, RECEIVER
is used as the 'this' value when evaluating the code."))

(defgeneric -set (this key value receiver)
  (:documentation "Set the value of the property whose key is KEY to VALUE. If
any code must be executed to set the property value, RECEIVER is used as the
'this' value when evaluating the code. Returns :TRUE if the property value was
set or :FALSE if it could not be set."))

(defgeneric -delete (this key)
  (:documentation "Remove the own property whose key is KEY from this object.
Return :FALSE if the property was not deleted and is still present. Return
:TRUE if the property was deleted or is not present."))

(defgeneric -define-own-property (this key descriptor)
  (:documentation "Create or alter the own property, whose key is KEY, to have
the state described by DESCRIPTOR. Return :TRUE if that property was
successfully created/updated or :FALSE if the property could not be created
or updated."))

(defgeneric -enumerate (this)
  (:documentation "Return an iterator object that produces the keys of the
string-keyed enumerable properties of the object."))

(defgeneric -own-property-keys (this)
  (:documentation "Return a list whose elements are all of the own property
keys for the object."))

(defgeneric -call (this &rest args)
  (:documentation "Executes code associated with this object. Invoked via a
function call expression. The arguments to the internal method are a 'this'
value and a list containing the arguments passed to the function by a call
expression. Objects that implement this internal method are callable."))

(defgeneric -construct (this args object)
  (:documentation "Creates an object. Invoked via the 'new' or 'super'
operators. The first argument to the internal method is a list containing the
arguments of the operator. The second argument is the object to which the 'new'
operator was initially applied. Objects that implement this internal method
are called constructors. A function object is not necessarily a constructor
and such non-constructor function objects do not have a [[Construct]] internal
method."))

(defgeneric %has-own-property (this value)
  (:documentation ""))

(defgeneric %is-prototype-of (this value)
  (:documentation ""))

(defgeneric %property-is-enumerable (this value)
  (:documentation ""))

(defgeneric %to-locale-string (this)
  (:documentation ""))

;;; A prototype other than Number simple (DECLARE (IGNORE RADIX)).
(defgeneric %to-string (this &optional radix)
  (:documentation ""))

(defgeneric %value-of (this)
  (:documentation "Return a type converted value of THIS."))

(defgeneric %apply (this this-arg args)
  (:documentation ""))

(defgeneric %bind (this this-arg &rest args)
  (:documentation ""))

(defgeneric %call (this this-arg &rest args)
  (:documentation ""))

(defgeneric %has-instance (this value)
  (:documentation ""))

(defgeneric %to-exponential (this digits)
  (:documentation ""))

(defgeneric %to-fixed (this digits)
  (:documentation ""))

(defgeneric %to-precision (this precision)
  (:documentation ""))

(defgeneric %char-at (this pos)
  (:documentation ""))

(defgeneric %char-code-at (this pos)
  (:documentation ""))

(defgeneric %code-point-at (this pos)
  (:documentation ""))

(defgeneric %concat (this &rest args)
  (:documentation ""))

(defgeneric %ends-with (this search &optional end)
  (:documentation ""))

(defgeneric %includes (this search &optional pos)
  (:documentation ""))

(defgeneric %index-of (this search &optional pos)
  (:documentation ""))

(defgeneric %last-index-of (this search &optional pos)
  (:documentation ""))

(defgeneric %locale-compare (this that)
  (:documentation ""))

(defgeneric %match (this regexp)
  (:documentation ""))

(defgeneric %normalize (this &optional form)
  (:documentation ""))

(defgeneric %repeat (this count)
  (:documentation ""))

(defgeneric %replace (this search replace)
  (:documentation ""))

(defgeneric %search (this regexp)
  (:documentation ""))

(defgeneric %slice (this start end)
  (:documentation ""))

(defgeneric %split (this separator limit)
  (:documentation ""))

(defgeneric %starts-with (this search &optional pos)
  (:documentation ""))

(defgeneric %substring (this start end)
  (:documentation ""))

(defgeneric %to-locale-lower-case (this)
  (:documentation ""))

(defgeneric %to-locale-upper-case (this)
  (:documentation ""))

(defgeneric %to-lower-case (this)
  (:documentation ""))

(defgeneric %to-upper-case (this)
  (:documentation ""))

(defgeneric %trim (this)
  (:documentation ""))

(defgeneric %iterator (this)
  (:documentation ""))

(defgeneric %next (this)
  (:documentation ""))

(defgeneric %exec (this string)
  (:documentation "Performs a regular expression match of STRING against the
regular expression and returns an Array object containing the results of the
match, or :NULL if STRING did not match."))

(defgeneric %test (this string)
  (:documentation ""))

(defgeneric %copy-within (this target start &optional end)
  (:documentation ""))

(defgeneric %entries (this)
  (:documentation ""))

(defgeneric %every (this callback &optional this-arg)
  (:documentation ""))

(defgeneric %fill (this value &optional start end)
  (:documentation ""))

(defgeneric %filter (this callback &optional this-arg)
  (:documentation ""))

(defgeneric %find (this predicate &optional this-arg)
  (:documentation ""))

(defgeneric %find-index (this predicate &optional this-arg)
  (:documentation ""))

(defgeneric %for-each (this callback &optional this-arg)
  (:documentation ""))

(defgeneric %join (this separator)
  (:documentation ""))

(defgeneric %map (this callback &optional this-arg)
  (:documentation ""))

(defgeneric %pop (this)
  (:documentation ""))

(defgeneric %push (this &rest items)
  (:documentation ""))

(defgeneric %reduce (this callback &optional initial)
  (:documentation ""))

(defgeneric %reduce-right (this callback &optional initial)
  (:documentation ""))

(defgeneric %reverse (this)
  (:documentation ""))

(defgeneric %shift (this)
  (:documentation ""))

(defgeneric %some (this callback &optional this-arg)
  (:documentation ""))

(defgeneric %sort (this compare)
  (:documentation "The elements of this array are sorted, if COMPARE is given
it should be a function that accepts two arguments X and Y and returns a
negative value if X < Y, zero if X = Y or a positive value if X > Y."))

(defgeneric %unshift (this &rest items)
  (:documentation ""))

(defgeneric %values (this)
  (:documentation ""))

(defgeneric %set (this array &optional offset)
  (:documentation "Sets multiple values in THIS, reading the values from the
object ARRAY, OFFSET value indicates the first element index in THIS where
values are written, if omitted, is assumed to be 0."))

(defgeneric %subarray (this &optional begin end)
  (:documentation "Returns a new typed-array whose element type is the same as
THIS and whose ArrayBuffer is the same as the ArrayBuffer of THIS, inclusively
from BEGIN and up to END exclusively, if either is negative, it refers to an
index from the end of the array."))

(defgeneric %clear (this)
  (:documentation ""))

(defgeneric %delete (this key)
  (:documentation ""))

(defgeneric %get (this key)
  (:documentation ""))

(defgeneric %has (this key)
  (:documentation ""))

(defgeneric %add (this value)
  (:documentation ""))

(defgeneric %get-float32 (this byte-offset &optional endian)
  (:documentation ""))

(defgeneric %get-float64 (this byte-offset &optional endian)
  (:documentation ""))

(defgeneric %get-int8 (this byte-offset)
  (:documentation ""))

(defgeneric %get-int16 (this byte-offset &optional endian)
  (:documentation ""))

(defgeneric %get-int32 (this byte-offset &optional endian)
  (:documentation ""))

(defgeneric %get-uint8 (this byte-offset)
  (:documentation ""))

(defgeneric %get-uint16 (this byte-offset &optional endian)
  (:documentation ""))

(defgeneric %get-uint32 (this byte-offset &optional endian)
  (:documentation ""))

(defgeneric %set-float32 (this byte-offset value &optional endian)
  (:documentation ""))

(defgeneric %set-float64 (this byte-offset value &optional endian)
  (:documentation ""))

(defgeneric %set-int8 (this byte-offset value)
  (:documentation ""))

(defgeneric %set-int16 (this byte-offset value &optional endian)
  (:documentation ""))

(defgeneric %set-int32 (this byte-offset value &optional endian)
  (:documentation ""))

(defgeneric %set-uint8 (this byte-offset value)
  (:documentation ""))

(defgeneric %set-uint16 (this byte-offset value &optional endian)
  (:documentation ""))

(defgeneric %set-uint32 (this byte-offset value &optional endian)
  (:documentation ""))

(defgeneric %return (this value)
  (:documentation ""))

(defgeneric %throw (this exception)
  (:documentation ""))

(defgeneric %catch (this on-rejected)
  (:documentation ""))

(defgeneric %then (this on-fulfilled on-rejected)
  (:documentation ""))

