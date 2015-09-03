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

(in-package :jsimple-builtin)

;;;; Generic methods definitions, since some of them will be used by different
;;;; types, move it into an one and only place.

;;; Here when we are facing error, we mean all kinds of errors,
;;; so EvalError will have the same generic functions set as Error.
;;; EvalError, RangeError, ReferenceError, SyntaxError, TypeError, URIError.

;;; Only include those multi-class generic methods.
(defgeneric js-add (this value)
  (:documentation "XXX.prototype.add(value). Candidate: Set, WeakSet"))

(defgeneric js-buffer (this)
  (:documentation "get XXX.prototype.buffer. Candidate: TypedArray, DataView"))

(defgeneric js-byte-length (this)
  (:documentation "get XXX.prototype.byteLength. Candidate: TypedArray,
ArrayBuffer, DataView"))

(defgeneric js-byte-offset (this)
  (:documentation "get XXX.prototype.byteOffset. Candidate: TypedArray,
DataView"))

(defgeneric js-clear (this)
  (:documentation "XXX.prototype.clear(). Candidate: Map, Set"))

(defgeneric js-code-point-at (this pos)
  (:documentation "XXX.prototype.codePointAt function. Candidate: String"))

(defgeneric js-concat (this &rest args)
  (:documentation "XXX.prototype.concat(...arguments). Candidate: String,
Array"))

(defgeneric js-copy-within (this target start &optional end)
  (:documentation "XXX.prototype.copyWithin(target,start[,end]). Candidate:
Array, TypedArray"))

;;; key and value.
(defgeneric js-delete (this key)
  (:documentation "XXX.prototype.delete(key). Candidate: Map, Set, WeakMap,
WeakSet"))

(defgeneric js-entries (this)
  (:documentation "XXX.prototype.entries(). Candidate: Array, TypedArray,
Map, Set"))

(defgeneric js-every (this callback &optional arg)
  (:documentation "XXX.prototype.every(callbackfn[,thisArg]). Candidate: Array,
TypedArray"))

(defgeneric js-fill (this value &optional start end)
  (:documentation "XXX.prototype.fill(value[,start[,end]]). Candidate: Array,
TypedArray"))

(defgeneric js-filter (this callback &optional arg)
  (:documentation "XXX.prototype.filter(callbackfn[,thisArg]). Candidate:
Array, TypedArray"))

(defgeneric js-find (this predicate &optional arg)
  (:documentation "XXX.prototype.find(predicate[,thisArg]). Candidate: Array,
TypedArray"))

(defgeneric js-find-index (this predicate &optional arg)
  (:documentation "XXX.prototype.findIndex(predicate[,thisArg]). Candidate:
Array, TypedArray"))

(defgeneric js-for-each (this callback &optional arg)
  (:documentation "XXX.prototype.forEach(callbackfn[,thisArg]). Candidate:
Array, TypedArray, Map, Set"))

(defgeneric js-from (this items &optional map arg)
  (:documentation "XXX.from(items[,mapfn[,thisArg]]). Candidate: Array,
TypedArray"))

(defgeneric js-get (this key)
  (:documentation "XXX.prototype.get(key). Candidate: Map, WeakMap"))

;;; key or value.
(defgeneric js-has (this key)
  (:documentation "XXX.prototype.has(key). Candidate: Map, Set, WeakMap,
WeakSet"))

(defgeneric js-index-of (this search &optional from)
  (:documentation "XXX.prototype.indexOf(searchElement[,fromIndex]). Candidate:
String, Array, TypedArray"))

(defgeneric js-join (this separator)
  (:documentation "XXX.prototype.join(separator). Candidate: Array,
TypedArray"))

(defgeneric js-keys (this)
  (:documentation "XXX.keys(). Candidate: Object or XXX.prototype.keys().
Candidate: Array, TypedArray, Map, Set"))

(defgeneric js-last-index-of (this search &optional from)
  (:documentation "XXX.prototype.lastIndexOf(searchElement[,fromIndex]).
Candidate: Array, TypedArray"))

(defgeneric js-length (this)
  (:documentation "get XXX.prototype.length. Candidate: Function, TypedArray"))

(defgeneric js-map (this callback &optional arg)
  (:documentation "XXX.prototype.map(callbackfn[,thisArg]. Candidate: Array,
TypedArray"))

(defgeneric js-message (this)
  (:documentation "XXX.prototype.message function. Candidate: Error,
TypedError"))

(defgeneric js-name (this)
  (:documentation "XXX.prototype.name function. Candidate: Error, TypedError"))

(defgeneric js-of (this &rest items)
  (:documentation "XXX.of(...items). Candidate: Array, TypedArray"))

(defgeneric js-reduce (this callback &optional initial)
  (:documentation "XXX.prototype.reduce(callbackfn[,initialValue]). Candidate:
Array, TypedArray"))

(defgeneric js-reduce-right (this callback &optional initial)
  (:documentation "XXX.prototype.reduceRight(callbackfn[,initialValue]).
Candidate: Array, TypedArray"))

(defgeneric js-reverse (this)
  (:documentation "XXX.prototype.reverse(). Candidate: Array, TypedArray"))

(defgeneric js-set (this key value)
  (:documentation "XXX.prototype.set(key,value). Candidate: Map, WeakMap"))

(defgeneric js-size (this)
  (:documentation "get XXX.prototype.size. Candidate: Map, Set"))

(defgeneric js-slice (this start end)
  (:documentation "XXX.prototype.slice(start,end). Candidate: String, Array,
TypedArray, ArrayBuffer"))

(defgeneric js-some (this callback &optional arg)
  (:documentation "XXX.prototype.some(callbackfn[,thisArg]). Candidate: Array,
TypedArray"))

(defgeneric js-sort (this compare)
  (:documentation "XXX.prototype.sort(comparefn). Candidate: Array,
TypedArray"))

(defgeneric js-to-locale-string (this &optional reserved1 reserved2)
  (:documentation "XXX.prototype.toLocaleString([reserved1[,reserved2]]).
Candidate: Object, Date, Array, TypedArray"))

(defgeneric js-to-string (this)
  (:documentation "XXX.prototype.toString(). Candidate: Object, Function,
Boolean, Symbol, Error, Date, String, RegExp, Array, TypedArray"))

(defgeneric js-values (this)
  (:documentation "XXX.prototype.values(). Candidate: Array, TypedArray, Map,
Set"))

(defgeneric js-value-of (this)
  (:documentation "XXX.prototype.valueOf(). Candidate: Object, Boolean,
Symbol, Date, String"))

