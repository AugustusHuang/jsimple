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

;;; NOTE: Some of them will be removed if candidate is not a class.
(defgeneric js-add (this value)
  (:documentation "XXX.prototype.add(value). Candidate: Set, WeakSet"))

(defgeneric js-all (this iterable)
  (:documentation "XXX.all(iterable). Candidate: Promise"))

(defgeneric js-apply (this arg-array)
  (:documentation "XXX.prototype.apply function. Candidate: Function"))

(defgeneric js-apply (this target this-arg arglist)
  (:documentation "XXX.apply(target,thisArgument,argumentsList). Candidate:
Reflect"))

(defgeneric js-assign (this &rest sources)
  (:documentation "XXX.assign function. Candidate: Object"))

(defgeneric js-bind (this &rest args)
  (:documentation "XXX.prototype.bind function. Candidate: Function"))

(defgeneric js-buffer (this)
  (:documentation "get XXX.prototype.buffer. Candidate: TypedArray, DataView"))

(defgeneric js-byte-length (this)
  (:documentation "get XXX.prototype.byteLength. Candidate: TypedArray,
ArrayBuffer, DataView"))

(defgeneric js-byte-offset (this)
  (:documentation "get XXX.prototype.byteOffset. Candidate: TypedArray,
DataView"))

(defgeneric js-call (func &rest args)
  (:documentation "XXX.prototype.call function. Candidate: Function"))

(defgeneric js-catch (this reject)
  (:documentation "XXX.prototype.catch(onRejected). Candidate: Promise"))

(defgeneric js-char-at (target pos)
  (:documentation "XXX.prototype.charAt function. Candidate: String"))

(defgeneric js-char-code-at (target pos)
  (:documentation "XXX.prototype.charCodeAt function. Candidate: String"))

(defgeneric js-clear (this)
  (:documentation "XXX.prototype.clear(). Candidate: Map, Set"))

(defgeneric js-code-point-at (target pos)
  (:documentation "XXX.prototype.codePointAt function. Candidate: String"))

(defgeneric js-concat (this &rest args)
  (:documentation "XXX.prototype.concat(...arguments). Candidate: String,
Array"))

(defgeneric js-construct (this target arglist &optional new-target)
  (:documentation "XXX.construct(target,argumentsList[,newTarget]). Candidate:
Reflect"))

(defgeneric js-copy-within (this target start &optional end)
  (:documentation "XXX.prototype.copyWithin(target,start[,end]). Candidate:
Array, TypedArray"))

(defgeneric js-create (target proto)
  (:documentation "XXX.create function. Candidate: Object"))

(defgeneric js-define-properties (obj properties)
  (:documentation "XXX.defineProperties function. Candidate: Object"))

(defgeneric js-define-property (obj property attributes)
  (:documentation "XXX.defineProperty function. Candidate: Object"))

;;; key and value.
(defgeneric js-delete (this key)
  (:documentation "XXX.prototype.delete(key). Candidate: Map, Set, WeakMap,
WeakSet"))

(defgeneric js-ends-with (target search)
  (:documentation "XXX.prototype.endsWith function. Candidate: String"))

(defgeneric js-entries (this)
  (:documentation "XXX.prototype.entries(). Candidate: Array, TypedArray,
Map, Set"))

(defgeneric js-every (this callback &optional arg)
  (:documentation "XXX.prototype.every(callbackfn[,thisArg]). Candidate: Array,
TypedArray"))

(defgeneric js-exec (target string)
  (:documentation "XXX.prototype.exec(string). Candidate: RegExp"))

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

(defgeneric js-flags (this)
  (:documentation "get XXX.prototype.flags. Candidate: RegExp"))

(defgeneric js-for (this key)
  (:documentation "XXX.for function. Candidate: Symbol"))

(defgeneric js-for-each (this callback &optional arg)
  (:documentation "XXX.prototype.forEach(callbackfn[,thisArg]). Candidate:
Array, TypedArray, Map, Set"))

(defgeneric js-freeze (this)
  (:documentation "XXX.freeze function. Candidate: Object"))

(defgeneric js-from (this items &optional map arg)
  (:documentation "XXX.from(items[,mapfn[,thisArg]]). Candidate: Array,
TypedArray"))

(defgeneric js-from-char-code (string &rest code-units)
  (:documentation "XXX.fromCharCode function. Candidate: String"))

(defgeneric js-from-code-point (string &rest code-points)
  (:documentation "XXX.fromCodePoint function. Candidate: String"))

(defgeneric js-get (this key)
  (:documentation "XXX.prototype.get(key). Candidate: Map, WeakMap"))

(defgeneric js-get-float32 (this offset &optional endian)
  (:documentation "XXX.prototype.getFloat32(byteOffset[,littleEndian]).
Candidate: DataView"))

(defgeneric js-get-float64 (this offset &optional endian)
  (:documentation "XXX.prototype.getFloat64(byteOffset[,littleEndian]).
Candidate: DataView"))

(defgeneric js-get-int8 (this offset)
  (:documentation "XXX.prototype.getInt8(byteOffset). Candidate: DataView"))

(defgeneric js-get-int16 (this offset &optional endian)
  (:documentation "XXX.prototype.getInt16(byteOffset[,littleEndian]).
Candidate: DataView"))

(defgeneric js-get-int32 (this offset &optional endian)
  (:documentation "XXX.prototype.getInt32(byteOffset[,littleEndian]).
Candidate: DataView"))

(defgeneric js-get-uint8 (this offset)
  (:documentation "XXX.prototype.getUInt8(byteOffset[,littleEndian]).
Candidate: DataView"))

(defgeneric js-get-uint16 (this offset &optional endian)
  (:documentation "XXX.prototype.getUInt16(byteOffset[,littleEndian]).
Candidate: DataView"))

(defgeneric js-get-uint32 (this offset &optional endian)
  (:documentation "XXX.prototype.getUInt32(byteOffset[,littleEndian]).
Candidate: DataView"))

(defgeneric js-get-own-property-descriptor (obj property)
  (:documentation "XXX.getOwnPropertyDescriptor function. Candidate: Object"))

(defgeneric js-get-own-property-names (obj)
  (:documentation "XXX.getOwnPropertyNames function. Candidate: Object"))

(defgeneric js-get-own-property-symbols (obj)
  (:documentation "XXX.getOwnPropertySymbols function. Candidate: Object"))

(defgeneric js-get-prototype-of (obj)
  (:documentation "XXX.getPrototypeOf function. Candidate: Object"))

(defgeneric js-global (this)
  (:documentation "get XXX.prototype.global. Candidate: RegExp"))

;;; key or value
(defgeneric js-has (this key)
  (:documentation "XXX.prototype.has(key). Candidate: Map, Set, WeakMap,
WeakSet"))

(defgeneric js-has-own-property (obj value)
  (:documentation "XXX.prototype.hasOwnProperty function. Candidate: Object"))

(defgeneric js-ignore-case (target)
  (:documentation "get XXX.prototype.ignoreCase. Candidate: RegExp"))

(defgeneric js-includes (target search)
  (:documentation "XXX.prototype.includes function. Candidate: String"))

(defgeneric js-index-of (this search)
  (:documentation "XXX.prototype.indexOf function. Candidate: String"))

(defgeneric js-index-of (this search &optional from)
  (:documentation "XXX.prototype.indexOf(searchElement[,fromIndex]). Candidate:
Array, TypedArray"))

(defgeneric js-is (value1 value2)
  (:documentation "XXX.is function. Candidate: Object"))

(defgeneric js-is-array (target)
  (:documentation "XXX.isArray(arg). Candidate: Array"))

(defgeneric js-is-extensible (obj)
  (:documentation "XXX.isExtensible function. Candidate: Object"))

(defgeneric js-is-frozen (obj)
  (:documentation "XXX.isFrozen function. Candidate: Object"))

(defgeneric js-is-prototype-of (obj value)
  (:documentation "XXX.prototype.isPrototypeOf function. Candidate: Object"))

(defgeneric js-is-sealed (this)
  (:documentation "XXX.isSealed function. Candidate: Object"))

(defgeneric js-join (this separator)
  (:documentation "XXX.prototype.join(separator). Candidate: Array,
TypedArray"))

(defgeneric js-key-for (symbol)
  (:documentation "XXX.keyFor function. Candidate: Symbol"))

(defgeneric js-keys (this)
  (:documentation "XXX.keys(). Candidate: Object or XXX.prototype.keys().
Candidate: Array, TypedArray, Map, Set"))

(defgeneric js-last-index-of (this search)
  (:documentation "XXX.prototype.lastIndexOf(searchElement[,fromIndex]).
Candidate: String"))

(defgeneric js-last-index-of (this search &optional from)
  (:documentation "XXX.prototype.lastIndexOf(searchElement[,fromIndex]).
Candidate: Array, TypedArray"))

(defgeneric js-length (this)
  (:documentation "get XXX.prototype.length. Candidate: Function, TypedArray"))

(defgeneric js-locale-compare (this that)
  (:documentation "XXX.prototype.localeCompare function. Candidate: String"))

(defgeneric js-map (this callback &optional arg)
  (:documentation "XXX.prototype.map(callbackfn[,thisArg]. Candidate: Array,
TypedArray"))

(defgeneric js-match (target regexp)
  (:documentation "XXX.prototype.match function. Candidate: String"))

(defgeneric js-message (error-name)
  (:documentation "XXX.prototype.message function. Candidate: Error"))

(defgeneric js-multiline (target)
  (:documentation "get XXX.prototype.multiline. Candidate: RegExp"))

(defgeneric js-name (error-name)
  (:documentation "XXX.prototype.name function. Candidate: Error"))

(defgeneric js-normalize (target &optional form)
  (:documentation "XXX.prototype.normalize function. Candidate: String"))

(defgeneric js-now (this)
  (:documentation "XXX.now function. Candidate: Date"))

(defgeneric js-of (this &rest items)
  (:documentation "XXX.of(...items). Candidate: Array, TypedArray"))

(defgeneric js-parse (this text &optional reviver)
  (:documentation "XXX.parse(text[,reviver]). Candidate: JSON"))

(defgeneric js-pop (this)
  (:documentation "XXX.prototype.pop(). Candidate: Array"))

(defgeneric js-prevent-extensions (this)
  (:documentation "XXX.preventExtensions. Candidate: Object"))

(defgeneric js-property-is-enumerable (this value)
  (:documentation "XXX.prototype.propertyIsEnumerable function. Candidate: Object"))

(defgeneric js-push (this &rest items)
  (:documentation "XXX.prototype.push(...items). Candidate: Array"))

(defgeneric js-parse (date string)
  (:documentation "XXX.parse function. Candidate: Date"))

(defgeneric js-race (this iterable)
  (:documentation "XXX.race(iterable). Candidate: Promise"))

(defgeneric js-raw (string template &rest substitutions)
  (:documentation "XXX.raw function. Candidate: String"))

(defgeneric js-reduce (this callback &optional initial)
  (:documentation "XXX.prototype.reduce(callbackfn[,initialValue]). Candidate:
Array, TypedArray"))

(defgeneric js-reduce-right (this callback &optional initial)
  (:documentation "XXX.prototype.reduceRight(callbackfn[,initialValue]).
Candidate: Array, TypedArray"))

(defgeneric js-reject (r)
  (:documentation "XXX.reject(r). Candidate: Promise"))

(defgeneric js-repeat (target count)
  (:documentation "XXX.prototype.repeat function. Candidate: String"))

(defgeneric js-replace (target search replace)
  (:documentation "XXX.prototype.replace function. Candidate: String"))

(defgeneric js-resolve (x)
  (:documentation "XXX.resolve(x). Candidate: Promise"))

(defgeneric js-reverse (this)
  (:documentation "XXX.prototype.reverse(). Candidate: Array, TypedArray"))

(defgeneric js-seal (obj)
  (:documentation "XXX.seal function. Candidate: Object"))

(defgeneric js-search (string regexp)
  (:documentation "XXX.prototype.search function. Candidate: String"))

;;; Three poly, overloaded, array and typedArray.
;;; For now we don't remove duplicated generic function.
(defgeneric js-set (this array &optional offset)
  (:documentation "XXX.prototype.set(array[,offset]). Candidate:
TypedArray"))

(defgeneric js-set (this key value)
  (:documentation "XXX.prototype.set(key,value). Candidate: Map, WeakMap"))

(defgeneric js-set-float32 (this offset value &optional endian)
  (:documentation "XXX.prototype.setFloat32(byteOffset,value[,littleEndian]).
Candidate: DataView"))

(defgeneric js-set-float64 (this offset value &optional endian)
  (:documentation "XXX.prototype.setFloat64(byteOffset,value[,littleEndian]).
Candidate: DataView"))

(defgeneric js-set-int8 (this offset value)
  (:documentation "XXX.prototype.setInt8(byteOffset,value). Candidate:
DataView"))

(defgeneric js-set-int16 (this offset value &optional endian)
  (:documentation "XXX.prototype.setInt16(byteOffset,value[,littleEndian]).
Candidate: DataView"))

(defgeneric js-set-int32 (this offset value &optional endian)
  (:documentation "XXX.prototype.setInt32(byteOffset,value[,littleEndian]).
Candidate: DataView"))

(defgeneric js-set-uint8 (this offset value)
  (:documentation "XXX.prototype.setUInt8(byteOffset,value). Candidate:
DataView"))

(defgeneric js-set-uint16 (this offset value &optional endian)
  (:documentation "XXX.prototype.setUInt16(byteOffset,value[,littleEndian]).
Candidate: DataView"))

(defgeneric js-set-uint32 (this offset value &optional endian)
  (:documentation "XXX.prototype.setUInt32(byteOffset,value[,littleEndian]).
Candidate: DataView"))

(defgeneric js-shift (this)
  (:documentation "XXX.prototype.shift(). Candidate: Array"))

(defgeneric js-set-prototype-of (obj proto)
  (:documentation "XXX.setPrototypeOf function. Candidate: Object"))

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

(defgeneric js-source (this)
  (:documentation "get XXX.prototype.source. Candidate: RegExp"))

(defgeneric js-splice (this start delete &rest items)
  (:documentation "XXX.prototype.splice(start,deleteCount,...items). Candidate:
Array"))

(defgeneric js-split (target separator limit)
  (:documentation "XXX.prototype.split function. Candidate: String"))

(defgeneric js-starts-with (target search &optional pos)
  (:documentation "XXX.prototype.startsWith function. Candidate: String"))

(defgeneric js-sticky (target)
  (:documentation "get XXX.prototype.sticky. Candidate: RegExp"))

(defgeneric js-stringify (this value &optional replacer space)
  (:documentation "XXX.stringify(value[,replacer[,space]]). Candidate: JSON"))

(defgeneric js-subarray (this &optional begin end)
  (:documentation "XXX.prototype.subarray. Candidate: TypedArray"))

(defgeneric js-substring (target start end)
  (:documentation "XXX.prototype.substring function. Candidate: String"))

(defgeneric js-test (target s)
  (:documentation "XXX.prototype.test(s). Candidate: RegExp"))

(defgeneric js-then (this fulfilled rejected)
  (:documentation "XXX.prototype.then(onFulfilled,onRejected). Candidate:
Promise"))

(defgeneric js-to-exponential (num fraction-digits)
  (:documentation "XXX.prototype.toExponential function. Candidate: Number"))

(defgeneric js-to-fixed (num fraction-digits)
  (:documentation "XXX.prototype.toFixed function. Candidate: Number"))

(defgeneric js-to-json (date key)
  (:documentation "XXX.prototype.toJSON function. Candidate: Date"))

(defgeneric js-to-locale-lower-case (target)
  (:documentation "XXX.prototype.toLocaleLowerCase function. Candidate: String"))

(defgeneric js-to-locale-upper-case (target)
  (:documentation "XXX.prototype.toLocaleUpperCase function. Candidate: String"))

(defgeneric js-to-locale-string (this &optional reserved1 reserved2)
  (:documentation "XXX.prototype.toLocaleString([reserved1[,reserved2]]).
Candidate: Object, Number, Date, Array, TypedArray"))

(defgeneric js-to-lower-case (target)
  (:documentation "XXX.prototype.toLowerCase(). Candidate: String"))

(defgeneric js-to-upper-case (target)
  (:documentation "XXX.prototype.toUpperCase(). Candidate: String"))

(defgeneric js-to-precision (num precision)
  (:documentation "XXX.prototype.toPrecision function. Candidate: Number"))

;;; Number.prototype.toString(radix).
(defgeneric js-to-string (this)
  (:documentation "XXX.prototype.toString(). Candidate: Object, Function,
Boolean, Symbol, Error, Number, Date, String, RegExp, Array, TypedArray"))

(defgeneric js-trim (target)
  (:documentation "XXX.prototype.trim(). Candidate: String"))

(defgeneric js-unicode (target)
  (:documentation "get XXX.prototype.unicode. Candidate: RegExp"))

(defgeneric js-unshift (this &rest items)
  (:documentation "XXX.prototype.unshift(...items). Candidate: Array"))

(defgeneric js-utc (date year month &rest args)
  (:documentation "XXX.UTC function. Candidate: Date"))

(defgeneric js-values (this)
  (:documentation "XXX.prototype.values(). Candidate: Array, TypedArray, Map,
Set"))

(defgeneric js-value-of (this)
  (:documentation "XXX.prototype.valueOf(). Candidate: Object, Boolean,
Symbol, Number, Date, String"))

