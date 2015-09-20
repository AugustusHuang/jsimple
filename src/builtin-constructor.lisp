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

;;; Global builtin constructor objects.
(in-package :lesp-builtin)

(setf !object
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Object")
		     :length (make-property :value 1 :configurable :true)
		     :prototype (make-property :value (find-class '-object-proto))
		     :properties
		     '((assign . (make-property :value !assign))
		       (create . (make-property :value !create))
		       (define-properties . (make-property :value !define-properties))
		       (define-property . (make-property :value !define-property))
		       (freeze . (make-property :value !freeze))
		       (get-own-property-descriptor . (make-property :value !get-own-property-descriptor))
		       (get-own-property-names . (make-property :value !get-own-property-names))
		       (get-own-property-symbols . (make-property :value !get-own-property-symbols))
		       (get-prototype-of . (make-property :value !get-prototype-of))
		       (is . (make-property :value !is))
		       (is-extensible . (make-property :value !is-extensible))
		       (is-frozen . (make-property :value !is-frozen))
		       (is-sealed . (make-property :value !is-sealed))
		       (keys . (make-property :value !keys))
		       (prevent-extensions . (make-property :value !prevent-extensions))
		       (seal . (make-property :value !seal))
		       (set-prototype-of . (make-property :value !set-prototype-of))))
      !function
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Function")
		     :length (make-property :value 1 :configurable :true)
		     :prototype (make-property :value (find-class '-function-proto)))
      !boolean
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Boolean")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-boolean-proto)))
      !number
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Number")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class 'number-proto)))
      !string
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "String")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class 'string-proto)))
      !symbol
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Symbol")
		     :length (make-property :value 0)
		     :prototype (make-property :value (find-class '-symbol-proto)))
      !error
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Error")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-error-proto)))
      !eval-error
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "EvalError")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-eval-error-proto)))
      !range-error
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "RangeError")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-eval-error-proto)))
      !reference-error
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "ReferenceError")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-reference-error-proto)))
      !syntax-error
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "SyntaxError")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-syntax-error-proto)))
      !type-error
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "TypeError")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-type-error-proto)))
      !uri-error
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "URIError")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-uri-error-proto)))
      !array
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Array")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-array-proto)))
      !float32-array
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Float32Array")
		     :length (make-property :value 3)
		     :prototype (make-property :value (find-class '-float32-array-proto)))
      !float64-array
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Float64Array")
		     :length (make-property :value 3)
		     :prototype (make-property :value (find-class '-float64-array-proto)))
      !int8-array
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Int8Array")
		     :length (make-property :value 3)
		     :prototype (make-property :value (find-class '-int8-array-proto)))
      !int16-array
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Int16Array")
		     :length (make-property :value 3)
		     :prototype (make-property :value (find-class '-int16-array-proto)))
      !int32-array
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Int32Array")
		     :length (make-property :value 3)
		     :prototype (make-property :value (find-class '-int32-array-proto)))
      !uint8-array
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Uint8Array")
		     :length (make-property :value 3)
		     :prototype (make-property :value (find-class '-uint8-array-proto)))
      !uint8-clamped-array
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Uint8ClampedArray")
		     :length (make-property :value 3)
		     :prototype (make-property :value (find-class '-uint8-clamped-array-proto)))
      !uint16-array
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Uint16Array")
		     :length (make-property :value 3)
		     :prototype (make-property :value (find-class '-uint16-array-proto)))
      !uint32-array
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Uint32Array")
		     :length (make-property :value 3)
		     :prototype (make-property :value (find-class '-uint32-array-proto)))
      !date-0
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Date-0")
		     :length (make-property :value 7)
		     :prototype (make-property :value (find-class '-date-proto)))
      !date-1
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Date-1")
		     :length (make-property :value 7)
		     :prototype (make-property :value (find-class '-date-proto)))
      !reg-exp
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "RegExp")
		     :length (make-property :value 2)
		     :prototype (make-property :value (find-class '-reg-exp-proto)))
      !map
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Map")
		     :length (make-property :value 0)
		     :prototype (make-property :value (find-class '-map-proto)))
      !set
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Set")
		     :length (make-property :value 0)
		     :prototype (make-property :value (find-class '-set-proto)))
      !weak-map
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "WeakMap")
		     :length (make-property :value 0)
		     :prototype (make-property :value (find-class '-weak-map-proto)))
      !weak-set
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "WeakSet")
		     :length (make-property :value 0)
		     :prototype (make-property :value (find-class '-weak-set-proto)))
      !array-buffer
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "ArrayBuffer")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-array-buffer-proto)))
      !data-view
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "DataView")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-data-view-proto)))
      !promise
      (make-instance '-function-proto
		     :-prototype (find-class '-function-proto)
		     :name (make-property :value "Promise")
		     :length (make-property :value 1)
		     :prototype (make-property :value (find-class '-promise-proto))))
