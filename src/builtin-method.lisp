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

;;; Prototype objects property functions.
(in-package :lesp-builtin)

(setf !has-own-property
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "hasOwnProperty")
		     :length (make-property :value 1))
      !is-prototype-of
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "isPrototypeOf")
		     :length (make-property :value 1))
      !property-is-enumerable
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "propertyIsEnumerable")
		     :length (make-property :value 1))
      !to-locale-string
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "toLocaleString")
		     :length (make-property :value 0))
      !to-string
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "toString")
		     :length (make-property :value 0))
      !value-of
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "valueOf")
		     :length (make-property :value 0))
      !apply
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "apply")
		     :length (make-property :value 2))
      !bind
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "bind")
		     :length (make-property :value 1))
      !call
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "call")
		     :length (make-property :value 1))
      !has-instance
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "[Symbol.hasInstance]")
		     :length (make-property :value 1))
      !to-primitive
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "[Symbol.toPrimitive]")
		     :length (make-property :value 1))
      !to-exponential
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "toExponential")
		     :length (make-property :value 1))
      !to-fixed
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "toFixed")
		     :length (make-property :value 1))
      !to-precision
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "toPrecision")
		     :length (make-property :value 1))
      !get-date
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "getDate")
		     :length (make-property :value 0))
      !get-day
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "getDay")
		     :length (make-property :value 0))
      !get-full-year
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "getFullYear")
		     :length (make-property :value 0))
      !get-hours
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "getHours")
		     :length (make-property :value 0))
      !get-milliseconds
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "getMilliseconds")
		     :length (make-property :value 0))
      !get-minutes
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "getMinutes")
		     :length (make-property :value 0))
      !get-month
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "getMonth")
		     :length (make-property :value 0))
      !get-seconds
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "getSeconds")
		     :length (make-property :value 0))
      !get-time
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "getTime")
		     :length (make-property :value 0))
      !get-timezone-offset
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "getTimezoneOffset")
		     :length (make-property :value 0))
      !get-utc-date
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "getUTCDate")
		     :length (make-property :value 0))
      !get-utc-day
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "getUTCDay")
		     :length (make-property :value 0))
      !get-utc-full-year
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "getUTCFullYear")
		     :length (make-property :value 0))
      !get-utc-hours
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "getUTCHours")
		     :length (make-property :value 0))
      !get-utc-milliseconds
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "getUTCMilliseconds")
		     :length (make-property :value 0))
      !get-utc-minutes
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "getUTCMinutes")
		     :length (make-property :value 0))
      !get-utc-month
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "getUTCMonth")
		     :length (make-property :value 0))
      !get-utc-seconds
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "getUTCSeconds")
		     :length (make-property :value 0))
      !set-date
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "setDate")
		     :length (make-property :value 1))
      !set-full-year
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "setFullYear")
		     :length (make-property :value 3))
      !set-hours
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "setHours")
		     :length (make-property :value 4))
      !set-milliseconds
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "setMilliseconds")
		     :length (make-property :value 1))
      !set-minutes
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "setMinutes")
		     :length (make-property :value 3))
      !set-month
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "setMonth")
		     :length (make-property :value 2))
      !set-seconds
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "setSeconds")
		     :length (make-property :value 2))
      !set-time
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "setTime")
		     :length (make-property :value 1))
      !set-utc-date
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "setUTCDate")
		     :length (make-property :value 1))
      !set-utc-full-year
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "setUTCFullYear")
		     :length (make-property :value 3))
      !set-utc-hours
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "setUTCHours")
		     :length (make-property :value 4))
      !set-utc-milliseconds
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "setUTCMilliseconds")
		     :length (make-property :value 1))
      !set-utc-minutes
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "setUTCMinutes")
		     :length (make-property :value 3))
      !set-utc-month
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "setUTCMonth")
		     :length (make-property :value 2))
      !set-utc-seconds
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "setUTCSeconds")
		     :length (make-property :value 2))
      !to-date-string
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "toDateString")
		     :length (make-property :value 0))
      !to-isos-string
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "toISOString")
		     :length (make-property :value 0))
      !to-json
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "toJSON")
		     :length (make-property :value 0))
      !to-locale-date-string
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "toLocaleDateString")
		     :length (make-property :value 0))
      !to-locale-time-string
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "toLocaleTimeString")
		     :length (make-property :value 0))
      !to-time-string
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "toTimeString")
		     :length (make-property :value 0))
      !to-utc-string
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "toUTCString")
		     :length (make-property :value 0))
      !char-at
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "charAt")
		     :length (make-property :value 1))
      !char-code-at
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "charCodeAt")
		     :length (make-property :value 1))
      !code-point-at
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "codePointAt")
		     :length (make-property :value 1))
      !concat
      (make-instance '-function-proto
		     :-extensible nil
		     :name (make-property :value "concat")
		     :length (make-property :value 1))
      !ends-with
      !includes
      !index-of
      !last-index-of
      !locale-compare
      !match
      !normalize
      !repeat
      !replace
      !search
      !slice
      !split
      !starts-with
      !substring
      !to-locale-lower-case
      !to-locale-upper-case
      !to-lower-case
      !to-upper-case
      !trim
      !iterator
      !next
      !exec
      !test
      !copy-within
      !entries
      !every
      !fill
      !filter
      !find
      !find-index
      !for-each
      !join
      !map
      !pop
      !push
      !reduce
      !reduce-right
      !reverse
      !shift
      !some
      !sort
      !unshift
      !values
      !set
      !subarray
      !clear
      !delete
      !get
      !has
      !add
      !get-float32
      !get-float64
      !get-int8
      !get-int16
      !get-int32
      !get-uint8
      !get-uint16
      !get-uint32
      !set-float32
      !set-float64
      !set-int8
      !set-int16
      !set-int32
      !set-uint8
      !set-uint16
      !set-uint32
      !return
      !throw
      !catch
      !then
      )
