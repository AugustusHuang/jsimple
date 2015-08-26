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

;;;; NODE definition, extensions and related functions.
;;;; NODE is the minimal element of IR.
(in-package :jsimple-ir)

(defparameter +js-type+
  '(member :js-int32 :js-double :js-null :js-boolean :js-array :js-string
    :js-object :js-undefined))

;;; FIXME: Shall we move this to piece module?
;;; CONTROL TRANSFER structure directly lended from SBCL.
(defstruct ctran
  ;; an indication of the way that this continuation is currently used.
  ;; :UNUSED
  ;;    A continuation for which all control-related slots have the
  ;;    default values. A continuation is unused during IR1 conversion
  ;;    until it is assigned a block, and may be also be temporarily
  ;;    unused during later manipulations of IR1. In a consistent
  ;;    state there should never be any mention of :UNUSED
  ;;    continuations. NEXT can have a non-null value if the next node
  ;;    has already been determined.
  ;; :BLOCK-START
  ;;    The continuation that is the START of BLOCK.
  ;; :INSIDE-BLOCK
  ;;    A continuation that is the NEXT of some node in BLOCK.
  (kind :unused :type (member :unused :inside-block :block-start))
  ;; A NODE which is to be evaluated next. Null only temporary.
  (next nil :type (or node null))
  ;; the node where this CTRAN is used, if unique. This is always null
  ;; in :UNUSED and :BLOCK-START CTRANs, and is never null in
  ;; :INSIDE-BLOCK continuations.
  (use nil :type (or node null))
  ;; the basic block this continuation is in. This is null only in
  ;; :UNUSED continuations.
  (block nil :type (or cblock null)))

(defstruct lvar
  ;; The node which receives this value. NIL only temporarily.
  (dest nil :type (or node null))
  ;; cached type of this lvar's value. Default will be UNDEFINED.
  (derived-type :undefined :type +js-type+)
  ;; the node (if unique) or a list of nodes where this lvar is used.
  (uses nil :type (or node list))
  ;; set to true when something about this lvar's value has changed.
  (reoptimize t :type boolean)
  ;; Cached type which is checked by DEST. If NIL, then this must be
  ;; recomputed: see LVAR-EXTERNALLY-CHECKABLE-TYPE.
  (externally-checkable-type :undefined :type +js-type+))

(defstruct node
  ;; True if this node needs to be optimized. This is set to true
  ;; whenever something changes about the value of an lvar whose DEST
  ;; is this node.
  (reoptimize t :type boolean)
  ;; the ctran indicating what we do controlwise after evaluating this
  ;; node. This is null if the node is the last in its block.
  (next nil :type (or ctran null))
  ;; the ctran that this node is the NEXT of. This is null during IR1
  ;; conversion when we haven't linked the node in yet or in nodes
  ;; that have been deleted from the IR1 by UNLINK-NODE.
  (prev nil :type (or ctran null))
  (tree-path nil :type list)
  ;; NOTE: ECMAscript force tail-recursive, be sensitive!
  ;; If this node is in a tail-recursive position, then this is set to
  ;; T. At the end of IR1 (in physical environment analysis) this is
  ;; computed for all nodes (after cleanup code has been emitted).
  ;; Before then, a non-null value indicates that IR1 optimization has
  ;; converted a tail local call to a direct transfer.
  ;; If the back-end breaks tail-recursion for some reason, then it
  ;; can null out this slot.
  (tail-p nil :type boolean))

(defstruct (leaf-node
	     (:include node))
  )

(defstruct (cond-node
	     (:include node))
  )

(defstruct (funcall-node
	     (:include node))
  )

(defstruct (var-node
	     (:include leaf-node))
  )

(defstruct (func-node
	     (:include leaf-node))
  )

;;; Et cetera...
