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
  (block-node nil :type (or block-node null)))

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

;;; NODE will be the generic interface of all different type of nodes,
;;; normally it won't be created, only its concrete form will do.
(defstruct node
  ;; True if this node needs to be optimized. This is set to true
  ;; whenever something changes about the value of an lvar whose DEST
  ;; is this node.
  (reoptimize t :type boolean)
  ;; The ctran indicating what we do controlwise after evaluating this
  ;; node. This is null if the node is the last in its block.
  (next nil :type (or ctran null))
  ;; The ctran that this node is the NEXT of. This is null during IR1
  ;; conversion when we haven't linked the node in yet or in nodes
  ;; that have been deleted from the IR1 by UNLINK-NODE.
  (prev nil :type (or ctran null))
  ;; The path to the original statement in the AST.
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

;;; XXX: Do we need a special print function?
;;; LEAF-NODE is created when facing a leaf object, like 
(defstruct (leaf-node
	     (:include node))
  )

(defstruct (value-node
	     (:include node))
  (derived-type :undefined :type +js-type+)
  (lvar nil :type (or lvar null)))

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

(defstruct (block-node
	     (:include node))
  ;; a list of all the blocks that are predecessors/successors of this
  ;; block. In well-formed IR1, most blocks will have one successor.
  ;; The only exceptions are:
  ;;  1. component head blocks (any number)
  ;;  2. blocks ending in an IF (1 or 2)
  ;;  3. blocks with DELETE-P set (zero)
  (pred nil :type list)
  (succ nil :type list)
  ;; the ctran which heads this block (a :BLOCK-START), or NIL when we
  ;; haven't made the start ctran yet (and in the dummy component head
  ;; and tail blocks)
  (start nil :type (or ctran null))
  ;; the last node in this block. This is NIL when we are in the
  ;; process of building a block (and in the dummy component head and
  ;; tail blocks.)
  (last nil :type (or node null))
  ;; the forward and backward links in the depth-first ordering of the
  ;; blocks. These slots are NIL at beginning/end.
  (next nil :type (or null block-node))
  (prev nil :type (or null block-node))
  ;; in constraint propagation: list of LAMBDA-VARs killed in this block
  ;; in copy propagation: list of killed TNs
  (kill nil)
  ;; other sets used in constraint propagation and/or copy propagation
  (gen nil)
  (in nil)
  (out nil)
  ;; the LOOP that this block belongs to
  (loop nil :type (or null loop-node))
  ;; next block in the loop.
  (loop-next nil :type (or null block-node))
  ;; the component this block is in, or NIL temporarily during IR1
  ;; conversion and in deleted blocks
  (component (progn
               (aver-live-component *current-component*)
               *current-component*)
             :type (or component null))
  ;; a flag used by various graph-walking code to determine whether
  ;; this block has been processed already or what. We make this
  ;; initially NIL so that FIND-INITIAL-DFO doesn't have to scan the
  ;; entire initial component just to clear the flags.
  (flag nil)
  ;; some kind of info used by the back end
  (info nil)
  ;; what macroexpansions and source transforms happened "in" this block, used
  ;; for xref
  (xrefs nil :type list))

(defstruct (loop-node
	      (:include node))
  ;; The kind of loop that this is.  These values are legal:
  ;;    NIL
  ;;        Temporary.
  ;;    :OUTER
  ;;        This is the outermost loop structure, and represents all the
  ;;        code in a component.
  ;;    :NATURAL
  ;;        A normal loop with only one entry.
  ;;    :STRANGE
  ;;        A segment of a "strange loop" in a non-reducible flow graph.
  (kind nil :type (member nil :outer :natural :strange) null)
  ;; The first and last blocks in the loop.  There may be more than one tail,
  ;; since there may be multiple back branches to the same head.
  (head nil :type (or block-node null))
  (tail nil :type list)
  ;; A list of all the blocks in this loop or its inferiors that have a
  ;; successor outside of the loop.
  (exits nil :type list)
  ;; The loop that this loop is nested within.  This is null in the outermost
  ;; loop structure.
  (superior nil :type (or loop-node null))
  ;; A list of the loops nested directly within this one.
  (inferiors nil :type list)
  (depth 0 :type fixnum)
  ;; The head of the list of blocks directly within this loop.  We must recurse
  ;; on INFERIORS to find all the blocks.
  (blocks nil :type (or null block-node)))

(defstruct (bind-node
	     (:include node))
  )

;;; A COMPONENT structure provides a handle on a connected piece of
;;; the flow graph. Most of the passes in the compiler operate on
;;; COMPONENTs rather than on the entire flow graph.
;;;
;;; According to the CMU CL internals/front.tex, the reason for
;;; separating compilation into COMPONENTs is
;;;   to increase the efficiency of large block compilations. In
;;;   addition to improving locality of reference and reducing the
;;;   size of flow analysis problems, this allows back-end data
;;;   structures to be reclaimed after the compilation of each
;;;   component.
(defstruct (component (:constructor
		       make-component
		       (head
			tail &aux
			(last-block tail)
			(outer-loop (make-loop :kind :outer :head head)))))
  ;; the kind of component
  ;;
  ;; (The terminology here is left over from before
  ;; sbcl-0.pre7.34.flaky5.2, when there was no such thing as
  ;; FUNCTIONAL-HAS-EXTERNAL-REFERENCES-P, so that Python was
  ;; incapable of building standalone :EXTERNAL functions, but instead
  ;; had to implement things like #'CL:COMPILE as FUNCALL of a little
  ;; toplevel stub whose sole purpose was to return an :EXTERNAL
  ;; function.)
  ;;
  ;; The possibilities are:
  ;;   NIL
  ;;     an ordinary component, containing non-top-level code
  ;;   :TOPLEVEL
  ;;     a component containing only load-time code
  ;;   :COMPLEX-TOPLEVEL
  ;;     In the old system, before FUNCTIONAL-HAS-EXTERNAL-REFERENCES-P
  ;;     was defined, this was necessarily a component containing both
  ;;     top level and run-time code. Now this state is also used for
  ;;     a component with HAS-EXTERNAL-REFERENCES-P functionals in it.
  ;;   :INITIAL
  ;;     the result of initial IR1 conversion, on which component
  ;;     analysis has not been done
  ;;   :DELETED
  ;;     debris left over from component analysis
  ;;
  ;; See also COMPONENT-TOPLEVELISH-P.
  (kind nil :type (member nil :toplevel :complex-toplevel :initial :deleted))
  ;; the blocks that are the dummy head and tail of the DFO
  ;;
  ;; Entry/exit points have these blocks as their
  ;; predecessors/successors. The start and return from each
  ;; non-deleted function is linked to the component head and
  ;; tail. Until physical environment analysis links NLX entry stubs
  ;; to the component head, every successor of the head is a function
  ;; start (i.e. begins with a BIND node.)
  (head nil :type (or block-node null))
  (tail nil :type (or block-node null))
  ;; New blocks are inserted before this.
  (last-block nil :type (or block-node null))
  ;; This becomes a list of the CLAMBDA structures for all functions
  ;; in this component. OPTIONAL-DISPATCHes are represented only by
  ;; their XEP and other associated lambdas. This doesn't contain any
  ;; deleted or LET lambdas.
  ;;
  ;; Note that logical associations between CLAMBDAs and COMPONENTs
  ;; seem to exist for a while before this is initialized. See e.g.
  ;; the NEW-FUNCTIONALS slot. In particular, I got burned by writing
  ;; some code to use this value to decide which components need
  ;; LOCALL-ANALYZE-COMPONENT, when it turns out that
  ;; LOCALL-ANALYZE-COMPONENT had a role in initializing this value
  ;; (and DFO stuff does too, maybe). Also, even after it's
  ;; initialized, it might change as CLAMBDAs are deleted or merged.
  ;; -- WHN 2001-09-30
  (lambdas () :type list)
  ;; a list of FUNCTIONALs for functions that are newly converted, and
  ;; haven't been local-call analyzed yet. Initially functions are not
  ;; in the LAMBDAS list. Local call analysis moves them there
  ;; (possibly as LETs, or implicitly as XEPs if an OPTIONAL-DISPATCH.)
  ;; Between runs of local call analysis there may be some debris of
  ;; converted or even deleted functions in this list.
  (new-functionals () :type list)
  ;; If this is :MAYBE, then there is stuff in this component that
  ;; could benefit from further IR1 optimization. T means that
  ;; reoptimization is necessary.
  (reoptimize t :type (member nil :maybe t))
  ;; If this is true, then the control flow in this component was
  ;; messed up by IR1 optimizations, so the DFO should be recomputed.
  (reanalyze nil :type boolean)
  ;; count of the number of inline expansions we have done while
  ;; compiling this component, to detect infinite or exponential
  ;; blowups
  (inline-expansions 0 :type index)
  ;; a map from combination nodes to things describing how an
  ;; optimization of the node failed. The description is an alist
  ;; (TRANSFORM . ARGS), where TRANSFORM is the structure describing
  ;; the transform that failed, and ARGS is either a list of format
  ;; arguments for the note, or the FUN-TYPE that would have
  ;; enabled the transformation but failed to match.
  (failed-optimizations (make-hash-table :test 'eq) :type hash-table)
  ;; This is similar to NEW-FUNCTIONALS, but is used when a function
  ;; has already been analyzed, but new references have been added by
  ;; inline expansion. Unlike NEW-FUNCTIONALS, this is not disjoint
  ;; from COMPONENT-LAMBDAS.
  (reanalyze-functionals nil :type list)
  (delete-blocks nil :type list)
  (nlx-info-generated-p nil :type boolean)
  ;; this is filled by physical environment analysis
  (dx-lvars nil :type list)
  ;; The default LOOP in the component.
  (outer-loop (missing-arg) :type loop-node))

;;; XXX: Where is AVER?
;;; Check that COMPONENT is suitable for roles which involve adding
;;; new code. (gotta love imperative programming with lotso in-place
;;; side effects...)
(defun aver-live-component (component)
  (aver (not (eql (component-info component) :dead))))

;;; Et cetera...
