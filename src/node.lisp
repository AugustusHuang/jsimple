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
;;; NOTE: CTRAN acts like a counter, and NODE is the road it walks upon.
;;; Every CTRAN can have its own way, but the road underlying will only
;;; change if some of the blocks can merge or with code elimination.
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
  ;; NOTE: If the LVAR is a direct computation with no explicit exit,
  ;; I suppose the DEST is the current node.
  (dest nil :type (or node null))
  ;; cached type of this lvar's value. Default will be UNDEFINED.
  ;; NOTE: When a LVAR is generated and determined, we can tell the type
  ;; somehow...
  (derived-type :undefined :type +js-type+)
  ;; the node (if unique) or a list of nodes where this lvar is used.
  (uses nil :type (or node list))
  ;; set to true when something about this lvar's value has changed.
  (reoptimize t :type boolean)
  ;; Cached type which is checked by DEST. If NIL, then this must be
  ;; recomputed.
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

;;;; LEAF structures

;;; Variables, constants and functions are all represented by LEAF
;;; structures. A reference to a LEAF is indicated by a REF node. This
;;; allows us to easily substitute one for the other without actually
;;; hacking the flow graph.
;;; NOTE: LEAF is the concrete (genuine) variables, and REF only acts like
;;; a pointer, so we don't need to change nodes, only change LEAF underlying
;;; REF nodes.
(defstruct leaf
  ;; FIXME: JS is different! There will be global and local variables,
  ;; constants, local functions, lambdas and global functions.
  ;;
  ;; the name of LEAF as it appears in the source, e.g. 'FOO or '(SETF
  ;; FOO) or 'N or '*Z*, or the special .ANONYMOUS. value if there's
  ;; no name for this thing in the source (as can happen for
  ;; FUNCTIONALs, e.g. for anonymous LAMBDAs or for functions for
  ;; top-level forms; and can also happen for anonymous constants) or
  ;; perhaps also if the match between the name and the thing is
  ;; skewed enough (e.g. for macro functions or method functions) that
  ;; we don't want to have that name affect compilation
  ;;
  ;; (We use .ANONYMOUS. here more or less the way we'd ordinarily use
  ;; NIL, but we're afraid to use NIL because it's a symbol which could
  ;; be the name of a leaf, if only the constant named NIL.)
  ;;
  ;; The value of this slot in can affect ordinary runtime behavior,
  ;; e.g. of special variables and known functions, not just debugging.
  (source-name nil
   ;; I guess we state the type this way to avoid calling
   ;; LEGAL-FUN-NAME-P unless absolutely necessary,
   ;; but this seems a bit of a premature optimization.
   :type (or null symbol (and cons (satisfies legal-fun-name-p)))
   :read-only t)
  ;; the type which values of this leaf must have
  (type :undefined :type +js-type+)
  ;; the type which values of this leaf have last been defined to have
  ;; (but maybe won't have in future, in case of redefinition)
  (defined-type :undefined :type +js-type+)
  ;; where the TYPE information came from (in order from strongest to weakest):
  ;;  :DECLARED, from a declaration.
  ;;  :DEFINED-HERE, from examination of the definition in the same file.
  ;;  :DEFINED, from examination of the definition elsewhere.
  ;;  :DEFINED-METHOD, implicit, piecemeal declarations from CLOS.
  ;;  :ASSUMED, from uses of the object.
  (where-from :assumed :type (member :declared :assumed :defined-here :defined :defined-method))
  ;; list of the REF nodes for this leaf
  (refs () :type list)
  ;; true if there was ever a REF or SET node for this leaf. This may
  ;; be true when REFS and SETS are null, since code can be deleted.
  (ever-used nil :type boolean)
  ;; is it declared dynamic-extent, or truly-dynamic-extent?
  ;; XXX: What is dynamic-extent counterpart in JS?
  (extent nil :type (member nil :maybe-dynamic :always-dynamic :indefinite)))

;;; The CONSTANT-LEAF structure is used to represent known constant values.
;;; Constant will be effectively made to evaluate to itself,
;;; so it will always be ANONYMOUS.
(defstruct (constant-leaf
	     (:constructor make-constant-leaf (value
					       &aux
					       ;; No, obey JS rule.
					       (type (type-of value))
					       (source-name '.anonymous.)
					       (where-from :defined)))
	     (:include leaf))
  ;; The value of the constant.
  (value nil)
  ;; Boxed TN for this constant, if any.
  ;; XXX: What's this?
  (boxed-tn nil :type (or null tn)))

;;; We default the WHERE-FROM and TYPE slots to :DEFINED and FUNCTION.
;;; We don't normally manipulate function types for defined functions,
;;; but if someone wants to know, an approximation is there.
(defstruct (functional-leaf
	     (:include leaf
		       (source-name '.anonymous.)
		       (where-from :defined)
		       ;; No, obey JS rule.
		       (type (specifier-type 'function))))
  ;; the name of FUNCTIONAL for debugging purposes, or NIL if we
  ;; should just let the SOURCE-NAME fall through
  ;;
  ;; Unlike the SOURCE-NAME slot, this slot's value should never
  ;; affect ordinary code behavior, only debugging/diagnostic behavior.
  ;;
  ;; E.g. for the function which implements (DEFUN FOO ...), we could
  ;; have
  ;;   SOURCE-NAME=FOO
  ;;   DEBUG-NAME=NIL
  ;; for the function which implements the top level form
  ;; (IN-PACKAGE :FOO) we could have
  ;;   SOURCE-NAME=NIL
  ;;   DEBUG-NAME=(TOP-LEVEL-FORM (IN-PACKAGE :FOO)
  ;; for the function which implements FOO in
  ;;   (DEFUN BAR (...) (FLET ((FOO (...) ...)) ...))
  ;; we could have
  ;;   SOURCE-NAME=FOO
  ;;   DEBUG-NAME=(FLET FOO)
  ;; and for the function which implements FOO in
  ;;   (DEFMACRO FOO (...) ...)
  ;; we could have
  ;;   SOURCE-NAME=FOO (or maybe .ANONYMOUS.?)
  ;;   DEBUG-NAME=(MACRO-FUNCTION FOO)
  (debug-name nil
	      :type (or null (not (satisfies legal-fun-name-p)))
	      :read-only t)
  ;; some information about how this function is used. These values
  ;; are meaningful:
  ;;    NIL
  ;;    an ordinary function, callable using local call
  ;;    :LET
  ;;    a lambda that is used in only one local call, and has in
  ;;    effect been substituted directly inline. The return node is
  ;;    deleted, and the result is computed with the actual result
  ;;    lvar for the call.
  ;;    :MV-LET
  ;;    Similar to :LET (as per FUNCTIONAL-LETLIKE-P), but the call
  ;;    is an MV-CALL.
  ;;    :ASSIGNMENT
  ;;    similar to a LET (as per FUNCTIONAL-SOMEWHAT-LETLIKE-P), but
  ;;    can have other than one call as long as there is at most
  ;;    one non-tail call.
  ;;    :OPTIONAL
  ;;    a lambda that is an entry point for an OPTIONAL-DISPATCH.
  ;;    Similar to NIL, but requires greater caution, since local call
  ;;    analysis may create new references to this function. Also, the
  ;;    function cannot be deleted even if it has *no* references. The
  ;;    OPTIONAL-DISPATCH is in the LAMDBA-OPTIONAL-DISPATCH.
  ;;    :EXTERNAL
  ;;    an external entry point lambda. The function it is an entry
  ;;    for is in the ENTRY-FUN slot.
  ;;    :TOPLEVEL
  ;;    a top level lambda, holding a compiled top level form.
  ;;    Compiled very much like NIL, but provides an indication of
  ;;    top level context. A :TOPLEVEL lambda should have *no*
  ;;    references. Its ENTRY-FUN is a self-pointer.
  ;;    :TOPLEVEL-XEP
  ;;    After a component is compiled, we clobber any top level code
  ;;    references to its non-closure XEPs with dummy FUNCTIONAL
  ;;    structures having this kind. This prevents the retained
  ;;    top level code from holding onto the IR for the code it
  ;;    references.
  ;;    :ESCAPE
  ;;    :CLEANUP
  ;;    special functions used internally by CATCH and UNWIND-PROTECT.
  ;;    These are pretty much like a normal function (NIL), but are
  ;;    treated specially by local call analysis and stuff. Neither
  ;;    kind should ever be given an XEP even though they appear as
  ;;    args to funny functions. An :ESCAPE function is never actually
  ;;    called, and thus doesn't need to have code generated for it.
  ;;    :DELETED
  ;;    This function has been found to be uncallable, and has been
  ;;    marked for deletion.
  ;;    :ZOMBIE
  ;;    Effectless [MV-]LET; has no BIND node.
  ;; TODO: Some of them are not valid semantic in JS, remove them.
  (kind nil :type (member nil :optional :deleted :external :toplevel
                          :escape :cleanup :let :mv-let :assignment
                          :zombie :toplevel-xep))
  ;; KIND was :TOPLEVEL. Now it must be set explicitly, both for
  ;; :TOPLEVEL functions and for any other kind of functions that we
  ;; want to dump or return from #'CL:COMPILE or whatever.
  (has-external-references-p nil)
  ;; In a normal function, this is the external entry point (XEP)
  ;; lambda for this function, if any. Each function that is used
  ;; other than in a local call has an XEP, and all of the
  ;; non-local-call references are replaced with references to the
  ;; XEP.
  ;;
  ;; In an XEP lambda (indicated by the :EXTERNAL kind), this is the
  ;; function that the XEP is an entry-point for. The body contains
  ;; local calls to all the actual entry points in the function. In a
  ;; :TOPLEVEL lambda (which is its own XEP) this is a self-pointer.
  ;;
  ;; With all other kinds, this is null.
  (entry-fun nil :type (or functional null))
  ;; the value of any inline/notinline declaration for a local
  ;; function (or NIL in any case if no inline expansion is available)
  (inlinep nil :type inlinep)
  ;; If we have a lambda that can be used as in inline expansion for
  ;; this function, then this is it. If there is no source-level
  ;; lambda corresponding to this function then this is null (but then
  ;; INLINEP will always be NIL as well.)
  (inline-expansion nil :type list)
  ;; the original function or macro lambda list, or :UNSPECIFIED if
  ;; this is a compiler created function
  (arg-documentation nil :type (or list (member :unspecified)))
  ;; Node, allocating closure for this lambda. May be NIL when we are
  ;; sure that no closure is needed.
  (allocator nil :type (or null combination))
  ;; various rare miscellaneous info that drives code generation & stuff
  (plist () :type list)
  ;; xref information for this functional (only used for functions with an
  ;; XEP)
  (xref () :type list)
  ;; True if this functional was created from an inline expansion. This
  ;; is either T, or the GLOBAL-VAR for which it is an expansion.
  (inline-expanded nil))

;;; The LAMBDA only deals with required lexical arguments. Special,
;;; optional, keyword and rest arguments are handled by transforming
;;; into simpler stuff.
(defstruct (lambda-leaf
	     (:include functional))
  ;; list of LAMBDA-VAR descriptors for arguments
  (vars nil :type list :read-only t)
  ;; If this function was ever a :OPTIONAL function (an entry-point
  ;; for an OPTIONAL-DISPATCH), then this is that OPTIONAL-DISPATCH.
  ;; The optional dispatch will be :DELETED if this function is no
  ;; longer :OPTIONAL.
  (optional-dispatch nil :type (or optional-dispatch null))
  ;; the BIND node for this LAMBDA. This node marks the beginning of
  ;; the lambda, and serves to explicitly represent the lambda binding
  ;; semantics within the flow graph representation. This is null in
  ;; deleted functions, and also in LETs where we deleted the call and
  ;; bind (because there are no variables left), but have not yet
  ;; actually deleted the LAMBDA yet.
  (bind nil :type (or bind null))
  ;; the RETURN node for this LAMBDA, or NIL if it has been
  ;; deleted. This marks the end of the lambda, receiving the result
  ;; of the body. In a LET, the return node is deleted, and the body
  ;; delivers the value to the actual lvar. The return may also be
  ;; deleted if it is unreachable.
  (return nil :type (or return-node null))
  ;; If this CLAMBDA is a LET, then this slot holds the LAMBDA whose
  ;; LETS list we are in, otherwise it is a self-pointer.
  (home nil :type (or lambda-leaf null))
  ;; all the lambdas that have been LET-substituted in this lambda.
  ;; This is only non-null in lambdas that aren't LETs.
  (lets nil :type list)
  ;; all the ENTRY nodes in this function and its LETs, or null in a LET
  (entries nil :type list)
  ;; CLAMBDAs which are locally called by this lambda, and other
  ;; objects (closed-over LAMBDA-VARs and XEPs) which this lambda
  ;; depends on in such a way that DFO shouldn't put them in separate
  ;; components.
  ;; FIXME: We don't implement sparse-set here, remove it or replace it.
  (calls-or-closes (make-sset) :type (or null sset))
  ;; the TAIL-SET that this LAMBDA is in. This is null during creation.
  (tail-set nil :type (or tail-set null))
  ;; list of embedded lambdas
  (children nil :type list)
  (parent nil :type (or lambda-leaf null)))

;;; The OPTIONAL-DISPATCH leaf is used to represent hairy lambdas. It
;;; is a FUNCTIONAL, like LAMBDA. Each legal number of arguments has a
;;; function which is called when that number of arguments is passed.
;;; The function is called with all the arguments actually passed. If
;;; additional arguments are legal, then the LEXPR style MORE-ENTRY
;;; handles them. The value returned by the function is the value
;;; which results from calling the OPTIONAL-DISPATCH.
;;;
;;; The theory is that each entry-point function calls the next entry
;;; point tail-recursively, passing all the arguments passed in and
;;; the default for the argument the entry point is for. The last
;;; entry point calls the real body of the function. In the presence
;;; of SUPPLIED-P args and other hair, things are more complicated. In
;;; general, there is a distinct internal function that takes the
;;; SUPPLIED-P args as parameters. The preceding entry point calls
;;; this function with NIL filled in for the SUPPLIED-P args, while
;;; the current entry point calls it with T in the SUPPLIED-P
;;; positions.
;;;
;;; Note that it is easy to turn a call with a known number of
;;; arguments into a direct call to the appropriate entry-point
;;; function, so functions that are compiled together can avoid doing
;;; the dispatch.
(defstruct (optional-dispatch
	     (:include functional))
  ;; the original parsed argument list, for anyone who cares
  (arglist nil :type list)
  ;; true if &ALLOW-OTHER-KEYS was supplied
  (allowp nil :type boolean)
  ;; true if &KEY was specified (which doesn't necessarily mean that
  ;; there are any &KEY arguments..)
  (keyp nil :type boolean)
  ;; the number of required arguments. This is the smallest legal
  ;; number of arguments.
  (min-args 0 :type unsigned-byte)
  ;; the total number of required and optional arguments. Args at
  ;; positions >= to this are &REST, &KEY or illegal args.
  (max-args 0 :type unsigned-byte)
  ;; list of the (maybe delayed) LAMBDAs which are the entry points
  ;; for non-rest, non-key calls. The entry for MIN-ARGS is first,
  ;; MIN-ARGS+1 second, ... MAX-ARGS last. The last entry-point always
  ;; calls the main entry; in simple cases it may be the main entry.
  (entry-points nil :type list)
  ;; an entry point which takes MAX-ARGS fixed arguments followed by
  ;; an argument context pointer and an argument count. This entry
  ;; point deals with listifying rest args and parsing keywords. This
  ;; is null when extra arguments aren't legal.
  (more-entry nil :type (or lambda-leaf null))
  ;; the main entry-point into the function, which takes all arguments
  ;; including keywords as fixed arguments. The format of the
  ;; arguments must be determined by examining the arglist. This may
  ;; be used by callers that supply at least MAX-ARGS arguments and
  ;; know what they are doing.
  (main-entry nil :type (or lambda-leaf null)))

(defstruct (value-node
	     (:include node))
  (derived-type :undefined :type +js-type+)
  (lvar nil :type (or lvar null)))

;;; A REF represents a reference to a LEAF. REF-REOPTIMIZE is
;;; initially (and forever) NIL, since REFs don't receive any values
;;; and don't have any IR1 optimizer.
(defstruct (ref
	     (:include valued-node (reoptimize nil))
	     (:constructor make-ref
			   (leaf
			    &optional (%source-name '.anonymous.)
			    &aux (leaf-type (leaf-type leaf))
			    (derived-type
			     (make-single-value-type leaf-type)))))
  ;; The leaf referenced.
  (leaf nil :type leaf)
  ;; CONSTANT nodes are always anonymous, since we wish to coalesce named and
  ;; unnamed constants that are equivalent, we need to keep track of the
  ;; reference name for XREF.
  (source-name nil :type (or null symbol) :read-only t))

(defstruct (cond-node
	     (:include node))
  ;; LVAR for the predicate
  (test nil :type (or null lvar))
  ;; the blocks that we execute next in true and false case,
  ;; respectively (may be the same)
  (consequent nil :type (or block-node null))
  (consequent-constraints nil :type (or null t))
  (alternative nil :type (or block-node null))
  (alternative-constraints nil :type (or null t)))

(defstruct (funcall-node
	     (:include node))
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
  ;; conversion and in deleted blocks.
  ;; FIXME: change it into PIECE and change related functions.
  (piece (progn
	   (aver-live-component *current-component*)
	   *current-component*)
	 :type (or piece null))
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

;;; An TAIL-SET structure is used to accumulate information about
;;; tail-recursive local calls. The "tail set" is effectively the
;;; transitive closure of the "is called tail-recursively by"
;;; relation.
;;;
;;; All functions in the same tail set share the same TAIL-SET
;;; structure. Initially each function has its own TAIL-SET, but when
;;; IR1-OPTIMIZE-RETURN notices a tail local call, it joins the tail
;;; sets of the called function and the calling function.
;;;
;;; The tail set is somewhat approximate, because it is too early to
;;; be sure which calls will be tail-recursive. Any call that *might*
;;; end up tail-recursive causes TAIL-SET merging.
(defstruct tail-set
  ;; a list of all the LAMBDAs in this tail set
  (funs nil :type list)
  ;; our current best guess of the type returned by these functions.
  ;; This is the union across all the functions of the return node's
  ;; RESULT-TYPE, excluding local calls.
  (type :undefined :type +js-type+))

;;; FIXME: Move it to the piece module, and remove some unused slots.
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
