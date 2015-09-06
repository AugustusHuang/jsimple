# Intermediate Representation of lesp
I decide to design the IR of lesp base on webassembly. Since webassembly
is a brand new project, this IR will update when needed, but since it lies
between Javascript and Common Lisp, it will be simpler than webassembly, which
is designed to be the lower level of web standard.

## types
Javascript has only two internal numeric types, a 32-bit integer and an
IEEE-754 64-bit double, nowadays nearly every architecture uses IEEE standard,
including Intel and all RISC, and since `most-positive-double-float` in SBCL is
`1.79769e+308`, which equals `DBL_MAX` in float.h, it doesn't cost accuracy
to form a tag. (In SBCL and CMUCL, all lisp objects have tags to represent
their type, so `most-positive-fixnum` is smaller than `INT_MAX`, and the ratio
depends on underlying architecture.) So I choose `double-float` to be the type
of floating numbers and `(integer 2147483647)` to be the type of integers.  
Strings are equivalent in two languages, arrays are equivalent, `true` can
map to `T` and `false` to `nil` (of course they are not equivalent, but it's
an semantic expansion, so no harmful things will be done in js-land. Undefined
type and null type will also point to `nil`, since `(eql (null nil) t)`.  
Objects are different, structure maybe a good style, if we have to implement
a raw assoc-list or hashtable-based structure, it may not be quite efficient.
So it's still open.

## variables
Variables can be relatively local. If it's global, it's context-local. So I
decide to assign every variable a locality level, and to meet with the binding
support of functional language, especially Common Lisp, will extensively use
local `let`, `flet`, `macrolet`, `multiple-value-bind` or anything acts alike.
Variables of different locality level will be assigned different symbol,
maybe this function acts like `gensym`.

## operators
These are the operators I currently feel good with:

    `add-int`
	`add-float`
	`sub-int`
	`sub-float`
	`mul-int`
	`mul-float`
	`div-int`
	`div-float`
	`mod-int`
	`mod-float`
	`inc`
	`dec`
	`le`
	`ge`
	`leq`
	`geq`
	`eq`
	`neq`
	`let`
	`flet`
	`const`
	`set`
	`get`
	`and`
	`or`
	`not`
	`left-shift`
	`right-shift`
	`struct`
	`aref`
	`coerce-float`
	`coerce-string`
	`nop`

Why `inc` and `dec`? They are assumed atomic (maybe, I think), so in
multi-threaded system it won't cause unpredictable result.  
`coerce` have to take part when we are trying to add a number and a string,
since javascript will take the number as another string and return a string.
And if an integer is added with a floating number, result will be a float.  
Still not completed! I don't know whether operators like `*=` or `-=` acts
atomically or not in javascript, or defined by a standard to have such kind
of restrictions.  
`struct` may not be a good low level element, but for now include it here,
OOP is a standard feature in ES 6 now, and the best way to implement it is to
have a structure, with slots properties and pointers to functions.  
`let` and `flet` are introduced because of the new standard encourages uses
of `let` and `const` instead of `var`, since top-level `let` means global,
it seems no need to use `var`.

## Node types (ideas from CMUCL/SBCL IR1)
`node` should be a general wrapper of some elements of the whole program.  
In an AST, leaf node will be something like a local variable declaration, a
function definition, and they will be packed into `leaf-node`.  
Likewise, conditionals, function calls, templates will also have their unique
node type, maybe `cond-node`, `call-node`, `temp-node` or something.  
All fix argument function call or something alike will have their argument
shown in the `node` directly (maybe in a list form), if the argument number is
not determined until runtime, use a different extension of `leaf-node`,
maybe `rest-node`.

## Phases

    1. Make different types of `node` and prepare to generate seperated `piece`.
    2. Traverse the abstract syntax tree and generate node array (or list),
	and then fill them into different pieces (maybe store them in hash-table).
    3. Find linked pieces and use a most-spread algorithm.
    4. Go through multiple passes and delete unused codes, determine variable
	types, adjust entries and exits...

## Detailed design
Traverse from form `(:js XXX)`, depth first, every step either push a new `1`
to the end of the counter list, or increment an existing element of the list
by 1, the counter list will be later stored into the corresponding node, e.g.
a node with counter `(2 3 4)` means it's in the fourth form of the third form
of the second one in the top-level form. If the form begins with
`(:function XXX)`, then make a new function node, if the form begins with
`(:binary XXX)` or `(:unary-prefix XXX)` or `(:const XXX)`,
make a new constant node, if the form begins with `(:let XXX)` or `(:var XXX)`
make a new variable node, if the form begins with `(:assign XXX)`, make
a new set node (here we don't tell the differences between SETF and SETQ),
if the form begins with `(:for XXX)`, `(:for-in XXX)` or `(:for-of XXX)`,
make a new loop node. If the form begins with `(:block XXX)`, make a new block
node.
