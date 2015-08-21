# Intermediate Representation of jsimple (ljsp)
I decide to design the IR of jsimple base on webassembly. Since webassembly
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

## opcodes
We won't compile the IR into raw assembly codes, so it seems no need to assign
a RISC-like binary code structure to IR.

## Format
The whole IR of a javascript input will be a linear array of form
`(operator operand1 operand2 operand3)`, here the number of arguments vary from
0 to 3.
