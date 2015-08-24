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

## opcodes
We won't compile the IR into raw assembly codes, so it seems no need to assign
a RISC-like binary code structure to IR.

## Format
The whole IR of a javascript input will be a list of form
`(operator operand1 operand2 operand3)`, here the number of arguments vary from
0 to 3. Every form should be called a **node**. A node structure will be made
(I borrow this name from **CMUCL**).

## Virtual machine
The IR will be assumed to run on a Lisp virtual machine. It has a linear memory
and the unit objects will be nodes.

## Idea scratch pad
Do we need high level IR forms like `loop` or `for`? But then the number of
arguments may change, since `for` may need a container, a step, a scope and
also a form to apply something, even more if we are facing a complex one.  
When to do type inference? In the first pass we won't know a lot, since we
only have the local information about a variable (Really?).  
How to represent different data types, like class or function, use es standard
way or use Lisp-ish way? I prefer the later, so a class with a constructor
will become a structure, and a function will be a function noted by its number
of arguments.
