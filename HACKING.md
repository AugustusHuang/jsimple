Lesp Hacking Guide
===================

This guide is influenced by *SBCL Hacking Guide*.  

Table of Contents
-----------------

	1. Packages
	2. Comments
	3. Functions and variables
	4. Error handling
	5. Optimizing
	6. Test

### Packages
Packages are the primary abstraction and packing level of Common Lisp,
so a good modularization starts from good packaging.
All packages related to Lesp should have prefix `lesp-`, and followed
with some descriptive words, if the package name is too long, you can add
a nickname (which is optional in Common Lisp). If a package will call function
from another non-system-dependent package (e.g. SB-C package in SBCL is a
system-dependent package), add the package into its use list.

### Comments
Comments can be surronded with `#|` and `|#` or started with `;`. If you want
to comment out some maybe-useful code, use `#| blabla |#` like `#if 0`
`#endif` in C. `;` comments have four levels, file-wise comment should start
with `;;;;`, paragraph-wise comment should start with `;;;` (paragraph means
a big form, like a function or a macro), form-wise comment should
start with `;;` and in-line comment `;`. In-line comment style can also be
applied to small pieces of codes which need to be commented out. Comment should
be informative.

### Functions and variables
Function and variables names follow the conventional Common Lisp style.  
Make the function general if it's needed, else make it as specific as possible.
Don't export redundant function in package definition. Never call a portable
or ANSI function with `:`, put the package into current package's use list
instead. If a structure is defined, export its make function, slot reader and
writter (if you defined an interface, export them instead), copier function
and its name, then it will be totally exported and used normally.
Only when necessary, use structure instead of class. Export all useful global
variables (or why you make it global?), but parameters, don't export them
unless it's globally useful.  
NOTE: All suchStyleFunction will be converted into such-style-function, and
SuchStyleFunction will be !such-style-function. Variables will be the same.

### Error handling
If needed, add `assert` and `check-type`. Maybe set a debug output variable
is a good choice, I don't know.

### Optimizing
Use inline function instead of macro.  
Make a function inline if you think it deserves.  
Declare the type of an argument or a local variable if you are sure,
some numerical method may be declared with `speed 3`.

### Test
Test functions and applications should be put in test system, which is optional
asdf-loadable, so there will be a *lesp-test.asd* file in the root directory
of our repo. Tests are needed if you decided to add a new feature or idea
into the main project.
