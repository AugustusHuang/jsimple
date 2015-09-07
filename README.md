# lesp
An Ecma-script 6 interpreter written in Common Lisp, still in its infancy!

## Target
The most recent version of ECMA-262 standard shows more possibilities to
regard ECMA-script (or javascript) as a general functional language,
so it becomes more natural to implement a es/js interpreter in functional
languages. With the help of functional IR and optimization,
I hope es/js code will run fast on a great Common Lisp environment such as SBCL.
In addition, sometimes it's really annoying to test a es/js module in a browser,
maybe simulating will be interesting? Or make a standalone executable?

## Usage
Till now only parser will run. Clone the repository into /common-lisp
directory in your home directory. In SBCL use `(require :lesp)` and the system
will be loaded into the environment, then `(lesp-parser:XXX)`. Some of the
new features are not supported now.

