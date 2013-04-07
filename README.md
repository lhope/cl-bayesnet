# cl-bayesnet - a Common Lisp Bayesian Network Inference Engine #

## Overview ##

cl-bayesnet is a tool for the compilation and probability calculation of discrete, probabilistic [Bayesian Networks](http://en.wikipedia.org/wiki/Bayesian_network).

It provides two types of compilation. The first is join-tree compilation. A join-tree is an auxiliary structure which uses message passing to calculate local probabilities given evidence in a network. Compiling a Bayesian Network to a join tree is quick, but message passing is relatively slow. The join tree's space cost should also be taken into account.

The second type of compilation compiles the Bayesian Network into an Arithmetic Circuit, which is written as a series of arithmetic instructions. These instructions can be interpreted on the fly, or written out as source code for a compiler such as gcc. The compilation process is quite slow, but the instructions are evaluated much faster than message passing (using my implementation, anyway). My tests showed approximately 70x speed up of interpreted instructions over message passing, and then the C compiled instructions executed 20x faster than interpreted instructions!

An additional advantage of the arithmetic circuit is that it compiles to a single, standalone function. That means it is perfect for embedded systems, and can be used anywhere, without dependence on any library at all! An absolutely minimal footprint.

## Scope and Goals ##

The scope of cl-bayesnet is just Bayesian Network compilation and probability calculation. There is no API for building or modifying a Bayesian Network. There are no modelling tools. There is no GUI. These tools already exist, and have done for years.

However, there are not many free, open-source Bayesian Network probability calculators. http://sourceforge.net/projects/bnj/ is the only other open-source one I know of. I know of no other project which can compile direct to machine code (through gcc) like cl-bayesnet can.

The goals of this project is to be useful to developers who want to use the results of their Bayesian Network modelling freely in any domain. If there is demand, compilation to pure Java and compilation to pure Lisp can be quickly developed.

## Highlights ##

- Load from dne, ace, xmlbif formats.
- Simple query API.
- Use message passing or arithmetic circuit methods.
- Embeddable C code generation. Other languages such as Java and Lisp are possible.

## Using cl-bayesnet ##

- Use `make-api-doc.sbcl` in this directory to make the documentation (cl-bayesnet.html). This assumes you have [quicklisp](http://www.quicklisp.org) and [sbcl](http://www.sbcl.org) installed.

- See `tests/test-alarm.lisp` for example usage.

## Ownership and License ##

cl-bayesnet's contributors are listed in the `AUTHORS` file. The authors of cl-bayesnet grant you use of this software under the terms of the Lisp Lesser General Public License (LLGPL). For details see the files `COPYING`, `COPYING.LESSER` and `COPYING.LISP` in this directory.

## Contributing ##

If you find problems with this library please take the time to report the issue. This helps you by increasing the chance the issue will be fixed. It helps motivate me by letting me know the library is being used. It helps everyone when the fixed issue creates a stronger codebase.

To report an issue, use the [cl-bayesnet issue tracker](https://github.com/lhope/cl-bayesnet/issues) at github.com.

For issue fixes and improvements, I prefer pull requests.  Simple one or two liner fixes are okay over email for now.
