## rktSICM

a racket port of scmutils (v 20230902 as found [here](http://groups.csail.mit.edu/mac/users/gjs/6946/installation.html))
 * Note Re 20230902: As I understand it, the MIT scheme 11.2 introduced an update to `equal?` that made the implementation of scmutils a lot slower, most of the changes made 20230902 are to address this and use a more selective `equal?` where possible to speed things up again.

Original documentation: [&lt;as text&gt;](http://groups.csail.mit.edu/mac/users/gjs/6946/refman.txt) or [&lt;as pdf&gt;](http://groups.csail.mit.edu/mac/users/gjs/6946/refman.pdf)
A start for scribble documentation is made in the [documentation branch](https://github.com/bdeket/rktsicm/tree/documentation), and can be previewed at [bdeket.github.io/rktsicm](https://bdeket.github.io/rktsicm).

Except for the print/plot/compile utilities everything in the main `load-real.scm` hieararchy has been preserved.
Currently everything compiles. More testing will be needed to check that everything works as expected.

#### install:
```
raco pkg install git://github.com/bdeket/rktsicm/?path=rktsicm
```
Installation with documentation can be done with the documentation branch as shown below. However the evaluator for the examples uses a lot of memory and building documentation fails because of this.
```
raco pkg install git://github.com/bdeket/rktsicm/?path=rktsicm#documentation
```


#### use:
```
#lang sicm
```
or `(require sicm)`

#### goals:

- [X] add tests (all tests found in the original files are in [/tests](./rktsicm/sicm/tests)) All test pass (except for 2 long-running that time out). See [testrun.txt](./testrun.txt).
- [X] clean up code. Code is preserved as much as possible to make updating easier.
- [ ] clean up provides (currently everything is provided)
- [X] turn into a 'lang'
- [ ] make things more 'rackety'
- [ ] use `TR` and the `math` library for number crunching
- [ ] add documentation
- [ ] use logger instead of printing to `current-output-port` (partly done => rktsicm-logger)

#### see also
racket libraries with a similar goal
* [soegaard/racket-cas](https://github.com/soegaard/racket-cas)
* [Metaxal/rascas](https://github.com/Metaxal/rascas)

Other scheme implementations of scmutils
* clojure: [sicmutils/sicmutils](https://github.com/sicmutils/sicmutils)
* guile: [guile-scmutils](https://www.cs.rochester.edu/~gildea/guile-scmutils/)