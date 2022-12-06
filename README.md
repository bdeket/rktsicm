## rktSICM

a racket port of scmutils (v 20220515 as found [here](http://groups.csail.mit.edu/mac/users/gjs/6946/linux-install.htm))

except for the print/plot/compile utilities everything in the main `load-real.scm` hieararchy has been preserved.
Currently everything compiles. More testing will be needed to check that everything works as expected.

#### install:
```
raco pkg install git://github.com/bdeket/rktsicm/?path=rktsicm
```

#### use:
```
#lang sicm
```
or `(require sicm)`

#### goals:

- [X] add tests (all tests found in the original files are in [/tests](./rktsicm/sicm/tests)) Most test pass except for some (regarding vectorfields and manifolds). See [testrun.txt](./testrun.txt).
- [X] clean up code. Code is preserved as much as possible to make updating easier.
- [ ] clean up provides (currently everything is provided)
- [X] turn into a 'lang'
- [ ] make things more 'rackety'
- [ ] use `TR` and the `math` library for number crunching
- [ ] add documentation
- [ ] use logger instead of printing to `current-output-port` (partly done => rktsicm-logger)