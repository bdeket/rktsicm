## rktSICM

a racket port of scmutils (v 20200810 as found [here](http://groups.csail.mit.edu/mac/users/gjs/6946/linux-install.htm))

except for the print/plot/compile utilities everything has been preserved.
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

- [ ] add tests
- [ ] clean up code
- [ ] clean up provides (currently everything is provided)
- [X] turn into a 'lang'
- [ ] make things more 'rackety'
- [ ] use `TR` and the `math` library for number crunching
- [ ] add documentation
- [ ] use logger instead of printing to `current-output-port`