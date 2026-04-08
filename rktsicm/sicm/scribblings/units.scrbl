#lang scribble/manual

@(require "helpers.rkt"
          scribble/examples
          (for-syntax racket/base sicm/units sicm/units/constants)
          (for-label (only-in racket/base number? symbol? real? boolean?)
                     (except-in racket/math radians->degrees degrees->radians)
                     racket/contract
                     (only-in sicm/generic
                              arity type zero-like one-like zero? one? negate invert sqrt exp sin cos
                              + - / * expt)
                     sicm/units
                     sicm/units/constants
                     )
          sicm/units
          sicm/units/constants
          (only-in sicm/general/eq-properties eq-get)
          (only-in sicm/kernel/express get-property))

@title[]{Units}
@defmodule[sicm/units #:packages ("rktsicm")]

@racket[sicm/units] allows for numbers to be tagged with units. It defines a basic unit system (SI) and alternative units. Conversions and dimension checking are done automatically. The basic form to add and work with units would be:
@examples[#:eval (make-sicm-eval)
          (with-si-units->expression (* (& 3 &kilogram)
                                        (& 4 &k &meter)
                                        (expt (& 3 &deka &second) -2)))]
@;*************************************************************************************************
@section{Unit}
@deftogether[(@defproc[(make-unit [system symbol?] [exponents (vectorof number?)] [scale real?]) units?]
              @defproc[(units? [expr any]) boolean?]
              @defproc[(unit-system [U units?]) symbol?]
              @defproc[(unit-exponents [U units?]) (vectorof number?)]
              @defproc[(unit-scale [U units?]) real?])]{
 Create and acces a new unit, where @racket[system] is the symbol of the linked @racket[unit-system?]. The vector of exponents need to be in the order of the @racket[unit-system?]. You probably don't want to use this directly but instead define the units in @racket[define-unit-system], @racket[define-derived-unit] or @racket[define-additional-unit].

 When the vector of exponents is all-zero (or empty) and the scale is 1 this will return the empty unit.
}

@defproc[(unitless? [expr any]) boolean?]{ Check if something is the empty unit. }
@defproc[(non-unit? [expr any]) boolean?]{ Check if something is not a unit as created with @racket[make-unit]. }

@deftogether[(@defproc[(same-dimensions? [U0 unit?] [U1 units?]) boolean?]
              @defproc[(same-units? [U0 units?] [U1 units?]) boolean?]
              @defproc[(<-units?  [U0 units?] [U1 units?]) boolean?]
              @defproc[(<=-units? [U0 units?] [U1 units?]) boolean?]
              @defproc[(>-units?  [U0 units?] [U1 units?]) boolean?]
              @defproc[(>=-units? [U0 units?] [U1 units?]) boolean?])]{
 @racket[same-dimensions?] means that the exponents are the same.

 @racket[same-units?] means that additionally also the @racket[system] and @racket[scale] are the same.

 For the less- and bigger-than versions the @racket[system] and @racket[exponents] need to be the same and the comparison is done on the @racket[scale].
 Note that if @racket[system] and @racket[exponents] are not @racket[equal?] that these return @racket[#f], whatever the order.}

@;*************************************************************************************************
@section{With-units}

@deftogether[(@defproc[(with-units [value any] [U unit?]) with-unit?]
              @defproc[(with-units? [expr any]) boolean?]
              @defproc[(u:value [W with-unit?]) any]
              @defproc[(u:unit [W with-unit?]) unit?])]{
 Create and access a value with units.}

@defproc[(without-units? [expr any]) boolean?]{ Check if an expression is not a @racket[with-units?] value.}
@defproc[(unitless-quantity? [expr any]) boolean?]{ Check if an expression has the @racket[unitless?] unit.}
@defproc[(has-units? [W with-units?] [U units?]) boolean?]{ Check if @racket[W] has units @racket[U].}
@defproc[(units:= [W0 with-units?] [W1 with-units?]) boolean?]{Check if @racket[W0] and @racket[W1] have the same units. }

@defproc*[([(& [value any] [U units?]) with-units?]
           [(& [value any] [scale real?] [U units?]) with-units?])]{Shorthand for creating values with units.}

@;*************************************************************************************************
@section{System}

@deftogether[(@defform[(define-unit-system id (unit-id unit-name unit-description) ...)]
              @defproc[(unit-system? [expr any]) boolean?]
              @defproc[(base-units [system unit-system?]) (listof (list unit-id unit-name unit-description units?))]
              @defproc[(derived-units [system unit-system?]) (listof (list unit-id unit-name unit-description units?))]
              @defproc[(alternate-units [system unit-system?]) (listof (list unit-id unit-name unit-description units?))])]{
 Define a new unit system with base @racket[unit-id ...].
 The derived and alternate units will be defined in terms of the base and at creation these lists are empty.}

@defform*[((define-derived-unit system unit-id unit-name unit-description)
           (define-derived-unit system unit-id unit-name unit-description unit-scale))]{
 Add a new derived unit to the system. Data can be entered in derived units, and when simplifying the best fit unit will be selected from this list.
}
@defform*[((define-additional-unit system unit-id unit-name unit-description)
           (define-additional-unit system unit-id unit-name unit-description unit-scale))]{
 Add an additional unit to the system. Data can be entered in additional units, but when simplifying these units will be ignored.
}

@defform[(define-multiplier id description exponent)]{
 Define a new multiplier (where the exponent is for base 10).
}

@defform*[((define-constant id abbreviation description the-value a-unit)
           (define-constant id abbreviation description the-value a-unit uncertainty))]{
 Define (and register) a new constant. The registering allows the simplifier to recognise constants and put their symbolic value back into the expression.
}
@defproc[(get-constant-data [name symbol?]) literal-number?]{
 Get the constant with name.
}

@deftogether[(@defproc[(numerical-constants [units? #t boolean?] [constants #f (U #f (listof constants))]) void?]
              @defproc[(symbolic-constants  [units? #t boolean?] [constants #f (U #f (listof constants))]) void?])]{
 @margin-note{Expect... problems. This depends on the @racket[*environment*]s which are not well implemented.}
 Set everything up to either use numerical or symbolic values when using constants in expressions. If units? is #t, units will be added. Otherwise units will be stripped.
 If constants is not provided, all defined constants (with @racket[define-constant]) will be included. Otherwise only the selected list will be set to the new mode.
}



@defproc[(express-in-given-units [W with-units?] [U units?] [u symbol?]) s-expr?]{
 Convert a unit to an expression of the unit U (with u as its symbol place-holder). U must be compatible with the units of W.}
@deftogether[(@defproc[(with-units->expression [system unit-system?] [W with-units?]) s-expr?]
              @defproc[(with-si-units->expression [W with-units?]) s-expr?])]{
 Convert a unit to an expression, using the simplest possible set of base and derived units from the system.}

@deftogether[(@defproc[(express-as [W with-units?] [target symbol?]) s-expr?]
              @defform[(unit-convert s-expr target)])]{
 @margin-note{Expect... problems. To find the target unit, this depends on the @racket[*environment*]s which are not well implemented.}
 Convert a value from one unit to the other. For this to work both units need to use the same base and have the same exponents.
}



@;*************************************************************************************************
@section{Angles}

@defthing[angular units?]{a unit to make explicit that something is to be treated as an angle.}
@deftogether[(@defproc[(degrees->radians [d real?]) real?]
              @defproc[(radians->degrees [d real?]) real?])]{ Conversion from degrees to radians and vice versa.}
@deftogether[(@defproc[(hours->radians [h real?]) real?]
              @defproc[(radians->hours [h real?]) real?])]{ Conversion from hours to radians and vice versa.}

@deftogether[(@defproc[(xms->x [xms (list real? real? real?)]) real?]
              @defproc[(x->xms [x real?]) (list real? real? real?)]
              @defproc[(dms->d [dms (list real? real? real?)]) real?]
              @defproc[(d->dms [d real?]) (list real? real? real?)]
              @defproc[(hms->h [hms (list real? real? real?)]) real?]
              @defproc[(h->hms [h real?]) (list real? real? real?)])]{ Converts the fractional part of @racket[x] (or @racket[d], @racket[h]) to 2 60-based numbers (minutes and seconds) and vice versa.}

@deftogether[(@defproc[(dms->radians [dms (list real? real? real?)]) real?]
              @defproc[(radians->dms [x real?]) (list real? real? real?)]
              @defproc[(hms->radians [hms (list real? real? real?)]) real?]
              @defproc[(radians->hms [x real?]) (list real? real? real?)])]{ Converts radians to degrees witht he fractional part as 2 60-based numbers (minuts and seconds) and vice versa}

@;*************************************************************************************************
@section{Multipliers}
@deftogether[(@defthing[&exa   number? #:value (expt 10  18)]
              @defthing[&peta  number? #:value (expt 10  15)]
              @defthing[&tera  number? #:value (expt 10  12)]
              @defthing[&giga  number? #:value (expt 10   9)]
              @defthing[&mega  number? #:value (expt 10   6)]
              @defthing[&kilo  number? #:value (expt 10   3)]
              @defthing[&hecto number? #:value (expt 10   2)]
              @defthing[&deka  number? #:value (expt 10   1)]
              @defthing[&deci  number? #:value (expt 10  -1)]
              @defthing[&centi number? #:value (expt 10  -2)]
              @defthing[&milli number? #:value (expt 10  -3)]
              @defthing[&micro number? #:value (expt 10  -6)]
              @defthing[&nano  number? #:value (expt 10  -9)]
              @defthing[&pico  number? #:value (expt 10 -12)]
              @defthing[&femto number? #:value (expt 10 -15)]
              @defthing[&E     number? #:value (expt 10  18)]
              @defthing[&P     number? #:value (expt 10  15)]
              @defthing[&T     number? #:value (expt 10  12)]
              @defthing[&G     number? #:value (expt 10   9)]
              @defthing[&M     number? #:value (expt 10   6)]
              @defthing[&k     number? #:value (expt 10   3)]
              @defthing[&h     number? #:value (expt 10   2)]
              @defthing[&da    number? #:value (expt 10   1)]
              @defthing[&d     number? #:value (expt 10  -1)]
              @defthing[&c     number? #:value (expt 10  -2)]
              @defthing[&m     number? #:value (expt 10  -3)]
              @defthing[&u     number? #:value (expt 10  -6)]
              @defthing[&n     number? #:value (expt 10  -9)]
              @defthing[&p     number? #:value (expt 10 -12)]
              @defthing[&f     number? #:value (expt 10 -15)]
              @defthing[&a     number? #:value (expt 10 -18)]
              @defthing[&atto  number? #:value (expt 10 -18)])]{
 Predefined multipliers and their abbreviations that can be used as a scale.}

@;*************************************************************************************************
@section{The SI system}
@defthing[SI unit-system?]{A unit system based on the @hyperlink["https://en.wikipedia.org/wiki/International_System_of_Units"]{International system of units}.}

@subsection{Base units}
@deftogether[(@defthing[&meter unit?]
              @defthing[&kilogram unit?]
              @defthing[&second unit?]
              @defthing[&ampere unit?]
              @defthing[&kelvin unit?]
              @defthing[&mole unit?]
              @defthing[&candela unit?])]

@subsection{Derived units}
These are unit combinations of the base set with a scale of 1.
@deftogether[(@defthing[&radian unit?]
              @defthing[&steradian unit?]
              @defthing[&newton unit?]
              @defthing[&joule unit?]
              @defthing[&coulomb unit?]
              @defthing[&watt unit?]
              @defthing[&volt unit?]
              @defthing[&ohm unit?]
              @defthing[&siemens unit?]
              @defthing[&farad unit?]
              @defthing[&weber unit?]
              @defthing[&henry unit?]
              @defthing[&hertz unit?]
              @defthing[&tesla unit?]
              @defthing[&pascal unit?])]

@subsection{Additional units}
These are unit combinations of the base set with a scale different from 1.
@deftogether[(@defthing[&lumen unit?]
              @defthing[&lux unit?]
              @defthing[&katal unit?]
              @defthing[&becquerel unit?]
              @defthing[&gray unit?]
              @defthing[&sievert unit?]
              @defthing[&degree unit?]
              @defthing[&gram unit?]
              @defthing[&inch unit?]
              @defthing[&centimeter unit?]
              @defthing[&pound unit?]
              @defthing[&slug unit?]
              @defthing[&foot unit?]
              @defthing[&mile unit?]
              @defthing[&dyne unit?]
              @defthing[&calorie unit?]
              @defthing[&minute unit?]
              @defthing[&hour unit?]
              @defthing[&day unit?]
              @defthing[&year unit?]
              @defthing[&sidereal-year unit?]
              @defthing[&AU unit?]
              @defthing[&arcsec unit?]
              @defthing[&pc unit?]
              @defthing[&ly unit?]
              @defthing[&esu unit?]
              @defthing[&ev unit?])]

@;*************************************************************************************************
@section{Constants}
@defmodule[sicm/units/constants #:packages ("rktsicm")]
Not standard loaded with @racket[sicm/units] but is included when @racket[sicm]

@deftogether[(@defthing[:pi    number?]
              @defthing[:2pi   number?]
              @defthing[:pi/2  number?] 
              @defthing[:pi/4  number?] 
              @defthing[:pi/3  number?] 
              @defthing[:pi/6  number?]
              @defthing[:-pi   number?]
              @defthing[:-2pi  number?]
              @defthing[:-pi/2 number?] 
              @defthing[:-pi/4 number?] 
              @defthing[:-pi/3 number?] 
              @defthing[:-pi/6 number?]
              @defthing[:+pi   number?]
              @defthing[:+2pi  number?]
              @defthing[:+pi/2 number?] 
              @defthing[:+pi/4 number?] 
              @defthing[:+pi/3 number?] 
              @defthing[:+pi/6 number?])]{fractions of pi}

@(define-syntax (defcnst stx)
   (syntax-case stx ()
     [(_ id) (syntax @defcnst[id #f])]
     [(_ id link)
      (with-syntax ([val (u:value (eval-syntax #'id))]
                    [unt (caddr (with-si-units->expression (u:units (eval-syntax #'id))))])
        (syntax @defthing[id with-units? #:value val]{The @(get-property (eq-get 'id 'constant) 'description) in @racket[@unt]: @(if link @hyperlink[(string-append "https://en.wikipedia.org/wiki/" link)]{wikipedia} "")}))]))
@(define-syntax (defcunit stx)
   (syntax-case stx ()
     [(_ id)
      (with-syntax ([val (u:value (eval-syntax #'id))]
                    [unt (caddr (with-si-units->expression (u:units (eval-syntax #'id))))])
        (syntax @defthing[id with-units? #:value (& val unt)]))]))

@defthing[:gamma number? #:auto-value]{The Euler-Mascheroni constant: @hyperlink["https://en.wikipedia.org/wiki/Euler's_constant"]{wikipedia}}
@defcnst[:c]{Speed_of_light}
@defcnst[:G]{Gravitational_constant}
@defcnst[:e]{Elementary_charge}
@defcnst[:h]{Planck_constant}
@defcnst[:N_A]{Avogadro_constant}
@defcnst[:m_e]{Electron_mass}
@defcnst[:m_p]{Proton}
@defcnst[:m_n]{Neutron}
@defcnst[:m_u]{Dalton_(unit)}
@defcnst[:mu_e]{Electron_magnetic_moment}
@defcnst[:mu_p]{Nucleon_magnetic_moment}
@defcnst[:gamma_p]{Nucleon_magnetic_moment#Nucleon_gyromagnetic_ratios}
@defcnst[:R_H]{Quantum_Hall_effect#Electrical_resistance_standards}
@defcnst[:R]{Gas_constant}
@defcnst[:k]{Boltzmann_constant}
@defcnst[:h-bar]{Planck_constant#Reduced_Planck_constant}
@defcnst[:F]{Faraday_constant}
@defcnst[:mu_0]{Vacuum_permeability}
@defcnst[:epsilon_0]{Vacuum_permittivity}
@defcnst[:Z0]{Impedance_of_free_space}
@defcnst[:alpha]{Fine-structure_constant}
@defcnst[:R_infinity]{Rydberg_constant}
@defcnst[:r_e]{Classical_electron_radius}
@defcnst[:lambda_C]{Compton_wavelength#Table_of_values}
@defcnst[:a_0]{Bohr_radius}
@defcnst[:Phi_0]{Magnetic_flux_quantum#Superconducting_magnetic_flux_quantum}
@defcnst[:h/2m_e]{List_of_physical_constants}
@defcnst[:e/m_e]{}
@defcnst[:mu_B]{Bohr_magneton}
@defthing[:mu_e/mu_B number? #:auto-value]{The electron magnetic moment ratio.}
@defcnst[:mu_N]{Nuclear_magneton}
@defcnst[:sigma]{Stefan–Boltzmann_law#Stefan–Boltzmann_constant}
@defcnst[:sigma_T]{Thomson_scattering#Description}

@deftogether[(@defcunit[background-temperature]
              @defcunit[water-freezing-temperature]
              @defcunit[room-temperature]
              @defcunit[water-boiling-temperature])]{
 Different standard temperatures.}

@deftogether[(@defcunit[vol@stp]
              @defcunit[sound-speed@stp]
              @defcunit[pressure@stp])]{
 Values at standard pressure
}

@deftogether[(@defcunit[earth-mass]
              @defcunit[earth-radius]
              @defcunit[earth-surface-area]
              @defcunit[earth-mean-density]
              @defcunit[earth-orbital-velocity]
              @defcunit[earth-escape-velocity]
              @defcunit[earth-gravitational-acceleration]
              @defcunit[:g]
              @defcunit[earth-incident-sunlight]
              @defcunit[earth-surface-temperature])]{
 Earth related constants}

@deftogether[(@defcunit[sun-mass]
              @defcunit[:m_sun]
              @defcunit[sun-radius]
              @defcunit[:r_sun]
              @defcunit[sun-luminosity]
              @defcunit[:l_sun]
              @defcunit[sun-rotation-period]
              @defcunit[GMsun]
              @defcunit[sun-surface-temperature])]{
 Sun related constants}

@defcunit[parsec]{}
@defcunit[speed-of-light]{}