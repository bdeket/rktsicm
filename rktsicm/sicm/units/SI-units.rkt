#lang racket/base

(provide (all-defined-out) &unitless)

(require "system.rkt"
         "units.rkt"
         "with-units.rkt"
         "../kernel-gnrc.rkt" ;using generic + - * etc...
         )
(void 'INSTALL-GENERICS
      (units:assign-operations #t)
      (with-units:assign-operations #t))

;;bdk;; start original file

;;;; SI units

(define-unit-system 'SI
  (list '&meter "m" "length")
  (list '&kilogram "kg" "mass")
  (list '&second "s" "time")
  (list '&ampere "A" "electric current")
  (list '&kelvin "K" "temperature")
  (list '&mole "mol" "amount of substance")
  (list '&candela "cd" "luminous intensity")
  #|
  (list '&radian "rad" "plane angle")
  (list '&steradian "sr" "solid angle")
  |#
  )

(define &angular &unitless)

(define-derived-unit SI '&radian 
  "rad" "plane angle" &angular)

(define-derived-unit SI '&steradian 
  "rad" "solid angle" &angular)


;;; Derived SI units

(define-derived-unit SI '&newton
  "N" "force" (/ (* &kilogram &meter) (square &second)))

(define-derived-unit SI '&joule
  "J" "energy" (* &newton &meter))


(define-derived-unit SI '&coulomb
  "C" "electric charge" (* &ampere &second))


(define-derived-unit SI '&watt
  "W" "power" (/ &joule &second))

(define-derived-unit SI '&volt
  "V" "electric potential" (/ &watt &ampere))

(define-derived-unit SI '&ohm
  "\\Omega" "electric resistance" (/ &volt &ampere))

(define-derived-unit SI '&siemens
  "S" "electric conductance" (/ &unitless &ohm))

(define-derived-unit SI '&farad
  "F" "capacitance" (/ &coulomb &volt))


(define-derived-unit SI '&weber
  "Wb" "magnetic flux" (* &volt &second))

(define-derived-unit SI '&henry
  "H" "inductance" (/ &weber &ampere))

(define-derived-unit SI '&hertz
  "Hz" "frequency" (/ &unitless &second))

(define-derived-unit SI '&tesla
  "T" "magnetic flux density" (/ &weber (square &meter)))

(define-derived-unit SI '&pascal
  "Pa" "pressure" (/ &newton (square &meter)))

;;;; SI multipliers

(define-multiplier '&exa "E" 18)

(define-multiplier '&peta "P" 15)

(define-multiplier '&tera "T" 12)

(define-multiplier '&giga "G" 9)

(define-multiplier '&mega "M" 6)

(define-multiplier '&kilo "k" 3)

(define-multiplier '&hecto "h" 2)

(define-multiplier '&deka "da" 1)

(define-multiplier '&deci "d" -1)

(define-multiplier '&centi "c" -2)

(define-multiplier '&milli "m" -3)

(define-multiplier '&micro "μ" -6)

(define-multiplier '&nano "n" -9)

(define-multiplier '&pico "p" -12)

(define-multiplier '&femto "f" -15)

(define-multiplier '&atto "a" -18)

;;; abbreviations

(define-multiplier '&E "E" 18)

(define-multiplier '&P "P" 15)

(define-multiplier '&T "T" 12)

(define-multiplier '&G "G" 9)

(define-multiplier '&M "M" 6)

(define-multiplier '&k "k" 3)

(define-multiplier '&h "h" 2)

(define-multiplier '&da "da" 1)

(define-multiplier '&d "d" -1)

(define-multiplier '&c "c" -2)

(define-multiplier '&m "m" -3)

(define-multiplier '&u "μ" -6)

(define-multiplier '&n "n" -9)

(define-multiplier '&p "p" -12)

(define-multiplier '&f "f" -15)

(define-multiplier '&a "a" -18)

;;; Other units in terms of SI system

(define-additional-unit SI '&lumen
  "lm" "luminous flux" (* &candela &steradian))

(define-additional-unit SI '&lux
  "lx" "illuminance" (/ &lumen (square &meter)))

(define-additional-unit SI '&katal
  "kat" "catalytic activity" (/ &mole &second))

(define-additional-unit SI '&becquerel
  "Bq" "activity" (/ &unitless &second))

(define-additional-unit SI '&gray
  "Gy" "absorbed dose" (/ &joule &kilogram))

(define-additional-unit SI '&sievert
  "Sv" "dose equivalent" (/ &joule &kilogram))


(define-additional-unit SI '&degree
  "$^\\circ$" "1/360 circle" &angular (/ :2pi 360))

(define-additional-unit SI '&gram
  "gm" "CGS mass" &kilogram 1/1000)

(define-additional-unit SI '&inch
  "in" "English length" &meter (* 2.54 &centi))

(define-additional-unit SI '&centimeter
  "cm" "CGS length" &meter 1/100)

(define-additional-unit SI '&pound
  "lb" "English force" &newton 4.4482)

(define-additional-unit SI '&slug
  "slug" "English mass" &kilogram 14.594)


(define-additional-unit SI '&foot
  "ft" "English length" &inch 12)

(define-additional-unit SI '&mile
  "mi" "English length" &foot 5280)


(define-additional-unit SI '&dyne
  "dyne" "Force" &newton 1.0e-5)

(define-additional-unit SI '&calorie	;at 20 C
  "cal" "Heat energy" &joule 4.1819)

(define-additional-unit SI '&minute
  "day" "Time" &second 60)

(define-additional-unit SI '&hour
  "day" "Time" &second 3600)

(define-additional-unit SI '&day
  "day" "Time" &second 86400)

(define-additional-unit SI '&year
  "yr" "Tropical year 1900"  &second 31556925.9747)

(define-additional-unit SI '&sidereal-year
  "syr" "Sidereal year 1900"  &second 3.1558149984e7)


(define-additional-unit SI '&AU
  "AU" "Astronomical Unit" &meter 149597870700)

(define-additional-unit SI '&arcsec
  "arcsec" "arc second" &radian (/ (* 2 :pi) (* 60 60 360)))


(define parsec
  (/ (& 1 &AU) (tan (& 1 &arcsec))))

(define-additional-unit SI '&pc
  "pc" "Parsec" &meter (u:value parsec))


(define speed-of-light (& 299792458 (/ &meter &second)))

(define-additional-unit SI '&ly
  "ly" "Light Year"
  &meter
  (u:value (* speed-of-light &year)))

(define-additional-unit SI '&esu
  "esu" "Electrostatic Unit"
  &coulomb
  (u:value (/ 1 (* 10 speed-of-light))))

(define-additional-unit SI '&ev
  "ev" "Electron Volt"
  &joule 1.602e-19)
