;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#|This line is 75 characters long.---------------------------------------|#
#|iEx5: Linear Regression
  Icalc.lisp
  Date: October 23rd, 2012
  Name: Isaac Sung
|#
(interface Icalc
  (sig avg (xs))
  (sig vector+scalar (xs s))
  (sig vector*vector (xs ys))
  (sig var (xs)))