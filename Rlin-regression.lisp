;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#|This line is 75 characters long.---------------------------------------|#
#|iEx5: Linear Regression
  Rlin-regression.lisp
  
  Usage: (calc-regression filename state)
  Will generate an output file with the data titled,
    filenamewithLRcoeffs.txt.

  Date: October 23rd, 2012
  Name: Isaac Sung
|#
(require "Mlin-regression.lisp")
(require "Mcalc.lisp")
(require "Mlex-ops.lisp")

(link Rlin-regression
      (import)
      (export Ilin-regression)
      (Mcalc Mlex-ops Mlin-regression))

(invoke Rlin-regression)