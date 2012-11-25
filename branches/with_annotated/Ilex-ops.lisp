;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#|This line is 75 characters long.---------------------------------------|#
#|iEx5: Linear Regression
  Ilex-ops.lisp
  Date: October 23rd, 2012
  Name: Isaac Sung
|#
(interface Ilex-ops
  (sig mux (xs ys))
  (sig dmx (xys))
  (sig concat (xss))
  (sig blocks (n xs))
  (sig split-at-delimiter (ds xs))
  (sig span (ps xs))
  (sig splitoff-prefix (ps xs))
  (sig split-on-token (tok xs)))