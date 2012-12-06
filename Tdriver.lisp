
(require "Mlex-ops.lisp")
(require "Mxml-reader.lisp")
(require "Mstocks.lisp")
(require "Mrequests.lisp")
(require "Moutput.lisp")
(require "Idriver.lisp")
(require "Mdriver.lisp")

(module Tdriver
  (import Idriver)
  (import IOutput)
  (import Irequests)
  (import Istocks)
  
  
  
  (include-book "testing" :dir :teachpacks)
  (include-book "doublecheck" :dir :teachpacks)
  
  (check-expect (isPresent '("AA" "AAPL") "AA") t)
  (check-expect (isPresent '("AA" "AAPL" "GOOG") "YHOO") nil)
  
  (check-expect (calcValue (list "AA" "AAPL" "GOOG") 
                           (list (cons "AA" 12.12) (cons "YHOO" 14.12))
                           0) 12.12)
  (check-expect (calcValue (list "AA" "AAPL" "GOOG") 
                           (list (cons "YHOO" 12.12) (cons "YHOO" 14.12))
                           0) 0)
  
  
  (defproperty calcValueratP
    (tks :value (list "AA" "AAPL" "GOOG") 
     srs :value (list (cons "AA" 12.12) (cons "YHOO" 14.12)))
    (rationalp (calcValue tks srs 0)))
  
  
  
  
  )
(link RTdriver

      (Mlex-ops Mxml-reader Mstocks Mrequests MOutput Mdriver Tdriver))
(invoke RTdriver)

;(link Mdriver
;      (import IOutput Irequests Istocks)
;      (export Idriver)
;      (Mdriver-private))