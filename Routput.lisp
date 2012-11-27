;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#|This line is 75 characters long.---------------------------------------|#
#|tImpl: Stock History Analysis
  Routput.lisp
  Run file that runs the whole project.

  Usage: (generateData stock-filename request-filename state)
  Generates HTML files for each analysis request in the request file.

  Date: December 6th, 2012
  Team Dijkstra
|#
;(require "Mdriver.lisp")
;(require "Mstocks.lisp")
;(require "Mrequests.lisp")
(require "Moutput.lisp")
(link Routput
      (import)
      (export Ioutput)
      (Moutput))
(invoke Routput)
(outputStockData (list ( list (list '(20121115 10) '(20121116 15) '(20121117 50) '(20121118 1)) (list "GOOG" "FB" "MSFT")) ( list (list '(20121112 20) '(20121113 70) '(20121114 40)) (list "AMZN" "ZNGA")))  )