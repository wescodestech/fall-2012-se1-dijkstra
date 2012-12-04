;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#|This line is 75 characters long.---------------------------------------|#
#|tImpl: Stock History Analysis 
  Idriver.lisp
  Driver file that runs the program.
  Date: December 6th, 2012
  Team Dijkstra
|#

(interface Idriver
  (sig calcValue (tks srs total_value))
  (sig getStockValues (flattened_tree request))
  (sig mapData (stocks requests))
  (sig RunProgram (RqstFile)))
