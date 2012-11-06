#|This line is 75 characters long.---------------------------------------|#
#|tImpl: Stock History Analysis 
  Mstocks.lisp
  Reads and parses the stock data file.
  Date: December 6th, 2012
  Team Dijkstra
|#
(require "Istocks.lisp")

(module Mstocks
  
  (export Istocks))

(link Mstocks
      (import)
      (export Istocks)
      (Mstocks))