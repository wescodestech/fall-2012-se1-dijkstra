#|This line is 75 characters long.---------------------------------------|#
#|tImpl: Stock History Analysis 
  Moutput.lisp
  Outputs the data as an HTML file.
  Date: December 6th, 2012
  Team Dijkstra
|#
(require "Ioutput.lisp")

(module Moutput
  
  (export Ioutput))

(link Moutput
      (import)
      (export Ioutput)
      (Moutput))