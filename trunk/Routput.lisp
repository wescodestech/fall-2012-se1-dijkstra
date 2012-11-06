#|This line is 75 characters long.---------------------------------------|#
#|tImpl: Stock History Analysis 
  Routput.lisp
  Run file that runs the whole project.

  Usage: (generateData stock-filename request-filename state)
  Generates HTML files for each analysis request in the request file.

  Date: December 6th, 2012
  Team Dijkstra
|#
(require "Mdriver.lisp")
(require "Mstocks.lisp")
(require "Mrequests.lisp")
(require "Moutput.lisp")

(link Routput
      (import)
      (export Ioutput)
      (Mdriver Mstocks Mrequests Moutput))

(invoke Routput)