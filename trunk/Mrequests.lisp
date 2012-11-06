#|This line is 75 characters long.---------------------------------------|#
#|tImpl: Stock History Analysis 
  Mrequests.lisp
  Request module that reads and parses the request file.
  Date: December 6th, 2012
  Team Dijkstra
|#

(require "Irequests.lisp")

(module Mrequests
  
  (export Irequests))

(link Mrequests
      (import)
      (export Irequests)
      (Mrequests))