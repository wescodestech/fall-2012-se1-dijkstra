;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
;This line is 75 characters long.---------------------------------------
;tImpl: Stock History Analysis
;  Routput.lisp
;  Run file that runs the whole project.
;
;  Usage: (RunProgram "rqst.txt")
;  Generates HTML files for each analysis request in the request file.
;
;  Date: December 6th, 2012
;  Team Dijkstra
(require "Mlex-ops.lisp")
(require "Mxml-reader.lisp")
(require "Mstocks.lisp")
(require "Mrequests.lisp")
(require "MOutput.lisp")
(require "Mdriver.lisp")
(link Routput
      (import)
      (export Idriver)
      (Mlex-ops Mxml-reader Mstocks Mrequests MOutput Mdriver))
(invoke Routput)