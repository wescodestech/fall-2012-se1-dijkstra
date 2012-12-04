#|This line is 75 characters long.---------------------------------------|#
#|tImpl: Stock History Analysis 
  Idriver.lisp
  Driver file that runs the program.
  Date: December 6th, 2012
  Team Dijkstra
|#

(interface Idriver
  (sig isDriver (tks sr))
  (sig calcValue (tks srs total_value))
  (sig getStockValues (flattened_tree request))
  (sig mapData (stocks requests)))