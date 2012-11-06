#|This line is 75 characters long.---------------------------------------|#
#|tImpl: Stock History Analysis 
  Irequests.lisp
  Request module that reads and parses the request file.
  Date: December 6th, 2012
  Team Dijkstra
|#

(interface Irequests
  (sig readRequest (filename state))
  (sig getRequests ()))