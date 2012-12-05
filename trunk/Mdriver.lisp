;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#|This line is 75 characters long.---------------------------------------|#
#|tImpl: Stock History Analysis 
  Mdriver.lisp
  Driver file that runs the program.
  Date: December 6th, 2012
  Team Dijkstra
|#
(require "Idriver.lisp")
(require "Moutput.lisp")
(require "Mrequests.lisp")
(require "Istocks.lisp")

(module Mdriver-private
  (include-book "avl-rational-keys" :dir :teachpacks)
  (include-book "io-utilities" :dir :teachpacks)
  (import IOutput)
  (import Irequests)
  (import Istocks)
  
  ; (isPresent tks sr)
  ; Verifies if the token for the stock record is present in the list of
  ; tks are are used for search.
  ;
  ; tks - a list of tokens to verify agains
  ; sr  - the stock record that houses information for comparison
  (defun isPresent (tks sr)
    (if (equal nil (car tks))
        ; No Token is present for the SR
        nil
        ; Might be a token present
        (if (equal (car tks) sr)
            ; Token does occur
            t
            ; Token does not occur, try next
            (isPresent (cdr tks) sr))))
  
  ; (calcValue tks srs total_value)
  ; Calculates a stocks value, given is occurs on that target date,
  ; 
  ; tks - the tokens that will be used to verify that a SR occurs on that
  ;       date.
  ; srs - the stock records that are present on a target date.
  ; total_value - The total value of the stock records that are verified 
  ;       against tks for the target date.
  (defun calcValue (tks srs total_value)
    (if (equal nil (car srs))
        total_value
        (let* ((sr (car srs))
               (tk (car sr))
               (op (cdr sr)))
          (if (isPresent tks tk)
            (if (rationalp op)
                (calcValue tks (cdr srs) (+ total_value op))
                (calcValue tks (cdr srs) total_value))
            (calcValue tks (cdr srs) total_value)))))
        
  ; (getStockValues flattened_tree request)
  ; Acquires stock values from a request
  (defun getStockValues (flattened_tree request)
    (if (equal nil (car flattened_tree))
        nil
        (let* ((current (car flattened_tree))
               (date (car current))
               (stocks (cdr current))
               (sd (car request))
               (ed (cadr request))
               (tks (caddr request)))
          (if (and (<= sd date) (>= ed date))
              (cons (list date (calcValue tks stocks 0)) 
                    (getStockValues (cdr flattened_tree) request))
              (getStockValues (cdr flattened_tree) request)))))
  
  
  ; (mapData stock requests)
  ; Maps the stock data to the requests and outputs the information to a
  ; format that can be written by the output module.
  ;
  ; stocks - tree stucture for stock data that is available.
  ; requests - the requests for data to be pulled from the stock data.
  (defun mapData (stocks requests)
    (if (equal nil (car requests))
        nil
        (let* ((req (car requests))
               (lin (avl-flatten stocks)))
          (cons (list (getStockValues lin req) (caddr req)) 
                (mapData stocks (cdr requests))))))
  
  (defun RunProgram (RqstFile)
    (outputStockData (mapData (getStockData) (getRequests RqstFile))))
  
  
  (export Idriver))

(link Mdriver
      (import IOutput Irequests Istocks)
      (export Idriver)
      (Mdriver-private))