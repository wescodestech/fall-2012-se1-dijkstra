#|This line is 75 characters long.---------------------------------------|#
#|tImpl: Stock History Analysis 
  Mstocks.lisp
  Reads and parses the stock data file.
  Date: December 6th, 2012
  Team Dijkstra
|#
;(require "Istocks.lisp")
(include-book "avl-rational-keys" :dir :teachpacks)
(include-book "io-utilities" :dir :teachpacks)
(include-book "Mxml-reader")

(defun getStockData () t)

; (populateTree parent child)
; This function will insert data into the tree where it can locate a
; key-value pair and correctly construct a subtree for that node, given
; the data adheres to the specific structure (td (tk op)) for each
; child value inserted.
;
; ** It is important to note that the TK value will require a hashing
;    mechanic.  (See hash-string function)
;
; parent - The tree upon which the insertion will occur.
; child  - The data structure that will be inserted into the tree in 
;          the form (td (tk op)) where td is the target date, tk is 
;          the token a stock represents and op is the opening price.
(defun populateTree(parent node)
  (if (occurs-in-tree? (car node) parent)
      (let* ((found (avl-retrieve parent (car node))))
        (avl-insert (avl-delete parent (car node)) (car node)
                    (cons (cadr found) (list (cons (cadr node) (caddr node))))))
      (avl-insert parent (first node) (cons (list (cadr node) (caddr node)) nil))))

; Commented out for now to get module functional
#|(module Mstocks
  
  (export Istocks))

(link Mstocks
      (import)
      (export Istocks)
      (Mstocks))|#