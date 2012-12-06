;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
(require "Mlex-ops.lisp")
(require "Mxml-reader.lisp")
(require "Mstocks.lisp")
(require "Mrequests.lisp")
(require "Moutput.lisp")
(require "Idriver.lisp")
(require "Mdriver.lisp")

(module Tdriver
  (import Idriver)
  (import IOutput)
  (import Irequests)
  (import Istocks)
  
  
  
  (include-book "testing" :dir :teachpacks)
  (include-book "doublecheck" :dir :teachpacks)
  (include-book "avl-rational-keys" :dir :teachpacks)
  
  (check-expect (isPresent '("AA" "AAPL") "AA") t)
  (check-expect (isPresent '("AA" "AAPL" "GOOG") "YHOO") nil)
  
  (check-expect (calcValue (list "AA" "AAPL" "GOOG") 
                           (list (cons "AA" 12.12) (cons "YHOO" 14.12))
                           0) 12.12)
  (check-expect (calcValue (list "AA" "AAPL" "GOOG") 
                           (list (cons "YHOO" 12.12) (cons "YHOO" 14.12))
                           0) 0)
  
  ; getStockValues check-expects
  ; Searching an empty tree
  (check-expect (getStockValues (avl-flatten (empty-tree)) 
                                '((20121203 20121214 '("AA" "AAPL")))) nil)
  ; Searching for a value that is not present
  (check-expect (getStockValues (avl-flatten 
                                 (avl-insert (empty-tree) 
                                             20121203 '(("AA" 12.32))))
                                '(20121023 20121024 ("AA"))) nil)
  ; Searching for nothing
  (check-expect (getStockValues (avl-flatten 
                                 (avl-insert (empty-tree) 
                                             20121203 '()))
                                '(20121023 20121024 ("AA"))) nil)
  
  ; isPresent-tst
  ; Checks the validity of the isPresent function.  Ensures that a token
  ; is present in the list that is generated.  If that value is cons'd
  ; into the list, it should be present in the list.
  (defproperty isPresent-tst
    (tk :value (random-string)
        tks :value (random-list-of (random-string)))
    (isPresent (cons tk tks) tk))
  
  ; getStockValues-tst
  ; Checks that if a token exists in a set of token that is fed to the AVL
  ; tree, then the getStockValues-tst will not return nil.
  (defproperty getStockValues-tst
    (tk :value (random-string)
        tks :value (random-list-of (random-string))
        date :value (random-rational))
    (implies (isPresent tks tk)
             (getStockValues (avl-flatten (avl-insert (empty-tree)
                                                      date (list tks)))
                             (list date date tks)))))
(link RTdriver
      (Mlex-ops Mxml-reader Mstocks Mrequests MOutput Mdriver Tdriver))
(invoke RTdriver)

;(link Mdriver
;      (import IOutput Irequests Istocks)
;      (export Idriver)
;      (Mdriver-private))