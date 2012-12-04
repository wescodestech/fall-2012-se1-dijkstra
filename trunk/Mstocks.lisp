;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#|This line is 75 characters long.---------------------------------------|#
#|tImpl: Stock History Analysis 
  Mstocks.lisp
  Reads and parses the stock data file.
  Date: December 6th, 2012
  Team Dijkstra
|#
(require "Istocks.lisp")
(require "Ixml-reader.lisp")

(module Mstocks-public
  (import Ixml-reader)
  
  (include-book "avl-rational-keys" :dir :teachpacks)
  (include-book "io-utilities" :dir :teachpacks)

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
        (avl-insert parent (first node) 
                  (cons (list (cadr node) (caddr node)) nil))))
  
  ; (readStockData xml)
  ; This function will read the XML string for sotck record data and 
  ; parse it as well as insert it into the tree structure that houses
  ; the information for searching.
  ;
  ; xml - The XML string that is read from the hist.txt file.
  (defun readStockData (xml)
    (if (equal nil xml)
        nil
        (let* ((sr (extractXMLTag xml "sr"))
               (tk (extractXMLTag (coerce (car sr) 'string) "tk"))
               (td (extractXMLTag (cdr tk) "td"))
               (hp (extractXMLTag (cdr td) "hp"))
               (lp (extractXMLTag (cdr hp) "lp"))
               (op (extractXMLTag (cdr lp) "op"))
               (cp (extractXMLTag (cdr op) "cp"))
               (tr (extractXMLTag (cdr cp) "tr")))
          (populateTree (readStockData (cdr sr)) 
                        (list (str->rat (coerce (car td) 'string)) 
                              (coerce (car tk) 'string) 
                              (str->rat (coerce (car cp) 'string)))))))
  
  ; (getStockData)
  ; Acquires the stock data in tree formate where TD is the key and 
  ; (TK OP) are values housed as a list in the value of the node.
  ;
  ; This function takes no arguments as the hist.txt is a static input
  ; which is updated externally.  "short_hist.txt" is used as a shorter
  ; notation for parsing purposes.
  (defun getStockData () 
    (mv-let (input-text error-open state)
            (file->string "hist.txt" state)
            (readStockData input-text)))
  
  (export Istocks))

(link Mstocks
      (import Ixml-reader)
      (export Istocks)
      (Mstocks-public))