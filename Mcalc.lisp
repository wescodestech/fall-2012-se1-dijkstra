;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#|This line is 75 characters long.---------------------------------------|#
#|iEx5: Linear Regression
  Mcalc.lisp
  Date: October 23rd, 2012
  Name: Isaac Sung
|#
(require "Icalc.lisp")

(module Mcalc
  
  (include-book "arithmetic-5/top" :dir :system)
  
  ; Recursive helper function for average. Uses accumulators count, sum.
  (defun avg-helper (xs count sum)
    (if (consp xs)
        (avg-helper (cdr xs) (+ count 1) (+ (car xs) sum))
        (if (= count 0)
            nil
            (/ sum count))))
  
  ; Returns the average of the numbers in the list xs.
  ; xs must be a nonempty list of rational numbers.
  (defun avg (xs)
    (avg-helper xs 0 0))
  
  ; Delivers the list formed by adding the number, s, to each element
  ; of the list, xs (assuming xs is a list of rationals)
  (defun vector+scalar (xs s)
    (if (consp xs)
        (cons (+ s (car xs)) (vector+scalar (cdr xs) s))
        nil))
  
  ; Delivers the list of products corresponding elements from the lists,
  ; xs and ys (assuming xs,ys are lists of numbers and are the same length)
  (defun vector*vector (xs ys)
    (if (and (consp xs) (consp ys))
        (cons (* (car xs) (car ys)) (vector*vector (cdr xs) (cdr ys)))
        nil))
  
  ; Calculates the variance of the values in list, xs
  ; variance = avg of the squared deviations (xi-xAvg)^2
  (defun var (xs)
    (if (consp xs)
        (let* ((xAvg (avg xs))
               (dev (vector+scalar xs (unary-- xAvg)))
               (dev2 (vector*vector dev dev)))
          (avg dev2))
        0))
  
  (export Icalc))