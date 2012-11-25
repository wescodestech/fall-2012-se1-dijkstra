;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#|This line is 75 characters long.---------------------------------------|#
#|iEx5: Linear Regression
  Mlin-regression.lisp
  Date: October 23rd, 2012
  Name: Isaac Sung
|#
(require "Icalc.lisp")
(require "Ilin-regression.lisp")
(require "Ilex-ops.lisp")

(module Mlin-regression-private
  (import Icalc)
  (import Ilex-ops)
  
  (set-state-ok T)
  (include-book "io-utilities" :dir :teachpacks)
  (include-book "list-utilities" :dir :teachpacks)
  (include-book "arithmetic-5/top" :dir :system)
  
  ; Calculates the b value
  (defun calc-b-val (xVar r)
    (/ r xVar))
  
  ; Calculates the a value
  (defun calc-a-val (b xAvg yAvg)
    (- yAvg (* b xAvg)))
  
  ; Calculates the r value: the avg of (xi-xAvg)(yi-yAvg)
  (defun calc-r-val (xs ys xAvg yAvg)
    (let* ((xdev (vector+scalar xs (unary-- xAvg)))
           (ydev (vector+scalar ys (unary-- yAvg)))
           (xdev*ydev (vector*vector xdev ydev)))
      (avg xdev*ydev)))
  
  ; Given two lists of rationals, returns the slope and
  ; y-intercept, and returns them in a pair (intercept, slope)
  (defun calc-slope-and-int (xs ys)
    (let* ((xVar (var xs))
           (xAvg (avg xs))
           (yAvg (avg ys))
           (r (calc-r-val xs ys xAvg yAvg))
           (b (calc-b-val xVar r))
           (a (calc-a-val b xAvg yAvg)))
      (list a b)))
  
  ; Given a string that represents a rational, it trims off all the
  ; 0's at the end of it.
  (defun trim0s (str)
    (let* ((chrs (str->chrs str))
           (trimmed (reverse (cadr (span '(#\0) (reverse chrs))))))
      (chrs->str trimmed)))
  
  ; Given a list of strings, converts it into a list of rationals.
  (defun strlist->ratlist (xs)
    (if (consp xs)
        (cons (str->rat (car xs)) (strlist->ratlist (cdr xs)))
        nil))
  
  ; Given a list of rationals, converts it to a list of strings.
  (defun ratlist->strlist (xs)
    (if (consp xs)
        (cons (trim0s (rat->str (car xs) 99))
              (ratlist->strlist (cdr xs)))
        nil))
  
  ; Converts a string list into a list of x-coords and y-coords
  (defun string->coords (str)
    (let* ((parsed (words str))
           (xsys (dmx parsed))
           (xs (strlist->ratlist (car xsys)))
           (ys (strlist->ratlist (cadr xsys))))
      (list xs ys)))
  
  ; Returns the number of significant figures in a rational number, x.
  (defun sigfigs (x)
    (let* ((str (rat->str x 99))
           (chrs (str->chrs str))
           (trim-- (cadr (span '(#\-) chrs)))
           (trim-0s-1 (cadr (span '(#\0) trim--)))
           (trim-0s (reverse (cadr (span '(#\0) (reverse trim-0s-1)))))
           (sigs (length trim-0s)))
      (if (char-equal #\. (car trim-0s))
          (let ((trimmed (cadr (span '(#\0) (cdr trim-0s)))))
            (length trimmed))
          (if (member-equal #\. trim-0s)
              (- sigs 1)
              sigs))))
  
  ; Returns the max number of sigfigs out of all the values in the
  ; list (xs ys), where xs and ys are lists of rationals.
  (defun max-sigfigs (xsys)
    (if (consp xsys)
        (let ((this (sigfigs (car xsys)))
              (rest (max-sigfigs (cdr xsys))))
          (if (> this rest)
              this
              rest))
        0))
  
  ; Given a rational, x, and a number, m, rounds out x to the max
  ; number of significant figures, m.
  (defun round-sigs (x m)
    (let* ((sigs (sigfigs x)))
      (if (> sigs m)
          (let* ((str (rat->str x 99))
                 (chrs (str->chrs str))
                 (rev (reverse chrs))
                 (trim-0s (cadr (span '(#\0) rev)))
                 (trimmed (cadr (break-at-nth (- sigs m) trim-0s))))
            (str->rat (chrs->str (reverse trimmed))))
          x)))
  
  ; Loads a file containing pairs of coordinates and returns
  ; (xs ys y-intercept slope)
  (defun load-file (filename state)
    (mv-let (msg error-open state)
            (file->string filename state)
            (if error-open
                (mv error-open state)
                (let* ((xsys (string->coords msg))
                       (cat (concatenate 'list (car xsys) (cadr xsys)))
                       (sigfigs (max-sigfigs cat))
                       (ab (calc-slope-and-int (car xsys) (cadr xsys)))
                       (a (round-sigs (car ab) sigfigs))
                       (b (round-sigs (cadr ab) sigfigs)))
                  (list (car xsys) (cadr xsys) a b)))))
   
  ; Takes a list of strings and merges them all into one string
  ; with a space in between.
  (defun merge (strlist)
    (if (consp strlist)
        (cons (string-append (caar strlist)
                             (string-append " " (cadar strlist)))
              (merge (cdr strlist)))
        nil))
  
  ; Writes an output file with the slope, intercept and the data points.
  (defun save-file (filename xs ys a b state)
    (let* ((line1 "Linear Regression Coefficients (slope, intercept)")
           (stra (rat->str a 99))
           (printa (trim0s stra))
           (strb (rat->str b 99))
           (printb (trim0s strb))
           (line2 (string-append printb (string-append " " printa)))
           (line3 "x-y Data")
           (rest (blocks 2 (ratlist->strlist (mux xs ys))))
           (rest-append (merge rest))
           (print-lines (append (list line1 line2 line3) rest-append)))
      (string-list->file filename print-lines state)))
  
  ; Main driver function that reads a file of coordinates and calculates
  ; the linear regression and writes it to a file.
  (defun calc-regression (txt-filename state)
    (let* ((results (load-file txt-filename state))
           (new-fn (string-append txt-filename "-withLRcoeffs.txt")))
      (save-file new-fn (car results) (cadr results) (caddr results)
                 (cadddr results) state)))
  
  ; Takes an input of pairs (x0 y0 x1 y1 ... )
  ; and returns (xs ys y-intercept slope)
  (defun calc-regression2 (xys)
    (let* ((xsys (dmx xys))
           (cat (concatenate 'list (car xsys) (cadr xsys)))
           (sigfigs (max-sigfigs cat))
           (ab (calc-slope-and-int (car xsys) (cadr xsys)))
           (a (round-sigs (car ab) sigfigs))
           (b (round-sigs (cadr ab) sigfigs)))
      (list (car xsys) (cadr xsys) a b)))
  
  (export Ilin-regression))

(link Mlin-regression
      (import Icalc Ilex-ops)
      (export Ilin-regression)
      (Mlin-regression-private))